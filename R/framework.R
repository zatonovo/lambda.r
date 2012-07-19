require(parser)

# f(a,b) %::% A : B : C
'%::%' <- function(signature, types)
{
  s.expr <- paste(deparse(substitute(signature)), collapse="\n")
  t.expr <- paste(deparse(substitute(types)), collapse="\n")
  text <- paste(s.expr,t.expr, sep=" %::% ")
  #cat("Full expression:",text,"\n")
  expr <- parser(text=text)
  raw <- attr(expr,"data")

  it <- iterator(raw)
  tree <- list()
  tree$name <- get_name(it)
  tree$args <- get_args(it)
  tree$types <- get_types(it, tree$args, t.expr)

  add_variant(tree)
  invisible()
}


# f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + b; z * 2 }
# f(a,b) %when% { a < 5 } %as% { a + b }
# f(a,b) %as% { a + b }
'%as%' <- function(signature, body)
{
  s.expr <- paste(deparse(substitute(signature)), collapse="\n")
  b.expr <- paste(deparse(substitute(body)), collapse="\n")
  text <- paste(s.expr,b.expr, sep=" %as% ")
  #cat("Full expression:",text,"\n")
  expr <- parser(text=text)
  raw <- attr(expr,"data")

  it <- iterator(raw)
  tree <- list()
  tree$name <- get_name(it)
  tree$args <- get_args(it)
  guard_expr <- get_guard(it)
  tree$guard <- guard_fn(tree$args, guard_expr)
  tree$def <- get_definition(tree$args, b.expr)

  add_variant(tree)
  invisible()
}

################################## RUN TIME ###################################
.ERR_NO_MATCH <- "No match for function %s/%s: %s"
.ERR_USE_FUNCTION <- "No valid function for '%s/%s: %s'"
.ERR_ENSURE_FAILED <- "Assertion '%s' failed for args = %s and result = %s"
NewObject <- function(type.name, ...)
{
  result <- UseFunction(type.name, ...)
  type <- gsub('"','', type.name)
  class(result) <- c(type, class(result))
  result
}

UseFunction <- function(fn.name, ...)
{
  result <- NULL
  #cat("[UseFunction] Getting function",fn.name,"\n")
  fn <- get(fn.name)
  #cat("[UseFunction] Getting guards for",fn.name,"\n")
  raw.args <- list(...)
  vs <- get_variant(fn,length(raw.args))
  if (is.null(vs))
    stop(sprintf(.ERR_NO_MATCH,fn.name,length(raw.args),as_simple(raw.args)))
  matched.fn <- NULL
  for (v in vs)
  {
    if (!check.types(v$types, raw.args)) next
    if (is.null(v$guard)) { matched.fn <- v$def; break }
    if (do.call(v$guard, raw.args)) { matched.fn <- v$def; break }
  }
  if (is.null(matched.fn))
    stop(sprintf(.ERR_USE_FUNCTION,fn.name,length(raw.args),as_simple(raw.args)))

  result <- do.call(matched.fn, list(...))

  # TODO: Add assertions

  result
}

check.types <- function(raw.types, raw.args)
{
  if (is.null(raw.types)) return(TRUE)
  if (nrow(raw.types) - 1 != length(raw.args)) return(FALSE)
  arg.types <- sapply(raw.args, function(x) class(x))
  all(raw.types$text[1:length(raw.args)] %in% arg.types)
}

.SIMPLE_TYPES <- c('numeric','character','POSIXt','POSIXct')
.is.simple <- function(x) any(class(x) %in% .SIMPLE_TYPES)
as_simple <- function(x)
{
  if (! .is.simple(x)) return(class(x)[1])
  if (length(x) == 1) return(x)
  if (length(x) < 5) sprintf("c(%s)", paste(x, collapse=','))
  else sprintf("c(%s, ...)", paste(x[1:4], collapse=','))
}

################################# PARSE TIME #################################
iterator <- function(tree)
{
  cap <- nrow(tree) + 1
  idx <- 0
  function()
  {
    idx <<- idx + 1
    if (idx < cap) tree[idx,]
    else NA
  }
}

get_name <- function(it)
{
  line <- it()
  if (line$token.desc != 'SYMBOL_FUNCTION_CALL')
    stop("Function must start with a symbol")
  line$text
}

get_args <- function(it)
{
  args <- NULL
  line <- it()
  if (line$token.desc != "'('")
    stop("Function missing opening parenthesis")

  # Get arguments (position, name)
  while (!is.na(line <- it()) && line$token.desc != "')'")
  {
    if (line$token.desc %in% c('SYMBOL_FUNCTION_CALL',"'('"))
      stop("Invalid symbol '",line$text,"'in function definition")
    if (line$token.desc %in% c('expr',"','")) next
    args <- rbind(args, line)
  }
  args[,c('line1','token.desc','text')]
}

get_guard <- function(it)
{
  guards <- NULL
  while (!is.na(line <- it()) && line$token.desc != "SPECIAL") next
  if (line$text == '%when%')
  {
    line <- it()
    if (line$token.desc != "'{'")
      stop("Guard missing opening block")
    while (!is.na(line <- it()) && line$token.desc != "'}'")
    {
      if (line$token.desc %in% c("'{'"))
        stop("Invalid symbol '",line$text,"'in function definition")
      if (line$token.desc %in% c('expr',"','")) next
      guards <- rbind(guards, line)
    }
    while (!is.na(line <- it()) && line$token.desc != "SPECIAL") next
  }
  guards[,c('line1','token.desc','text')]
}

guard_fn <- function(raw.args, tree)
{
  lines <- NULL
  args <- apply(raw.args,1, arguer())
  # Add any pattern matches
  mismatch <- args != raw.args$text
  if (any(mismatch))
    lines <- paste(args[mismatch],'==',raw.args[mismatch,'text'], sep=' ')

  # Add explicit guards
  if (!is.null(tree))
  {
    f <- function(x) paste(tree[tree$line1 %in% x,]$text, collapse=' ')
    index <- array(unique(tree$line1))
    lines <- c(lines,apply(index,1,f))
  }

  if (length(lines) < 1) return(NULL)

  body <- paste(lines, collapse=' & ')
  arg.string <- paste(args, collapse=',')
  fn.string <- sprintf(".fn <- function(%s) { %s }", arg.string, body)
  eval(parse(text=fn.string))
  .fn
}

is.type <- function(fn.string)
{
  length(grep('^[A-Z]', fn.string)) > 0
}

# Get argument names
arguer <- function()
{
  idx <- 0
  function(x)
  {
    if (x['token.desc'] == 'SYMBOL') x['text']
    else { idx <<- idx + 1; paste('.z',idx,sep='') }
  }
}
get_definition <- function(args, body)
{
  # TODO: Check for side effects
  args <- paste(apply(args,1, arguer()), collapse=',')
  fn.string <- paste(".fn <- function(",args,") ",body, collapse="")
  eval(parse(text=fn.string))
  .fn
}

get_types <- function(it, args, expr)
{
  types <- NULL
  while (!is.na(line <- it()) && line$token.desc != "SPECIAL") next
  if (line$text == '%::%')
  {
    while (!is.na(line <- it()) && TRUE)
    {
      if (line$token.desc %in% c("'{'", "'}'", "'('", "')'"))
        stop("Invalid symbol '",line$text,"'in function definition")
      if (line$token.desc != "SYMBOL") next
      types <- rbind(types, line)
    }
  }
  if (nrow(args) != nrow(types) - 1)
    stop("Incorrect number of parameters in type declaration")

  types[,c('line1','token.desc','text')]
}

add_variant <- function(tree)
{
  # We use 2 because this is called from within the 'guard' function so the
  # stack is two down
  where <- topenv(parent.frame(2))
  .setup_parent(tree$name, where)
  fn <- get(tree$name, where)
  variants <- attr(fn,'variants')

  num.args <- nrow(tree$args)
  if (length(variants) < num.args) arg.list <- list()
  else arg.list <- variants[[num.args]]
  tree.name <- paste(tree$args$text, collapse=',')

  if (is.null(arg.list[[tree.name]]))
  {
    arg.list[[tree.name]] <- tree
  }
  else
  {
    if (! is.null(tree$types))
      arg.list[[tree.name]]$types <- tree$types
    else 
    {
      arg.list[[tree.name]]$guard <- tree$guard
      arg.list[[tree.name]]$def <- tree$def
    }
  }

  variants[[num.args]] <- arg.list
  attr(fn,'variants') <- variants

  assign(tree$name, fn, where)
  invisible()
}

# TODO: Add support for ellipsis arguments
get_variant <- function(fn, arg.length)
{
  raw <- attr(fn,'variants')
  raw[[arg.length]]
}


.setup_parent <- function(parent, where)
{
  # Overwrite a final definition (as opposed to appending)
  if (exists(parent, where))
  {
    parent.def <- get(parent, where)
    is.final <- attr(parent.def, 'final')
    if (!is.null(is.final) && is.final == TRUE)
    {
      attributes(fn) <- NULL
      attr(parent.def, 'variants') <- list()
      assign(parent, parent.def, where)
    }
  }
  else
  {
    if (is.type(parent)) 
      pattern <- 'function(...) NewObject(\'%s\',...)'
    else
      pattern <- 'function(...) UseFunction(\'%s\',...)'
    parent.def <- eval(parse(text=sprintf(pattern,parent)))
    attr(parent.def, 'variants') <- list()
    #cat("Adding parent function",parent.def,"to",where,"\n")
    assign(parent, parent.def, where)
    #msg <- "Function %s has no visible parent function '%s'"
    #stop(sprintf(msg, child, parent))
  }
}

