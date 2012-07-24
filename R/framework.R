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
  name <- get_name(it)
  tree <- list()
  tree$args <- get_args(it)
  tree$types <- get_types(it, tree$args, t.expr)

  add_variant(name, tree)
  invisible()
}


# f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + 2; z * 2 }
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
  name <- get_name(it)
  tree <- list()
  args_expr <- get_args(it)
  tree$args <- remove_defaults(args_expr)
  guard_expr <- get_guard(it)
  guard_expr <- transform_attrs(guard_expr)
  tree$guard <- guard_fn(tree$args, guard_expr)

  body_expr <- get_body(it)
  body_expr <- transform_attrs(body_expr)
  tree$def <- body_fn(tree$args, body_expr)

  add_variant(name, tree)
  create_defaults(tree, args_expr)
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
  if (is.null(vs) || length(vs) < 1)
    stop(sprintf(.ERR_NO_MATCH,fn.name,length(raw.args),as_simple(raw.args)))
  matched.fn <- NULL
  for (v in vs)
  {
    full.args <- fill_args(raw.args, v)
    # TODO: Update type structure (allow only one type declaration per unique
    # count of arguments)
    if (!check_types(v$types, full.args)) next
    if (is.null(v$guard)) { matched.fn <- v$def; break }
    if (do.call(v$guard, full.args)) { matched.fn <- v$def; break }
  }
  if (is.null(matched.fn))
    stop(sprintf(.ERR_USE_FUNCTION,fn.name,length(raw.args),as_simple(raw.args)))

  result <- do.call(matched.fn, full.args)

  # TODO: Add assertions

  result
}

# TODO: Add support for ellipsis arguments
fill_args <- function(raw.args, tree)
{
  default <- tree$args$default
  if (length(raw.args) == length(default)) return(raw.args)

  # This is for unnamed arguments
  if (is.null(names(raw.args)))
    c(raw.args, default[(length(raw.args)+1):length(default)] )
  else
  {
    names(default) <- tree$args$text
    shim <- tree$args$text[1:length(raw.args)]
    names(raw.args)[names(raw.args) == ""] <- shim[names(raw.args) == '']
    default[names(raw.args)] <- raw.args
    default
  }
}

check_types <- function(raw.types, raw.args)
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
  function(rewind=FALSE)
  {
    if (rewind) idx <<- idx - 1
    else idx <<- idx + 1
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
  args <- data.frame(stringsAsFactors=FALSE)
  line <- it()
  if (line$token.desc != "'('")
    stop("Function missing opening parenthesis")

  # Get arguments (position, name)
  defaults <- NULL
  default <- NULL
  fn <- function(v, vs)
  {
    if (is.null(v)) v <- NA
    else if (length(v) == 1 && v %in% c(",", ")") )
      v <- NULL
    else if (v[length(v)] %in% c(",", ")") )
      v <- v[1:(length(v)-1)]

    if (!is.null(v) && is.na(v))
      vs <- c(vs, v)
    else if (!is.null(v))
      vs <- c(vs, paste(v,collapse=' '))
    vs
  }

  while (!is.na(line <- it()) && line$token.desc != "SPECIAL")
  {
    #if (line$token.desc %in% c('SYMBOL_FUNCTION_CALL',"'('"))
    #  stop("Invalid symbol '",line$text,"'in function definition")
    if (line$token.desc %in% c('expr','EQ_SUB')) next
    if (line$token.desc %in% c('SYMBOL','SYMBOL_SUB'))
    {
      defaults <- fn(default,defaults)
      default <- NULL
      args <- rbind(args, line)
    }
    else
    {
      if (line$token.desc == 'STR_CONST')
        line$text <- sub('^[\'"]([^\'"]+)[\'"]$', '\\1', line$text)
      default <- c(default, line$text)
    }
  }
  if (length(default) > 1)
  {
    if (default[length(default)] %in% c(",", ")") )
      default <- default[1:(length(default)-1)]
    defaults <- c(defaults, default)
  }
  it(rewind=TRUE)
  out <- args[,c('line1','token.desc','text')]
  out$default=defaults
  out
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
    #while (!is.na(line <- it()) && line$token.desc != "SPECIAL") next
  }
  else
    it(rewind=TRUE)
  guards[,c('line1','token.desc','text')]
}

guard_fn <- function(raw.args, tree)
{
  lines <- NULL
  args <- apply(raw.args,1, arg.names())
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

# A parse transform to change object@attribute to attr(object,'attribute')
# f(x) %when% { x@name == "bob" } %as% x
transform_attrs <- function(tree)
{
  start <- grep("'@'", tree$token.desc, value=FALSE) - 1
  stop <- grep("SLOT", tree$token.desc, value=FALSE)
  if (length(start) < 1) return(tree)

  template <- data.frame(line1=0,
    token.desc=c('SYMBOL_FUNCTION_CALL',"'('",'SYMBOL',"','",'STR_CONST',"')'"),
    text=c('attr','(', 'object', ',', '"key"',')'),
    stringsAsFactors=FALSE)
  rep.fn <- function(idx,o,k)
  {
    template$line1 <- idx
    template$text[3] <- o
    template$text[5] <- paste('"',k,'"', sep='')
    template
  }

  positions <- data.frame(cbind(start,stop), stringsAsFactors=FALSE)
  cut.fn <- function(idx)
  {
    ls <- NULL
    if (idx == 1) inf <- 1
    else inf <- positions$stop[idx - 1] + 1
    sup <- positions$start[idx] - 1
    if (sup > 0) ls <- rbind(ls, tree[inf:sup,])

    i <- tree[positions$start[idx],]$line1
    o <- tree[positions$start[idx],]$text
    k <- tree[positions$stop[idx],]$text
    ls <- rbind(ls, rep.fn(i,o,k))

    if (idx == nrow(positions)) {
      ls <- rbind(ls, tree[(positions$stop[idx] + 1) : nrow(tree),] )
    }
    ls
  }
  lines <- apply(array(1:nrow(positions)),1,cut.fn)
  do.call(rbind, lines)
}

is.type <- function(fn.string)
{
  length(grep('^[A-Z]', fn.string)) > 0
}

# Get argument names
arg.names <- function()
{
  idx <- 0
  function(x)
  {
    if (x['token.desc'] == 'SYMBOL') x['text']
    else { idx <<- idx + 1; paste('.z',idx,sep='') }
  }
}

# Obsolete
get_definition <- function(args, body)
{
  # TODO: Check for side effects
  args <- paste(apply(args,1, arg.names()), collapse=',')
  fn.string <- paste(".fn <- function(",args,") ",body, collapse="")
  eval(parse(text=fn.string))
  .fn
}

get_body <- function(it)
{
  body <- NULL
  while (!is.na(line <- it()) && line$token.desc != "SPECIAL") next
  if (line$text == '%as%')
  {
    needs.wrapping <- FALSE
    while (!is.na(line <- it()) && TRUE)
    {
      if (line$token.desc %in% c('expr')) next
      body <- rbind(body, line)
    }
  }
  else
    it(rewind=TRUE)
  body[,c('line1','token.desc','text')]
}

open_brace <- function()
{
  data.frame(line1=0,col1=0,byte1=0, line2=0,col2=1,byte2=1, token=123,id=13,
    parent=30,top_level=0, token.desc="'{'", terminal=TRUE, text='{')
}

close_brace <- function()
{
  data.frame(line1=0,col1=0,byte1=0, line2=0,col2=1,byte2=1, token=125,id=15,
    parent=30,top_level=0, token.desc="'}'", terminal=TRUE, text='}')
}


body_fn <- function(raw.args, tree)
{
  if (tree$token.desc[1] == "'{'") tree <- tree[2:(nrow(tree)-1), ]
  lines <- NULL
  args <- apply(raw.args,1, arg.names())

  if (!is.null(tree))
  {
    f <- function(x) paste(tree[tree$line1 %in% x,]$text, collapse=' ')
    index <- array(unique(tree$line1))
    lines <- apply(index,1,f)
  }

  if (length(lines) < 1) return(NULL)

  body <- paste(lines, collapse='\n')
  arg.string <- paste(args, collapse=',')
  fn.string <- sprintf(".fn <- function(%s) { %s }", arg.string, body)
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

# from.pattern - if TRUE then insert at the end of the patterns but before
# variables. This way type declarations don't supersede patterns.
# variants[[idx]] -> list(num.args=c(), args=data.frame(),
#   guard=character(), def=character())
add_variant <- function(fn.name, tree, from.pattern=FALSE)
{
  # We use 2 because this is called from within the 'guard' function so the
  # stack is two down
  where <- topenv(parent.frame(2))
  setup_parent(fn.name, where)
  fn <- get(fn.name, where)
  variants <- attr(fn,'variants')

  args <- tree$args
  tree$accepts <- length(args$default[is.na(args$default)]) : nrow(args)
  # TODO: Support ellipsis as Inf
  if (from.pattern)
    variants <- c(list(tree), variants)
  else
    variants[[length(variants) + 1]] <- tree
  attr(fn,'variants') <- variants

  assign(fn.name, fn, where)
  invisible()
}

get_variant <- function(fn, arg.length)
{
  raw <- attr(fn,'variants')
  match.fn <- function(x)
    any(arg.length >= x$accepts & arg.length <= x$accepts)
  matches <- sapply(raw, match.fn)
  raw[matches]
}


setup_parent <- function(parent, where)
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

# Remove default values from parse tree
remove_defaults <- function(args_expr)
{
  args <- args_expr[args_expr$token.desc %in% c('SYMBOL','SYMBOL_SUB'), ]
  args$token.desc <- 'SYMBOL'
  args
}

# Create new functions that use default values
create_defaults <- function(tree, args_expr)
{
  num.symbols <- length(args_expr$token.desc[args_expr$token.desc == 'SYMBOL'])
  options <- NULL
  it = iterator(args_expr)
  key <- NULL
  default <- NULL
  while (!is.na(line <- it()) && TRUE)
  {
    if (line$token.desc %in% c('SYMBOL','EQ_SUB')) next
    if (line$token.desc == 'SYMBOL_SUB')
    {
      if (!is.null(key) && !is.null(default))
        options <- rbind(options, c(key, paste(default,collapse='')))
      key <- line$text
      default <- NULL
    }
    else
      default <- c(default, line$text)
  }
  options <- rbind(options, c(key, paste(default,collapse='')))
  options <- data.frame(options, stringsAsFactors=FALSE)
  options
}
