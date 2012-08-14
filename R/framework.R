require(parser)

# f(a,b) %::% A : B : C
'%::%' <- function(signature, types)
{
  s.expr <- paste(deparse(substitute(signature)), collapse="\n")
  t.expr <- paste(deparse(substitute(types)), collapse="\n")
  text <- paste(s.expr,t.expr, sep=" %::% ")
  expr <- parser(text=text)
  raw <- attr(expr,"data")

  it <- iterator(raw)
  tree <- list()
  args_expr <- parse_fun(it)
  name <- args_expr$token[1]
  tree$args <- args_expr[2:nrow(args_expr),]
  tree$types <- parse_types(it, tree$args, t.expr)
  tree$signature <- paste(s.expr,"%::%",t.expr, sep=' ')

  add_type(name, tree)
  invisible()
}


# f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + 2; z * 2 }
# f(a,b) %when% { a < 0 } %as% { abs(a) + b }
# f(a,b) %as% { a + b }
'%as%' <- function(signature, body)
{
  s.expr <- paste(deparse(substitute(signature)), collapse="\n")
  b.expr <- paste(deparse(substitute(body)), collapse="\n")
  text <- paste(s.expr,b.expr, sep=" %as% ")
  expr <- parser(text=text)
  raw <- attr(expr,"data")
  it <- iterator(raw)

  tree <- list()
  args_expr <- parse_fun(it)
  name <- args_expr$token[1]
  tree$args <- args_expr[2:nrow(args_expr),]
  guard_expr <- parse_guard(it)
  guard_expr <- transform_attrs(guard_expr)
  tree$guard <- guard_fn(tree$args, guard_expr)

  body_expr <- parse_body(it)
  body_expr <- transform_attrs(body_expr)
  tree$def <- body_fn(tree$args, body_expr)
  tree$signature <- s.expr
  tree$body <- b.expr

  add_variant(name, tree)
  invisible()
}

################################## RUN TIME ###################################
.ERR_NO_MATCH <- "No match for function"
.ERR_USE_FUNCTION <- "No valid function for"
.ERR_ENSURE_FAILED <- "Assertion '%s' failed for args = %s and result = %s"
NewObject <- function(type.name, ...)
{
  result <- UseFunction(type.name, ...)
  type <- gsub('"','', type.name)
  if (!type %in% class(result))
    class(result) <- c(type, class(result))
  result
}

NewObject.old <- function(type, ...)
{
  result <- UseFunction(type, ...)
  type.name <- deparse(substitute(type))
  type.name <- gsub('"','', type.name)
  if (!type.name %in% class(result))
    class(result) <- c(type.name, class(result))
  result
}

UseFunction <- function(fn.name, ...)
{
  #cat("Objects visible in UseFunction:\n")
  #print(sapply(sys.frames(), function(x) ls(x)))
  #cat("Call stack for UseFunction:\n")
  #sapply(sys.calls(), function(x) print(x))
  cat("\n")
  #fn.name <- deparse(substitute(fn))
  fn <- get(fn.name, inherits=TRUE)
  result <- NULL
  raw.args <- list(...)
  vs <- get_variant(fn,length(raw.args))
  if (is.null(vs) || length(vs) < 1)
    stop(use_error(.ERR_NO_MATCH,fn.name,raw.args))
  matched.fn <- NULL
  for (v in vs)
  {
    full.args <- fill_args(raw.args, v)
    full.type <- get_type(fn,v$type.index)
    if (!check_types(full.type, full.args)) next
    if (is.null(v$guard)) { matched.fn <- v$def; break }
    gout <- do.call(v$guard, full.args)
    if (length(gout) > 0 && gout) { matched.fn <- v$def; break }
  }
  if (is.null(matched.fn))
    stop(use_error(.ERR_USE_FUNCTION,fn.name,raw.args))

  if (attr(fn,'debug')) debug(matched.fn)
  result <- do.call(matched.fn, full.args)

  if (!is.null(full.type))
  {
    if (!full.type$types$text[length(raw.args)+1] %in% class(result))
    {
      exp <- full.type$types$text[length(raw.args)+1]
      act <- paste(class(result), collapse=', ')
      msg <- sprintf("Expected '%s' as return type but found '%s' for",exp,act)
      stop(use_error(msg,fn.name,raw.args))
    }
  }

  result
}

fill_args <- function(raw.args, tree)
{
  tree$args <- tree$args[tree$args$token != '...',]
  default <- tree$args$default

  # This is for unnamed arguments
  if (is.null(names(raw.args)))
  {
    if (length(raw.args) == length(default)) return(raw.args)
    c(raw.args, default[(length(raw.args)+1):length(default)] )
  }
  else
  {
    names(default) <- tree$args$token
    shim <- tree$args$token[1:length(raw.args)]
    names(raw.args)[names(raw.args) == ""] <- shim[names(raw.args) == '']
    default[names(raw.args)] <- raw.args
    default
  }
}

# Validate arguments against types
check_types <- function(raw.types, raw.args)
{
  if (is.null(raw.types)) return(TRUE)
  types <- raw.types$types
  if (nrow(types) - 1 != length(raw.args)) return(FALSE)
  arg.types <- sapply(raw.args, function(x) class(x))
  idx <- 1:length(raw.args)
  if (!is.null(ncol(arg.types)) && ncol(arg.types) > 1)
    all(sapply(idx, function(x) types$text[x] %in% arg.types[,x]))
  else
    all(sapply(idx, function(x) types$text[x] %in% arg.types[[x]]))
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

use_error <- function(msg, fn.name, raw.args)
{
  args <- paste(sapply(raw.args, as_simple), collapse=',')
  signature <- sprintf("'%s(%s)'", fn.name, args)
  sprintf("%s %s", msg, signature)
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

# parse_fun(raw=parser(text="fib(0,y=some.fun(1)) %as% 1"))
# parse_fun(raw=parser(text="fib(x,y=some.fun(1), 'bgfs') %as% 1"))
parse_fun <- function(it, raw=NULL)
{
  if (!is.null(raw))
  {
    if (!is.null(attr(raw,'data'))) raw <- attr(raw,'data')
    it <- iterator(raw)
  }
  name <- get_name(it)
  paren.level <- 0
  node <- 'function.name'
  out <- data.frame(paren.level=paren.level, node=node, token=name,
    pattern=NA, default=NA, stringsAsFactors=FALSE)

  arg.idx <- 1
  node <- 'argument'
  token <- pattern <- default <- NULL
  in.default <- FALSE
  while (!is.na(line <- it()) && line$token.desc != "SPECIAL")
  {
    line.token <- line$token.desc
    if (line.token == 'expr') next
    if (line.token == "'('") 
    {
      paren.level <- paren.level + 1
      if (paren.level == 1) next # Opening function parenthesis
    }
    if (line.token == "')'")
    {
      paren.level <- paren.level - 1
      if (paren.level < 1) # Closing function parenthesis
      {
        if (is.null(token)) token <- paste('.lambda',arg.idx,sep='_')
        if (is.null(pattern)) pattern <- NA
        else pattern <- strip_quotes(paste(pattern, collapse=' '))
        if (is.null(default)) default <- NA
        else default <- strip_quotes(paste(default, collapse=' '))
        out <- rbind(out, c(1,node,token,pattern,default))
        break
      }
    }

    #cat("paren.level:",paren.level,"\n")
    if (paren.level == 1) 
    {
      if (line.token %in% c('SYMBOL','SYMBOL_SUB','SYMBOL_FUNCTION_CALL'))
      {
        token <- line$text
        next
      }
      if (line.token == 'EQ_SUB')
      {
        in.default <- TRUE
        next
      }
      # Close current node
      if (line.token == "','")
      {
        if (is.null(token)) token <- paste('.lambda',arg.idx,sep='_')
        if (is.null(pattern)) pattern <- NA
        else pattern <- strip_quotes(paste(pattern, collapse=' '))
        if (is.null(default)) default <- NA
        else default <- strip_quotes(paste(default, collapse=' '))

        out <- rbind(out, c(paren.level,node,token,pattern,default))
        token <- pattern <- default <- NULL
        node <- 'argument'
        arg.idx <- arg.idx + 1
        in.default <- FALSE
        next
      }

      if (in.default)
        default <- c(default, line$text)
      else
        pattern <- c(pattern, line$text)
    }
    else 
    {
      default <- c(default, line$text)
      #cat("Default is now",default,"\n")
    }
  }
  out
}

strip_quotes <- function(x) sub('^[\'"]([^\'"]+)[\'"]$', '\\1', x)


parse_guard <- function(it)
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
  args <- raw.args$token
  # Add any pattern matches
  if (any(!is.na(raw.args$pattern)))
  {
    patterns <- raw.args[!is.na(raw.args$pattern),]
    lines <- paste(patterns$token,'==',patterns$pattern, sep=' ')
  }

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
  fn.string <- sprintf("function(%s) { %s }", arg.string, body)
  eval(parse(text=fn.string))
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


parse_body <- function(it)
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


body_fn <- function(raw.args, tree)
{
  if (tree$token.desc[1] == "'{'") tree <- tree[2:(nrow(tree)-1), ]
  lines <- NULL
  args <- raw.args$token

  if (!is.null(tree))
  {
    f <- function(x) paste(tree[tree$line1 %in% x,]$text, collapse=' ')
    index <- array(unique(tree$line1))
    lines <- apply(index,1,f)
  }

  if (length(lines) < 1) return(NULL)

  body <- paste(lines, collapse='\n')
  arg.string <- paste(args, collapse=',')
  fn.string <- sprintf("function(%s) { %s }", arg.string, body)
  eval(parse(text=fn.string))
}

parse_types <- function(it, args, expr)
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

add_variant <- function(fn.name, tree)
{
  frames <- sys.frames()
  if (length(frames) < 3)
    where <- topenv(parent.frame(2))
  else
    where <- target_env(sys.calls()[[length(frames)-2]], length(frames))

  setup_parent(fn.name, where)
  fn <- get(fn.name, where)
  variants <- attr(fn,'variants')

  args <- tree$args
  required.args <- length(args$default[is.na(args$default)])
  if ('...' %in% tree$args$token)
    tree$accepts <- c(required.args : nrow(args) - 1, Inf)
  else
    tree$accepts <- required.args : nrow(args)
  type.index <- get_type_index(fn, nrow(args))
  if (!is.null(type.index) && length(type.index) > 0)
    tree$type.index <- type.index
  variants[[length(variants) + 1]] <- tree
  attr(fn,'variants') <- variants

  assign(fn.name, fn, where)
  invisible()
}

get_variant <- function(fn, arg.length)
{
  raw <- attr(fn,'variants')
  match.fn <- function(x)
    arg.length >= min(x$accepts) & arg.length <= max(x$accepts)
  matches <- sapply(raw, match.fn)
  raw[matches]
}

# Type definitions are always additive
add_type <- function(fn.name, tree)
{
  # We use 2 because this is called from within the 'guard' function so the
  # stack is two down
  where <- topenv(parent.frame(2))
  setup_parent(fn.name, where)
  fn <- get(fn.name, where)
  types <- attr(fn,'types')

  args <- tree$args
  tree$accepts <- length(args$default[is.na(args$default)]) : nrow(args)
  types[[length(types) + 1]] <- tree
  attr(fn,'types') <- types

  assign(fn.name, fn, where)
  invisible()
}

# Type declarations are scoped based on when they are created. They continue
# until a new type declaration is added.
get_type <- function(fn, idx)
{
  if (is.null(idx)) return(NULL)
  raw <- attr(fn,'types')
  if (length(raw) < 1) return(NULL)
  match <- raw[[idx]]
}

# Get the index for the most recent type declaration for the given arg.length
get_type_index <- function(fn, arg.length)
{
  raw <- attr(fn,'types')
  if (length(raw) < 1) return(NULL)
  
  match.fn <- function(x)
    any(arg.length >= raw[[x]]$accepts & arg.length <= raw[[x]]$accepts)
  matches <- data.frame(idx=(1:length(raw)), v=sapply(1:length(raw), match.fn))
  if (!all(matches$v)) return(NULL)
  max(matches$idx[matches$v==TRUE])
}

setup_parent <- function(parent, where)
{
  # Overwrite a final definition (as opposed to appending)
  if (exists(parent, where))
  {
    parent.def <- get(parent, where)
    is.final <- attr(parent.def, 'sealed')
    if ((!is.null(is.final) && is.final == TRUE) ||
        (! any(c('lambdar.fun','lambdar.type') %in% class(parent.def))) )
    {
      parent.def <- init_function(parent)
      assign(parent, parent.def, where)
    }
  }
  else
  {
    parent.def <- init_function(parent)
    assign(parent, parent.def, where)
  }
}

init_function <- function(name)
{
  if (is.type(name)) 
    pattern <- 'function(...) NewObject("%s",...)'
  else
    pattern <- 'function(...) UseFunction("%s",...)'
  fn <- eval(parse(text=sprintf(pattern,name)))
  if (is.type(name))
    attr(fn, 'class') <- c('lambdar.type',attr(fn,'class'))
  else
    attr(fn, 'class') <- c('lambdar.fun',attr(fn,'class'))
  attr(fn, 'variants') <- list()
  attr(fn, 'types') <- list()
  attr(fn, 'debug') <- FALSE
  fn
}


# Check if the same signature already exists in the function. If so return the
# index of the existing definition
# Types do not require default values specified in the signature, so we don't
# check for that
# With guards, there could be multiple matches, so each match will get a type
# added
# For adding types, we want to match all applicable
# INCOMPLETE - Requires examining guards as well
signature_idx <- function(tree, variants)
{
  if (length(variants) < 1) return(NULL)
  args <- tree$args
  fn <- function(idx)
  {
    vargs <- variants[[idx]]$args
    if (nrow(args) != nrow(vargs)) return(NULL)
    if (length(args$pattern[is.na(args$pattern)]) !=
        length(vargs$pattern[is.na(vargs$pattern)]) ) return(NULL)
    if (!all(args$token == vargs$token))
      stop("Mismatched argument names found")
    idx
  }
  temp <- sapply(array(1:length(variants)), fn)
  do.call(c, temp)
}

seal <- function(fn)
{
  fn.name <- deparse(substitute(fn))
  attr(fn,'sealed') <- TRUE
  assign(fn.name, fn, inherits=TRUE)
  invisible()
}

# This is a fall back for special cases. It is clearly not efficient but is
# necessary for unit testing frameworks that manipulate the normal environment
# structures
# Returns the index of the most recent frame that contains the variable
# UNUSED
really_get <- function(x)
{
  frames <- sys.frames()
  match.idx <- sapply(frames, function(y) x %in% ls(y))
  frame.idx <- (1:length(frames))[match.idx]
  if (length(frame.idx) < 1) stop("Still couldn't find ",x,"\n")
  get(x, frames[frame.idx[length(frame.idx)]])
}

# Get the target env for the function definition. Normally this would be
# just traversing the frame stack, but we need to add special logic to
# handle eval() calls with an explicit environment.
target_env <- function(head.call, frame.length)
{
  parsed.call <- attr(parser(text=deparse(head.call)),'data')
  it <- iterator(parsed.call)
  args <- parse_eval(it)

  # 3 is a magic number based on the lambda.r call stack
  stack.depth <- 3
  top.frame <- topenv(parent.frame(stack.depth))
  if (args$token[1] != 'eval') return(top.frame)
  #if (nrow(args) < 3) return(top.frame)

  eval.frame <- sys.frame(frame.length-stack.depth)
  lambda.r_temp_env <- tryCatch(get('envir', envir=eval.frame),
    error=function(e) { cat("WARNING: Falling back to top.frame\n"); top.frame})
  if ('lambda.r_temp_env' %in% search())
    detach('lambda.r_temp_env', character.only=TRUE)

  #cat("Note: Forcing eval env onto search path\n")
  attach(lambda.r_temp_env)
  lambda.r_temp_env
}

parse_eval <- function(it, raw=NULL)
{
  if (!is.null(raw))
  {
    if (!is.null(attr(raw,'data'))) raw <- attr(raw,'data')
    it <- iterator(raw)
  }
  name <- get_name(it)
  paren.level <- 0
  node <- 'function.name'
  out <- data.frame(paren.level=paren.level, node=node, token=name,
    pattern=NA, default=NA, stringsAsFactors=FALSE)

  arg.idx <- 1
  node <- 'argument'
  token <- NULL
  while (!is.na(line <- it()) && TRUE)
  {
    line.token <- line$token.desc
    if (line.token == 'expr') next
    if (line.token == "'('") 
    {
      paren.level <- paren.level + 1
      if (paren.level == 1) next # Opening function parenthesis
    }
    if (line.token == "')'")
    {
      paren.level <- paren.level - 1
      if (paren.level < 1) # Closing function parenthesis
      {
        out <- rbind(out, c(1,node,paste(token,collapse=' '),NA,NA))
        break
      }
    }

    if (paren.level == 1 && line.token == "','")
    {
      out <- rbind(out, c(paren.level,node,paste(token,collapse=' '),NA,NA))
      token <- NULL
      arg.idx <- arg.idx + 1
      next
    }
    token <- c(token, line$text)
  }
  out
}

