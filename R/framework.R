EMPTY <- 'EMPTY'

#' Check if name is bound to a non-lambda.r object
is.bound <- function(name) {
  if (! exists(name, inherits=TRUE)) return(FALSE)

  o <- get(name, inherits=TRUE)
  ! any(c('lambdar.fun','lambdar.type') %in% class(o))
}

# f(a,b) %::% A : B : C
'%::%' <- function(signature, types) {
  os <- options(keep.source=TRUE)
  s.expr <- paste(deparse(substitute(signature)), collapse="\n")
  t.expr <- paste(deparse(substitute(types)), collapse="\n")
  text <- paste(s.expr,t.expr, sep=" %::% ")
  raw <- getParseData(parse(text=text))
  # SPECIAL tokens now appear with a leading white space
  raw$text <- sub("^ ","", raw$text)

  it <- iterator(raw)
  tree <- list(args=NULL)
  args_expr <- parse_fun(it)
  name <- args_expr$token[1]
  if (is.bound(name))
    stop("Function name is already bound to non lambda.r object")

  if (nrow(args_expr) > 1)
    tree$args <- args_expr[2:nrow(args_expr),]
  tree$types <- parse_types(it, tree$args, text)
  tree$signature <- paste(s.expr,"%::%",t.expr, sep=' ')

  add_type(name, tree)
  options(keep.source=os$keep.source)
  invisible()
}


# f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + 2; z * 2 }
# f(a,b) %when% { a < 0 } %as% { abs(a) + b }
# f(a,b) %as% { a + b }
'%as%' <- function(signature, body) {
  os <- options(keep.source=TRUE)
  s.expr <- paste(deparse(substitute(signature)), collapse="\n")
  b.expr <- paste(deparse(substitute(body)), collapse="\n")
  text <- paste(s.expr,b.expr, sep=" %as% ")
  raw <- getParseData(parse(text=text))
  # SPECIAL tokens now appear with a leading white space
  raw$text <- sub("^ ","", raw$text)
  it <- iterator(raw)

  tree <- list(args=NULL)
  args_expr <- parse_fun(it)
  name <- args_expr$token[1]
  if (is.bound(name))
    stop("Function name is already bound to non lambda.r object")

  where <- get_function_env()
  #cat(sprintf("Function env for %s is\n", name))
  #print(where)
  #cat("\n")

  if (nrow(args_expr) > 1)
    tree$args <- args_expr[2:nrow(args_expr),]
  guard_expr <- parse_guard(it)
  guard_expr <- transform_attrs(guard_expr)
  if (!is.null(tree$args))
    tree$guard <- guard_fn(tree$args, guard_expr, where)

  body_expr <- parse_body(it)
  body_expr <- transform_attrs(body_expr)
  tree$def <- body_fn(tree$args, body_expr, where)
  tree$signature <- s.expr
  tree$body <- b.expr
  tree$ellipsis <- idx_ellipsis(tree)
  tree$fill.tokens <- clean_tokens(tree)
  tree$fill.defaults <- clean_defaults(tree)

  add_variant(name, tree, where)
  options(keep.source=os$keep.source)
  invisible()
}

################################## RUN TIME ###################################
.ERR_NO_MATCH <- "No match for function"
.ERR_USE_FUNCTION <- "No valid function for"
.ERR_ENSURE_FAILED <- "Assertion '%s' failed for args = %s and result = %s"
#NewObject <- function(type.name, ...)
NewObject <- function(type.fn,type.name, ...)
{
  result <- UseFunction(type.fn,type.name, ...)

  type <- gsub('"','', type.name)
  if (!type %in% class(result))
    class(result) <- c(type, class(result))
  result
}

# Some timings
# Baseline:
# g <- function(x) x
# system.time(for (i in 1:10000) g(i) )
#  user  system elapsed
# 0.004   0.000   0.003
#
# S3:
# h <- function(x, ...) UseMethod("h")
# h.default <- function(x, ...) x
# system.time(for (i in 1:10000) h(i) )
#  user  system elapsed
# 0.035   0.001   0.035
#
# Lambda.r:
# f(x) %as% x
# system.time(for (i in 1:10000) { fn <- get('f', inherits=TRUE) })
#  user  system elapsed
# 0.017   0.000   0.018
#
# system.time(for (i in 1:10000) f(i) )
#  user  system elapsed
# 1.580   0.005   1.590
# 0.622   0.005   0.628
# 0.443   0.003   0.447
# 0.407   0.000   0.408
# 0.391   0.001   0.392
# 0.384   0.001   0.386
# 0.372   0.003   0.376
# 0.347   0.001   0.347
# 0.305   0.000   0.305
# 0.238   0.000   0.238
UseFunction <- function(fn,fn.name, ...)
{
  result <- NULL
  # u:0.007 s:0.002
  raw.args <- list(...)
  # u:0.305 s:0.010
  # u:0.096 s:0.002
  # u:0.088 s:0.004
  # u:0.082 s:0.000
  vs <- get_variant(fn,length(raw.args))
  if (is.null(vs) || length(vs) < 1)
    stop(use_error(.ERR_NO_MATCH,fn.name,raw.args))

  matched.fn <- NULL
  for (v in vs)
  {
    # u:1.007 s:0.006
    # u:0.106 s:0.001
    # u:0.068 s:0.001
    full.args <- fill_args(raw.args, v$fill.tokens, v$fill.defaults, v$ellipsis)
    if (is.null(full.args)) next
    # u:0.019 s:0.003
    full.type <- get_type(fn,v$type.index)
    if (!check_types(full.type, full.args)) next
    if (is.null(v$guard)) { matched.fn <- v$def; break }
    gout <- do.call(v$guard, full.args)
    if (!is.na(gout) && length(gout) > 0 && gout) { matched.fn <- v$def; break }
  }
  if (is.null(matched.fn))
    stop(use_error(.ERR_USE_FUNCTION,fn.name,raw.args))

  result <- do.call(matched.fn, full.args)

  if (!is.null(full.type))
  {
    result.class <- class(result)
    return.type <- return_type(full.type, full.args, result.class)[1]
    if ('integer' %in% result.class) result.class <- c(result.class, 'numeric')

    if (return.type == '.') {
      NULL
    } else if (return.type == '.lambda.r_UNIQUE') {
      act <- paste(result.class, collapse=', ')
      first <- result.class[1]
      if (first %in% sapply(raw.args, class)) {
        msg <- sprintf("Expected unique return type but found '%s' for",first)
        stop(use_error(msg,fn.name,raw.args))
      }
    } else if (!return.type %in% result.class) {
      exp <- return.type
      act <- paste(result.class, collapse=', ')
      msg <- sprintf("Expected '%s' as return type but found '%s' for",exp,act)
      stop(use_error(msg,fn.name,raw.args))
    }
  }

  result
}


idx_ellipsis <- function(tree) {
  which(tree$args$token == '...')
}

clean_tokens <- function(tree) {
  if (length(tree$ellipsis) == 0)
    tree$args$token
  else
    tree$args$token[-tree$ellipsis]
}

clean_defaults <- function(tree) {
  if (length(tree$ellipsis) == 0)
    tree$args$default
  else
    tree$args$default[-tree$ellipsis]
}

# rm(list=ls()); detach('package:lambda.r', unload=TRUE); library(lambda.r)
fill_args <- function(params, tokens, defaults, idx.ellipsis)
{
  args <- list()
  if (is.null(params) && all(is.na(defaults))) return(args)

  # Skip parameters that don't coincide with the expected tokens
  param.names <- names(params)
  if (!is.null(param.names) &&
      !all(param.names[nchar(param.names) > 0] %in% tokens) && 
      length(idx.ellipsis) == 0) return(NULL)

  # Initialize arguments with NA
  arg.length <- max(length(tokens), length(defaults)) + length(idx.ellipsis)
  if (arg.length == 0) return(args)

  idx.concrete <- idx.args <- 1:arg.length
  if (length(idx.ellipsis) > 0)
    idx.concrete <- idx.args[-idx.ellipsis]
  names(idx.concrete) <- tokens
  args[idx.args] <- NA
  names(args)[idx.concrete] <- tokens

  # Populate named arguments
  named.params <- param.names[param.names %in% tokens]
  args[named.params] <- params[named.params]

  # Catalog named and unnamed arguments
  if (length(params) > 0) {
    idx.params <- 1:length(params)
    names(idx.params) <- names(params)
    if (is.null(named.params) || length(named.params) < 1) {
      idx.p.named <- integer()
      idx.p.unnamed <- idx.params
      idx.a.named <- integer()
      idx.a.unnamed <- idx.concrete
    } else {
      idx.p.named <- idx.params[named.params]
      idx.p.unnamed <- idx.params[-idx.p.named]
      idx.a.named <- idx.concrete[named.params]
      idx.a.unnamed <- idx.concrete[-idx.a.named]
    }

    if (length(idx.ellipsis) > 0) {
      # Choose only required arguments
      idx.required <- idx.concrete[is.na(defaults)]
      idx.required <- idx.required[!idx.required %in% idx.a.named]

      # Set arguments before ellipsis
      idx.left <- idx.required[idx.required < idx.ellipsis]
      args[idx.left] <- params[idx.p.unnamed[1:length(idx.left)]]

      idx.right <- idx.required[idx.required > idx.ellipsis]
      args[idx.right] <- params[tail(idx.p.unnamed, length(idx.right))]

      # Fill the ellipsis with the remainder
      orphans <- c(idx.p.named, idx.left, idx.right)
      if (length(orphans) == 0) {
        args[[idx.ellipsis]] <- params
      } else {
        args[[idx.ellipsis]] <- params[-orphans]
      }
    } else if (length(idx.p.unnamed) > 0) {
        args[idx.a.unnamed[1:length(idx.p.unnamed)]] <- params[idx.p.unnamed]
    }
  }

  # Apply default values to unset optional arguments
  if (!is.null(defaults)) {
    idx.optional <- idx.concrete[is.na(args[idx.concrete]) & !is.na(defaults)]
    if (length(idx.ellipsis) > 0) {
      idx.defaults <- ifelse(idx.optional >= idx.ellipsis,
        idx.optional - 1,
        idx.optional)
    } else {
      idx.defaults <- idx.optional
    }
    args[idx.optional] <- lapply(idx.defaults, 
      function(idx) eval(parse(text=defaults[idx]), list2env(args)))
  }

  if (length(idx.ellipsis) > 0) {
    names(args)[idx.ellipsis] <- ''
    #args <- c(args[-idx.ellipsis],unlist(args[idx.ellipsis], recursive=FALSE))
    args <- c(args[idx.args < idx.ellipsis],
      unlist(args[idx.ellipsis], recursive = FALSE),
      args[idx.args > idx.ellipsis])
  }
  args
}


# Return the index of the ellipsis argument or an empty vector otherwise
has_ellipsis <- function(declared.types) {
  idx <- 1:length(declared.types)
  val <- sapply(declared.types, 
    function(x) any(grep('...', x, fixed=TRUE) > 0))
  idx[val]
}

update_type_map <- function(type.map, the.type, arg.type) {
  if (is.null(type.map[[the.type]])) {
    if (any(arg.type %in% type.map))
      # This forces a failure in the type check later on
      type.map[[the.type]] <- paste("!",arg.type,sep='')
    else
      # Add the new type if it doesn't exist
      type.map[[the.type]] <- arg.type
  }
  type.map
}

strip_ellipsis <- function(the.type) {
  sub('...','',the.type, fixed=TRUE)
}

# Used internally to determine the declared type based on its
# value and corresponding argument type.
dereference_type <- function(declared.types, arg.types) {
  type.map <- list()
  len.delta <- length(arg.types) - length(declared.types) + 1

  # Check for type variables (can only be a-z)
  fn <- function(x) {
    the.type <- declared.types[[x]]
    if (the.type == '.')
      return(arg.types[[x]])
    else if (the.type == '...') 
      return(arg.types[x + 0:len.delta])
    else if (the.type %in% letters) {
      type.map <<- update_type_map(type.map, the.type, arg.types[[x]])
      return(type.map[[the.type]])
    }
    else if (any(grep('[a-z]\\.\\.\\.', the.type) > 0)) {
      the.type <- strip_ellipsis(the.type)
      type.map <<- update_type_map(type.map, the.type, arg.types[[x]])
      return(rep(type.map[[the.type]], len.delta + 1))
    }
    else if (any(grep('[a-zA-Z0-9._]+\\.\\.\\.', the.type) > 0)) {
      the.type <- strip_ellipsis(the.type)
      return(rep(the.type, len.delta + 1))
    }
    # Default
    the.type
  }
}


# Validate arguments against types
check_types <- function(raw.types, raw.args)
{
  if (is.null(raw.types)) return(TRUE)
  declared.types <- raw.types$types$text
  idx.ellipsis <- has_ellipsis(declared.types)
  if (length(idx.ellipsis) == 0 &&
      nrow(raw.types$types) - 1 != length(raw.args)) return(FALSE)

  arg.fn <- function(x) {
    cl <- class(x)
    if ('integer' %in% cl) cl <- c(cl, 'numeric')
    cl
  }
  arg.types <- lapply(raw.args, arg.fn)

  fn <- dereference_type(declared.types, arg.types)
  declared.types <- lapply(1:(length(declared.types)-1), fn)
  if (length(idx.ellipsis) > 0) {
    idx.declared <- 1:length(declared.types)
    declared.types <- c(
      declared.types[idx.declared[idx.declared < idx.ellipsis]],
      unlist(declared.types[idx.ellipsis], recursive=FALSE),
      declared.types[idx.declared[idx.declared > idx.ellipsis]]
    )
  }

  idx <- 1:length(raw.args)
  all(sapply(idx, function(x) any(declared.types[[x]] %in% arg.types[[x]])))
}



# Get the return type of a function declaration. This is aware of type
# variables.
# TODO: Make this more efficient using information computed
# by check_types.
return_type <- function(raw.types, raw.args, result.class)
{
  declared.types <- raw.types$types$text
  if (! has_ellipsis(declared.types) &&
      nrow(raw.types$types) - 1 != length(raw.args)) return(MissingReturnType)

  arg.types <- lapply(raw.args, function(x) class(x))

  # Check for type variables (can only be a-z)
  ret.type <- declared.types[length(declared.types)]
  if (ret.type %in% letters) {
    fn <- dereference_type(declared.types, c(arg.types,result.class))
    sapply(1:(length(declared.types)-1), fn)
    ret.type <- fn(length(declared.types))
    if (is.null(ret.type)) ret.type <- ".lambda.r_UNIQUE"
  }
  # Use Function as a proxy for function
  gsub('\\bFunction\\b','function',ret.type, perl=TRUE)
}

.SIMPLE_TYPES <- c('numeric','character','POSIXt','POSIXct','Date')
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
  if (!is.null(tree)) tree <- tree[! (tree$token=='expr' & tree$text==''),]
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
  if (line$token != 'SYMBOL_FUNCTION_CALL')
    stop("Function must start with a symbol (instead of ",line$token,")")
  line$text
}

# parse_fun(raw=parser(text="fib(0,y=some.fun(1)) %as% 1"))
# parse_fun(raw=parser(text="fib(x,y=some.fun(1), 'bgfs') %as% 1"))
parse_fun <- function(it, raw=NULL)
{
  if (!is.null(raw)) { it <- iterator(raw) }
  name <- get_name(it)
  paren.level <- 0
  node <- 'function.name'
  out <- data.frame(paren.level=paren.level, node=node, token=name,
    pattern=NA, default=NA, stringsAsFactors=FALSE)

  arg.idx <- 1
  node <- 'argument'
  token <- pattern <- default <- NULL
  in.default <- FALSE
  while (!is.na(line <- it()) && line$token != "SPECIAL")
  {
    line.token <- line$token
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
        # Check for 0 argument function
        if (is.null(token) && is.null(pattern)) break
        # Otherwise...
        if (!is.null(token) && token == EMPTY) {
          token <- NULL
          pattern <- EMPTY
        }
        if (is.null(token)) token <- paste('.lambda',arg.idx,sep='_')
        if (is.null(pattern)) pattern <- NA
        #else pattern <- strip_quotes(paste(pattern, collapse=' '))
        else pattern <- paste(pattern, collapse=' ')
        if (is.null(default)) default <- NA
        #else default <- strip_quotes(paste(default, collapse=' '))
        else default <- paste(default, collapse=' ')
        out <- rbind(out, c(1,node,token,pattern,default))
        break
      }
    }

    #cat("paren.level:",paren.level,"\n")
    if (paren.level == 1) 
    {
      if (!in.default && line.token %in% c('SYMBOL','SYMBOL_SUB','SYMBOL_FUNCTION_CALL'))
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
        if (!is.null(token) && token == EMPTY) {
          token <- NULL
          pattern <- EMPTY
        }
        if (is.null(token)) token <- paste('.lambda',arg.idx,sep='_')
        if (is.null(pattern)) pattern <- NA
        #else pattern <- strip_quotes(paste(pattern, collapse=' '))
        else pattern <- paste(pattern, collapse=' ')
        if (is.null(default)) default <- NA
        #else default <- strip_quotes(paste(default, collapse=' '))
        else default <- paste(default, collapse=' ')

        out <- rbind(out, c(paren.level,node,token,pattern,default))
        token <- pattern <- default <- NULL
        node <- 'argument'
        arg.idx <- arg.idx + 1
        in.default <- FALSE
        next
      }

      # TODO: Current structure will fail if a default uses a function call
      # with multiple arguments (due to the comma)
      if (in.default) {
        default <- c(default, line$text)
        #cat("Adding to default value:",line$text,"\n")
      } else
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
  while (!is.na(line <- it()) && line$token != "SPECIAL") next
  if (line$text == '%when%')
  {
    line <- it()
    if (line$token != "'{'")
      stop("Guard missing opening block")
    while (!is.na(line <- it()) && line$token != "'}'")
    {
      if (line$token %in% c("'{'"))
        stop("Invalid symbol '",line$text,"'in function definition")
      #if (line$token %in% c('expr',"','")) next
      if (line$token %in% c('expr')) next
      guards <- rbind(guards, line)
    }
    #while (!is.na(line <- it()) && line$token != "SPECIAL") next
  }
  else
    it(rewind=TRUE)
  guards[,c('line1','token','text')]
}

guard_fn <- function(raw.args, tree, where)
{
  lines <- NULL
  # Add any pattern matches
  if (any(!is.na(raw.args$pattern)))
  {
    patterns <- raw.args[!is.na(raw.args$pattern),]
    f <- function(x) {
      if (patterns$pattern[x] == 'NULL')
        paste("is.null(", patterns$token[x],")", sep='')
      else if (patterns$pattern[x] == 'NA')
        paste("is.na(", patterns$token[x],")", sep='')
      else if (patterns$pattern[x] == 'EMPTY')
        paste("length(", patterns$token[x],") == 0 || ",
          "(!is.null(dim(",patterns$token[x],")) && ",
          "nrow(",patterns$token[x],") == 0)" , sep='')
      else 
        paste(patterns$token[x],'==',patterns$pattern[x], sep=' ')
    }
    lines <- sapply(1:nrow(patterns), f)
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
  arg.string <- paste(raw.args$token, collapse=',')
  fn.string <- sprintf("function(%s) { %s }", arg.string, body)
  eval(parse(text=fn.string), where)
}

# A parse transform to change object@attribute to attr(object,'attribute')
# f(x) %when% { x@name == "bob" } %as% x
transform_attrs <- function(tree)
{
  start <- grep("'@'", tree$token, value=FALSE) - 1
  stop <- grep("SLOT", tree$token, value=FALSE)
  if (length(start) < 1) return(tree)

  template <- data.frame(line1=0,
    token=c('SYMBOL_FUNCTION_CALL',"'('",'SYMBOL',"','",'STR_CONST',"')'"),
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
  # Skip until we get to the 
  while (!is.na(line <- it()) && line$token != "SPECIAL") next
  if (line$text == '%as%')
  {
    needs.wrapping <- FALSE
    while (!is.na(line <- it()) && TRUE)
    {
      if (line$token %in% c('expr')) next
      body <- rbind(body, line)
    }
  }
  else
    it(rewind=TRUE)
  body[,c('line1','token','text')]
}


body_fn <- function(raw.args, tree, where)
{
  if (tree$token[1] == "'{'") tree <- tree[2:(nrow(tree)-1), ]
  lines <- NULL

  if (!is.null(tree))
  {
    f <- function(x) paste(tree[tree$line1 %in% x,]$text, collapse=' ')
    index <- array(unique(tree$line1))
    lines <- apply(index,1,f)
  }

  if (length(lines) < 1) return(NULL)

  body <- paste(lines, collapse='\n')
  if (is.null(raw.args))
    arg.string <- ''
  else
    arg.string <- paste(raw.args$token, collapse=',')
  fn.string <- sprintf("function(%s) { %s }", arg.string, body)
  eval(parse(text=fn.string), where)
}

parse_types <- function(it, args, sig)
{
  types <- NULL
  while (!is.na(line <- it()) && line$token != "SPECIAL") next
  if (line$text == '%::%')
  {
    while (!is.na(line <- it()) && TRUE)
    {
      if (line$token %in% c("'{'", "'}'", "'('", "')'"))
        stop("Invalid symbol '",line$text,"'in definition of ",sig)
      if (line$token != "SYMBOL") next
      types <- rbind(types, line)
    }
  }
  if (is.null(args)) {
    if (nrow(types) != 1)
      stop("Incorrect number of parameters in type declaration for ",sig)
  } else {
    if (nrow(args) != nrow(types) - 1)
      stop("Incorrect number of parameters in type declaration for ",sig)
  }

  types[,c('line1','token','text')]
}

from_root_env <- function(frames)
{
  length(frames) < 3
}

add_variant <- function(fn.name, tree, where)
{
  #cat("NOTE: Environment for",fn.name,"is\n", sep=' ')
  #print(sprintf("NOTE: Environment for %s is",fn.name))
  #print(where)
  env <- capture.output(str(as.environment(where), give.attr=FALSE))
  if (! is.null(tree$def)) {
    attr(tree$def,'topenv') <- env
    attr(tree$def,'name') <- fn.name
  } else {
    cat("NOTE: Empty body definition encountered for",tree$signature,"\n")
  }

  setup_parent(fn.name, where)
  fn <- get(fn.name, where)
  #cat(sprintf("The parent.env(%s) is\n", fn.name))
  #print(parent.env(environment(fn)))
  #cat("\n")

  variants <- attr(fn,'variants')
  active.type <- attr(fn,'active.type')
  args <- NULL

  if (is.null(tree$args))
    tree$accepts <- c(0,0)
  else {
    args <- tree$args
    required.args <- length(args$default[is.na(args$default)])
    if ('...' %in% tree$args$token)
      tree$accepts <- c(required.args-1, Inf)
      #tree$accepts <- c(required.args : nrow(args) - 1, Inf)
    else
      tree$accepts <- c(required.args, nrow(args))
    type.index <- get_type_index(fn, nrow(args), active.type)
    if (!is.null(type.index) && length(type.index) > 0)
      tree$type.index <- type.index
  }

  # Replace existing function clauses if there is a signature match
  idx <- has_variant(variants, args, tree$guard, active.type)
  if (length(idx) > 0) variants[[idx]] <- tree
  else variants[[length(variants) + 1]] <- tree
  attr(fn,'variants') <- variants

  assign(fn.name, fn, where)
  #if (! from_root_env(frames)) attach(where, name='lambda.r_temp_env')
  .sync_debug(fn.name)
  invisible()
}

get_variant <- function(fn, arg.length)
{
  # u:0.007 s:0.000
  raw <- attr(fn,'variants')
  len <- length(raw)
  matches <- vector(length=len)
  for (j in 1:len) {
    accepts <- raw[[j]]$accepts
    matches[j] <- arg.length >= accepts[1] && arg.length <= accepts[2]
  }
  raw[matches]
}

# Check whether this function already has the given variant
has_variant <- function(variants, args, guard=NULL, active.type=NULL)
{
  if (length(variants) == 0) return(variants)

  keys <- colnames(args)[! colnames(args) %in% 'default']
  fn <- function(x) {
    v <- variants[[x]]
    if (!is.null(v$type.index) && !is.null(active.type) && v$type.index != active.type) return(NA)
    var.len <- ifelse(is.null(v$args), 0, nrow(v$args))
    arg.len <- ifelse(is.null(args), 0, nrow(args))
    if (var.len != arg.len) return(NA)
    if (var.len == 0) return (x)

    if (!is.null(v$guard) || !is.null(guard)) {
      if (!is.null(v$guard) && is.null(guard)) return(NA)
      if (is.null(v$guard) && !is.null(guard)) return(NA)
      dv <- deparse(v$guard)
      dg <- deparse(guard)
      if (length(dv) != length(dg)) return(NA)
      if (!all(deparse(v$guard) == deparse(guard))) return(NA)
    }

    args$pattern[is.na(args$pattern)] <- ".lambdar_NA" 
    v$args$pattern[is.na(v$args$pattern)] <- ".lambdar_NA"
    ifelse(all(v$args[,keys] == args[,keys]),x, NA)
  }
  out <- sapply(1:length(variants), fn)
  out[!is.na(out)]
}

# Adds type constraint to function
# If an existing type constraint is encountered, then the active.type index
# will be set to this type constraint. This has the same effect as adding a
# new constraint.
add_type <- function(fn.name, tree)
{
  frames <- sys.frames()
  if (length(frames) < 3)
    where <- topenv(parent.frame(2))
  else
    where <- target_env(sys.calls()[[length(frames)-2]], length(frames))

  setup_parent(fn.name, where)
  fn <- get(fn.name, where)
  types <- attr(fn,'types')

  if (is.null(tree$args))
    tree$accepts <- c(0,0)
  else {
    args <- tree$args
    tree$accepts <- c(length(args$default[is.na(args$default)]), nrow(args))
  }
  f <- function(x) {
    ifelse(types[[x]]$signature == tree$signature, x, NA)
  }
  if (length(types) > 0)
  {
    out <- sapply(1:length(types), f)
  }
  else
    out <- NA
  out <- out[!is.na(out)]
  idx <- ifelse(length(out) == 0, length(types) + 1, out[1])
  types[[idx]] <- tree
  attr(fn,'types') <- types
  attr(fn,'active.type') <- idx

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
  # Use Function as a proxy for function
  char.type <- match$types$text
  match$types$text <- gsub('\\bFunction\\b','function',char.type, perl=TRUE)
  match
}

# Get the index for the most recent type declaration for the given arg.length
get_type_index <- function(fn, arg.length, active.type)
{
  raw <- attr(fn,'types')
  if (length(raw) < 1) return(NULL)
  if (!is.null(active.type) &&
      !is.null(raw[[active.type]]$args) &&
      nrow(raw[[active.type]]$args) == arg.length) return(active.type)
  
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
      parent.def <- init_function(parent, where)
      assign(parent, parent.def, where)
    }
  }
  else
  {
    parent.def <- init_function(parent, where)
    assign(parent, parent.def, where)
  }
}

init_function <- function(name, where)
{
  if (is.type(name)) 
    pattern <- 'function(...) NewObject(%s,"%s",...)'
  else
    pattern <- 'function(...) UseFunction(%s,"%s",...)'
  fn <- eval(parse(text=sprintf(pattern,name,name)), where)
  if (is.type(name))
    attr(fn, 'class') <- c('lambdar.type', 'function')
  else
    attr(fn, 'class') <- c('lambdar.fun', 'function')
  attr(fn, 'variants') <- list()
  attr(fn, 'types') <- list()
  #print(sprintf("Parent.env(%s) is", name))
  #print(parent.env(environment(fn)))
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

get_function_env <- function() {
  frames <- sys.frames()

  if (from_root_env(frames)) {
    #print("Assuming in root environment")
    where <- topenv(parent.frame(2))
  } else {
    #print("Getting target environment from call stack")
    #if ('lambda.r_temp_env' %in% search())
    #  detach('lambda.r_temp_env', character.only=TRUE)
    my.call <- sys.calls()[[length(frames)-2]]
    where <- target_env(my.call, length(frames))
  }
  where
}


# Get the target env for the function definition. Normally this would be
# just traversing the frame stack, but we need to add special logic to
# handle eval() calls with an explicit environment.
target_env <- function(head.call, frame.length)
{
  parsed.call <- getParseData(parse(text=deparse(head.call)))
  it <- iterator(parsed.call)
  args <- parse_eval(it)

  # 3 is a magic number based on the lambda.r call stack to this function
  stack.depth <- 3
  top.env <- topenv(parent.frame(stack.depth))
  if (args$token[1] != 'eval') return(top.env)

  eval.frame <- sys.frame(frame.length-stack.depth)
  lambda.r_temp_env <- tryCatch(get('envir', envir=eval.frame),
    error=function(e) stop("Unable to extract envir in eval frame\n"))

  #cat("NOTE: Using lambda.r_temp_env for",parsed.call[1,'token'],"\n", sep=' ')
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
    line.token <- line$token
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

.sync_debug <- function(fn.name) {
  os <- getOption('lambdar.debug')
  if (is.null(os)) return(invisible())

  os[[fn.name]] <- NULL
  options(lambdar.debug=os)
  invisible()
}

