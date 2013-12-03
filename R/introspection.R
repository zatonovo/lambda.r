describe(fn, idx, raw=FALSE) %when% { raw } %as% {
  class(fn) <- NULL
  print(fn)
}
describe(fn, idx) %when% {
  idx > 0
} %as% {
  variants <- attr(fn,'variants')
  types <- attr(fn,'types')
  if (length(variants) < 1) stop("Nothing to describe")
  if (idx > length(variants)) stop("Invalid index specified")
  variants[[idx]]$def
}
seal(describe)


debug.lr <- function(x)
{
  name <- deparse(substitute(x))
  os <- getOption('lambdar.debug')
  if (is.null(os)) os <- list()

  os[[name]] <- TRUE
  options(lambdar.debug=os)

  if (! any(c('lambdar.fun','lambdar.type') %in% class(x)))
    return(debug(x))

  variants <- attr(x,'variants')
  sapply(variants, function(v) debug(v$def))
  invisible()
}

undebug.lr <- function(x)
{
  if (is.function(x)) {
    name <- deparse(substitute(x))
  } else {
    name <- x
    x <- get(x, parent.frame(), inherits=TRUE)
  }
  os <- getOption('lambdar.debug')
  if (is.null(os)) return(invisible())

  os[[name]] <- NULL
  options(lambdar.debug=os)

  if (! any(c('lambdar.fun','lambdar.type') %in% class(x)))
    return(undebug(x))

  variants <- attr(x,'variants')
  sapply(variants, function(v) undebug(v$def))
  invisible()
}

#' Undebug all registered functions
undebug.all <- function() {
  sapply(which.debug(), undebug.lr)
  invisible()
}

is.debug <- function(fn.name) {
  os <- getOption('lambdar.debug')
  fn.name %in% names(os)
}

which.debug <- function() {
  names(getOption('lambdar.debug'))
}

print.lambdar.fun <- function(x, ...)
{
  variants <- attr(x,'variants')
  types <- attr(x,'types')
  if (is.null(variants)) stop("Oops: lambda.r function incorrectly defined")
  if (length(variants) < 1) stop("Function has no clauses")
  cat("<function>\n")
  fn <- function(idx)
  {
    f <- variants[[idx]]
    cat("[[",idx,"]]\n",sep='')
    if (!is.null(f$type.index)) 
      cat(types[[f$type.index]]$signature,"\n")
    cat(f$signature,"%as% ...\n")
  }
  sapply(1:length(variants),fn)
  invisible()
}

print.lambdar.type <- function(x, ...)
{
  variants <- attr(x,'variants')
  types <- attr(x,'types')
  if (is.null(variants)) stop("Oops: lambda.R type constructor incorrectly defined")
  cat("<type constructor>\n")
  fn <- function(idx)
  {
    f <- variants[[idx]]
    cat("[[",idx,"]]\n",sep='')
    if (!is.null(f$type.index)) 
      cat(types[[f$type.index]]$signature,"\n")
    cat(f$signature,"%as% ...\n")
  }
  sapply(1:length(variants),fn)
  invisible()
}
