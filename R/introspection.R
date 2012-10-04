describe(fn, idx, raw=FALSE) %when% { raw } %as% {
  class(fn) <- NULL
  print(fn)
}
describe(fn, idx) %when% {
  idx > 0
} %as% {
  variants <- attr(fn,'variants')
  types <- attr(fn,'types')
  if (is.null(variants)) stop("Nothing to describe")
  if (idx > length(variants)) stop("Invalid index specified")
  variants[[idx]]$def
}

seal(describe)

debug.lr <- function(x)
{
  name <- deparse(substitute(x))
  attr(x,'debug') <- TRUE
  assign(name,x,inherits=TRUE)
}

undebug.lr <- function(x)
{
  name <- deparse(substitute(x))
  attr(x,'debug') <- FALSE
  assign(name,x,inherits=TRUE)
}

print.lambdar.fun <- function(x, ...)
{
  variants <- attr(x,'variants')
  types <- attr(x,'types')
  if (is.null(variants)) stop("Lambda-R function incorrectly defined")
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
  if (is.null(variants)) stop("Lambda-R type constructor incorrectly defined")
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
