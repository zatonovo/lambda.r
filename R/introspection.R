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

# TODO: Use options to manage this. Looks like environments hash names.
# lambda.r.debug <- environment()
debug.lr <- function(x)
{
  name <- deparse(substitute(x))
  attr(x,'debug') <- TRUE
  assign(name,x,inherits=TRUE)
  # Need to think about how to do this efficiently for package functions
  #debugs <- c(getOption("lambda.r.debug"), name)
  #options(lambda.r.debug=debugs)
}

undebug.lr <- function(x)
{
  name <- deparse(substitute(x))
  attr(x,'debug') <- FALSE
  assign(name,x,inherits=TRUE)
  # Need to think about how to do this efficiently for package functions
  #debugs <- c(getOption("lambda.r.debug"), name)
  #debug <- debugs[debugs != name]
  #options(lambda.r.debug=debugs)
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
