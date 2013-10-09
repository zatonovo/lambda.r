#act <- tryCatch(fib(3), error=function(x) 'error')
#checkEquals(act, 'error')

test.type_ellipsis_1 <- function() {
  ioc(f, ...) %::% Function : ... : .
  ioc(f, ...) %as% f(...)
  seal(ioc)

  act <- ioc(sum, 1, 2, 3)
  checkEquals(act, 6)
}

test.type_ellipsis_2 <- function() {
  ioc(f, ...) %::% Function : ... : numeric
  ioc(f, ...) %as% f(...)
  seal(ioc)

  act <- ioc(sum, 1, 2, 3)
  checkEquals(act, 6)
}

test.type_ellipsis_3 <- function() {
  mysum(x, ...) %::% a : ... : numeric
  mysum(x, ...) %as% sum(...)
  seal(mysum)

  act <- mysum('foo', 1, 2, 3)
  checkEquals(act, 6)
}

test.type_ellipsis_4 <- function() {
  mysum(..., x) %::% ... : logical : numeric
  mysum(..., x) %as% sum(..., na.rm=x)
  seal(mysum)

  act <- mysum(1, 2, 3, x=TRUE)
  checkEquals(act, 6)
}

test.type_ellipsis_var_1 <- function() {
  mysum(..., x) %::% numeric... : logical : numeric
  mysum(..., x) %as% sum(..., na.rm=x)
  seal(mysum)

  act <- mysum(1, 2, 3, x=FALSE)
  checkEquals(act, 6)
}

