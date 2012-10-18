test.factorial_1 <- function() {
  fac(0) %as% 1
  fac(n) %when% { n > 0 } %as% { n * fac(n - 1) }
  seal(fac)

  checkEquals(fac(1), 1)
  checkEquals(fac(5), 120)

  act <- tryCatch(fac(-1), error=function(x) 'error')
  checkEquals(act, 'error')
}
