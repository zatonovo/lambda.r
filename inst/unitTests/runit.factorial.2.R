test.factorial_2 <- function() {
  WholeNumber(x) %when% { x > 0 } %as% x

  fac(n) %::% WholeNumber : WholeNumber
  fac(0) %as% WholeNumber(1)
  fac(n) %when% { n > 0 } %as% { n * fac(n - 1) }

  fac(n) %::% numeric : WholeNumber
  fac(n) %as% fac(WholeNumber(n))

  checkEquals(fac(WholeNumber(1)), WholeNumber(1))
  checkEquals(fac(WholeNumber(5)), WholeNumber(120))
  checkEquals(fac(1), WholeNumber(1))
  checkEquals(fac(5), WholeNumber(120))

  act <- tryCatch(fac(-1), error=function(x) 'error')
  checkEquals(act, 'error')
}
