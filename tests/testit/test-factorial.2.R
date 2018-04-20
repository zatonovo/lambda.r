rm(list=ls())

assert('factorial_2', {
  WholeNumber(x) %when% { x > 0 } %as% x

  fac(n) %::% WholeNumber : WholeNumber
  fac(0) %as% WholeNumber(1)
  fac(n) %when% { n > 0 } %as% { n * fac(n - 1) }

  fac(n) %::% numeric : WholeNumber
  fac(n) %as% fac(WholeNumber(n))

  (fac(WholeNumber(1)) == WholeNumber(1))
  (fac(WholeNumber(5)) == WholeNumber(120))
  (fac(1) == WholeNumber(1))
  (fac(5) == WholeNumber(120))

  act <- tryCatch(fac(-1), error=function(x) 'error')
  (act == 'error')
})
