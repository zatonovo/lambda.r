rm(list=ls())

assert('factorial_1', {
  fac(0) %as% 1
  fac(n) %when% { n > 0 } %as% { n * fac(n - 1) }
  seal(fac)

  act <- tryCatch(fac(-1), error=function(x) 'error')

  (fac(1) == 1)
  (fac(5) == 120)
  (act == 'error')
})
