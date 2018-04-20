#act <- tryCatch(fib(3), error=function(x) 'error')
#checkEquals(act, 'error')

rm(list=ls())
assert('type_ellipsis_1', {
  ioc(f, ...) %::% Function : ... : .
  ioc(f, ...) %as% f(...)
  seal(ioc)

  act <- ioc(sum, 1, 2, 3)
  (act == 6)
})

rm(list=ls())
assert('type_ellipsis_2', {
  ioc(f, ...) %::% Function : ... : numeric
  ioc(f, ...) %as% f(...)
  seal(ioc)

  act <- ioc(sum, 1, 2, 3)
  (act == 6)
})

rm(list=ls())
assert('type_ellipsis_3', {
  mysum(x, ...) %::% a : ... : numeric
  mysum(x, ...) %as% sum(...)
  seal(mysum)

  act <- mysum('foo', 1, 2, 3)
  (act == 6)
})

rm(list=ls())
assert('type_ellipsis_4', {
  mysum(..., x) %::% ... : logical : numeric
  mysum(..., x) %as% sum(..., na.rm=x)
  seal(mysum)

  act <- mysum(1, 2, 3, x=TRUE)
  (act == 6)
})

rm(list=ls())
assert('type_ellipsis_var_1', {
  mysum(..., x) %::% numeric... : logical : numeric
  mysum(..., x) %as% sum(..., na.rm=x)
  seal(mysum)

  act <- mysum(1, 2, 3, x=FALSE)
  (act == 6)
})

