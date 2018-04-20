rm(list=ls())

#act <- tryCatch(fib(3), error=function(x) 'error')
#checkEquals(act, 'error')

assert('type_fill_args_1', {
  mysum(x, y, ...) %as% { (x - y) * sum(...) }
  seal(mysum)

  act <- mysum(2, 3, 1, 2, 3)
  (act == -6)

  act <- mysum(x=2, 3, 1, 2, 3)
  (act == -6)

  act <- mysum(2, y=3, 1, 2, 3)
  (act == -6)

  act <- mysum(y=3, x=2, 1, 2, 3)
  (act == -6)

  act <- mysum(y=3, 1, 2, 3, x=2)
  (act == -6)

  act <- mysum(2, 1, 2, 3, y=3)
  (act == -6)
})

assert('type_fill_args_2', {
  mysum(x, y=3, ...) %as% { (x - y) * sum(...) }
  seal(mysum)

  act <- mysum(2, 1, 1, 2, 3)
  (act == -7)

  act <- mysum(1, y=2, 1, 1, 2, 3)
  (act == -7)

  act <- mysum(y=2, x=1, 1, 1, 2, 3)
  (act == -7)

  act <- mysum(1, 1, 2, 3, x=2)
  (act == -7)

  act <- mysum(1, 1, 1, 2, 3, y=2)
  (act == -7)
})


