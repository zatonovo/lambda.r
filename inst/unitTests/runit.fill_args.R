#act <- tryCatch(fib(3), error=function(x) 'error')
#checkEquals(act, 'error')

test.type_fill_args_1 <- function() {
  mysum(x, y, ...) %as% { (x - y) * sum(...) }
  seal(mysum)

  act <- mysum(2, 3, 1, 2, 3)
  checkEquals(act, -6)

  act <- mysum(x=2, 3, 1, 2, 3)
  checkEquals(act, -6)

  act <- mysum(2, y=3, 1, 2, 3)
  checkEquals(act, -6)

  act <- mysum(y=3, x=2, 1, 2, 3)
  checkEquals(act, -6)

  act <- mysum(y=3, 1, 2, 3, x=2)
  checkEquals(act, -6)

  act <- mysum(2, 1, 2, 3, y=3)
  checkEquals(act, -6)
}

test.type_fill_args_2 <- function() {
  mysum(x, y=3, ...) %as% { (x - y) * sum(...) }
  seal(mysum)

  act <- mysum(2, 1, 1, 2, 3)
  checkEquals(act, -7)

  act <- mysum(1, y=2, 1, 1, 2, 3)
  checkEquals(act, -7)

  act <- mysum(y=2, x=1, 1, 1, 2, 3)
  checkEquals(act, -7)

  act <- mysum(1, 1, 2, 3, x=2)
  checkEquals(act, -7)

  act <- mysum(1, 1, 1, 2, 3, y=2)
  checkEquals(act, -7)
}


