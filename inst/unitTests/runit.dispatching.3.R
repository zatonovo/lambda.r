test.dispatching_3 <- function() {
  Integer(x) %as% x

  fib(n) %::% Integer : Integer
  fib(0) %as% Integer(1)
  fib(1) %as% Integer(1)
  fib(n) %as% { Integer(fib(n-1) + fib(n-2)) }
  seal(Integer)
  seal(fib)

  checkEquals(fib(Integer(5)), Integer(8))

  act <- tryCatch(fib(5), error=function(x) 'error')
  checkEquals(act, 'error')
}
