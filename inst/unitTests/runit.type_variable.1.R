test.type_variable_1 <- function() {
  fib(n) %::% a : a
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }

  #act <- tryCatch(f(2,3), error=function(x) 'error')
  #checkEquals(act, 'error')
  act <- fib(3)
  checkEquals(act, 3)
}
