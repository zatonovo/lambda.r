test.type_variable_3 <- function() {
  fib(n) %::% a : b
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- tryCatch(f(2), error=function(x) 'error')
  checkEquals(act, 'error')
}

