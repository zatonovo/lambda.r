test.type_variable_2 <- function() {
  fib(n) %::% b : a
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }

  act <- tryCatch(f(2), error=function(x) 'error')
  checkEquals(act, 'error')
}

