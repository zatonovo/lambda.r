test.dispatching_2 <- function() {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act.1 <- fib(5)
  checkEquals(act.1, 8)

  act.2 <- tryCatch(fib("a"), error=function(x) 'error')
  checkEquals(act.2, 'error')
}
