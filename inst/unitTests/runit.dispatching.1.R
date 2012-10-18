test.dispatching_1 <- function() {
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %when% {
    abs(n - round(n)) < .Machine$double.eps^0.5
  } %as% {
    fib(n-1) + fib(n-2)
  }
  seal(fib)

  checkEquals(fib(5), 8)
}
