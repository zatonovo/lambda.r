test.auto_replace_3 <- function() {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n=5) %as% { fib(n-1) - fib(n-2) }
  fib(n=2) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  # These are failing
  act <- fib(3)
  checkEquals(act, 3)
  act <- fib(2)
  checkEquals(act, 2)
}

