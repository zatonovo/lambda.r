test.auto_replace_3 <- function() {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n=5) %as% { fib(n-1) - fib(n-2) }
  fib(n=2) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  # These are failing
  act.1 <- fib(3)
  act.2 <- fib(2)

  (act.1 == 3)
  (act.2 == 2)
}

