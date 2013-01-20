test.auto_replace_1a <- function() {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
}

test.auto_replace_1b <- function() {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) - fib(n-2) }
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
}
