test.auto_replace.no_types_1a <- function() {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
}

test.auto_replace.no_types_1b <- function() {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) - fib(n-2) }
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
}

# Zero argument functions
test.auto_replace.no_types_1c <- function() {
  foo() %as% 2
  foo() %as% 1
  seal(foo)

  act <- foo()
  checkEquals(act, 1)
}

# Zero argument functions as part of a multipart definition
test.auto_replace.no_types_1c <- function() {
  foo(n) %as% n
  foo() %as% 2
  foo() %as% 1
  seal(foo)

  act <- foo()
  checkEquals(act, 1)
  act <- foo(5)
  checkEquals(act, 5)
}

