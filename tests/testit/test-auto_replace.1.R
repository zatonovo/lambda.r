rm(list=ls())

assert('auto_replace.no_types_1a', {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  act == 3
})

assert('auto_replace.no_types_1b', {
  fib(0) %as% 2
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) - fib(n-2) }
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  act == 3
})

# Zero argument functions
assert('auto_replace.no_types_1c', {
  foo() %as% 2
  foo() %as% 1
  seal(foo)

  act <- foo()
  act == 1
})

# Zero argument functions as part of a multipart definition
assert('auto_replace.no_types_1c', {
  foo(n) %as% n
  foo() %as% 2
  foo() %as% 1
  seal(foo)

  act.1 <- foo()
  act.2 <- foo(5)

  (act.1 == 1)
  (act.2 == 5)
})

