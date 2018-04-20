rm(list=ls())
assert('type_integer_1', {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  (act == 3)
})

rm(list=ls())
assert('type_integer_2', {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(as.integer(3))
  (act == 3)
})

rm(list=ls())
assert('type_integer_5', {
  fib(n) %::% numeric : numeric
  fib(0) %as% as.integer(1)
  fib(1) %as% as.integer(1)
  fib(n) %as% { as.integer(fib(n-1) + fib(n-2)) }
  seal(fib)

  act <- fib(as.integer(3))
  (act == 3)
})

rm(list=ls())
assert('type_integer_3', {
  hypotenuse(a,b) %::% numeric : numeric : numeric
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- hypotenuse(as.integer(3),4)
  (act ==5)
})

rm(list=ls())
assert('type_integer_4', {
  hypotenuse(a,b) %::% numeric : numeric : numeric
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- hypotenuse(as.integer(3), as.integer(4))
  (act ==5)
})

