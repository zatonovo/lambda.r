rm(list=ls())
assert('dispatching_1a', {
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %when% {
    abs(n - round(n)) < .Machine$double.eps^0.5
  } %as% {
    fib(n-1) + fib(n-2)
  }
  seal(fib)

  fib(5) == 8
})

rm(list=ls())
assert('dispatching_1b', {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act.1 <- fib(5)
  act.2 <- tryCatch(fib("a"), error=function(x) 'error')

  (act.1 == 8)
  (act.2 == 'error')
})

rm(list=ls())
assert('dispatching_1c', {
  Integer(x) %as% x

  fib(n) %::% Integer : Integer
  fib(0) %as% Integer(1)
  fib(1) %as% Integer(1)
  fib(n) %as% { Integer(fib(n-1) + fib(n-2)) }
  seal(Integer)
  seal(fib)

  act <- tryCatch(fib(5), error=function(x) 'error')

  (fib(Integer(5)) == Integer(8))
  (act == 'error')
})

rm(list=ls())
assert('dispatching_1d', {
  abs_max(a,b) %::% numeric : numeric : numeric
  abs_max(a,b) %when% {
    a != b
  } %as% {
    pmax(abs(a), abs(b))
  }

  abs_max(a,b) %::% character : character : numeric
  abs_max(a,b) %as%
  {
    abs_max(as.numeric(a), as.numeric(b))
  }

  abs_max(a) %as% { max(abs(a)) }
  seal(abs_max)

  a <- c(1,2,5,6,3,2,1,3)

  (abs_max(2,-3) == 3)
  (abs_max("3","-4") == 4)
  (abs_max(a) == 6)
})


rm(list=ls())
assert('different_names', {
  A(a) %as% { list(a=a) }
  A(b) %as% { list(b=b) }
  seal(A)

  (A(5)$a == 5)
  (A(a=5)$a == 5)
  (A(b=5)$b == 5)
})

rm(list=ls())
assert('empty_function', {
  a() %as% {  }
  seal(a)

  b(a) %as% {  }
  seal(b)

  # Empty functions will fail
  has_error(a())
  has_error(b(1))
})

rm(list=ls())
assert('empty_type_constructor', {
  A() %as% {  }
  seal(A)

  B(a) %as% {  }
  seal(B)

  # Empty functions will fail
  has_error(A())
  has_error(B(1))
})

