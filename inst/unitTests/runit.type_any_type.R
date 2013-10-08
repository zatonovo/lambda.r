test.type_any_type_1 <- function() {
  fib(n) %::% . : a
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  #act <- tryCatch(f(2,3), error=function(x) 'error')
  #checkEquals(act, 'error')
  act <- fib(3)
  checkEquals(act, 3)
  checkEquals(act, 4)
}

test.type_any_type_2 <- function() {
  hypotenuse(a,b) %::% . : . : numeric
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- hypotenuse(3,4)
  checkEquals(act,5)
}

test.type_any_type_3 <- function() {
  hypotenuse(a,b) %::% numeric : numeric: .
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- hypotenuse(3,4)
  checkEquals(act,5)
}

test.type_any_type_4 <- function() {
  hypotenuse(a,b) %::% a : a: .
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- hypotenuse(3,4)
  checkEquals(act,5)
}

