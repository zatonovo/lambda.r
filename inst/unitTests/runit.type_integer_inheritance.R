test.type_integer_1 <- function() {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
}

test.type_integer_2 <- function() {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- fib(as.integer(3))
  checkEquals(act, 3)
}

test.type_integer_5 <- function() {
  fib(n) %::% numeric : numeric
  fib(0) %as% as.integer(1)
  fib(1) %as% as.integer(1)
  fib(n) %as% { as.integer(fib(n-1) + fib(n-2)) }
  seal(fib)

  act <- fib(as.integer(3))
  checkEquals(act, 3)
}

test.type_integer_3 <- function() {
  hypotenuse(a,b) %::% numeric : numeric : numeric
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- hypotenuse(as.integer(3),4)
  checkEquals(act,5)
}

test.type_integer_4 <- function() {
  hypotenuse(a,b) %::% numeric : numeric : numeric
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- hypotenuse(as.integer(3), as.integer(4))
  checkEquals(act,5)
}

