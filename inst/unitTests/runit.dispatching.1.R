test.dispatching_1a <- function() {
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

test.dispatching_1b <- function() {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act.1 <- fib(5)
  checkEquals(act.1, 8)

  act.2 <- tryCatch(fib("a"), error=function(x) 'error')
  checkEquals(act.2, 'error')
}

test.dispatching_1c <- function() {
  Integer(x) %as% x

  fib(n) %::% Integer : Integer
  fib(0) %as% Integer(1)
  fib(1) %as% Integer(1)
  fib(n) %as% { Integer(fib(n-1) + fib(n-2)) }
  seal(Integer)
  seal(fib)

  checkEquals(fib(Integer(5)), Integer(8))

  act <- tryCatch(fib(5), error=function(x) 'error')
  checkEquals(act, 'error')
}

test.dispatching_1d <- function() {
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

  checkEquals(abs_max(2,-3), 3)
  checkEquals(abs_max("3","-4"), 4)

  a <- c(1,2,5,6,3,2,1,3)
  checkEquals(abs_max(a), 6)
}


test.different_names <- function() {
  A(a) %as% { list(a=a) }
  A(b) %as% { list(b=b) }
  seal(A)

  checkEquals(A(5)$a, 5)
  checkEquals(A(a=5)$a, 5)
  checkEquals(A(b=5)$b, 5)
}

test.empty_function <- function() {
  a() %as% {  }
  seal(a)

  b(a) %as% {  }
  seal(b)

  # Empty functions will fail
  checkException(a(), NULL)
  checkException(b(1), NULL)
}

test.empty_type_constructor <- function() {
  A() %as% {  }
  seal(A)

  B(a) %as% {  }
  seal(B)

  # Empty functions will fail
  checkException(A(), NULL)
  checkException(B(1), NULL)
}

