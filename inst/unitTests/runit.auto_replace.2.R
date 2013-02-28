test.auto_replace.types_2a <- function() {
  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 2
  fib(n) %as% { fib(n-1) - fib(n-2) }
  fib(n) %as% { fib(n-1) + fib(n-2) }

  fib(n) %::% character : numeric
  fib(n) %as% { fib(as.numeric(n)) }

  fib(n) %::% numeric : numeric
  fib(1) %as% 1
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
  act <- fib("3")
  checkEquals(act, 3)
}

test.auto_replace.types_2b <- function() {
  fib() %::% numeric
  fib() %as% 3

  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  fib(1) %as% 2
  fib(n) %as% { fib(n-1) - fib(n-2) }
  fib(n) %as% { fib(n-1) + fib(n-2) } 
  fib(n) %::% character : numeric
  fib(n) %as% { fib(as.numeric(n)) }

  fib(n) %::% numeric : numeric
  fib(1) %as% 1

  fib() %as% 5
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
  act <- fib("3")
  checkEquals(act, 3)
  act <- fib()
  checkEquals(act, 5)
}

test.auto_replace.types_2c <- function() {
  fib() %::% numeric
  fib() %as% 3
  fib() %as% 5
  seal(fib)

  act <- fib()
  checkEquals(act, 5)
}

test.auto_replace.types_2d <- function() {
  fib() %::% numeric
  fib() %as% 3

  fib(n) %::% numeric : numeric
  fib(n) %as% n

  fib() %as% 5
  seal(fib)

  act <- fib()
  checkEquals(act, 5)
  act <- fib(4)
  checkEquals(act, 4)
}

