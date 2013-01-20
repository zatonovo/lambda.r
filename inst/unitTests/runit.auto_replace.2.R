test.auto_replace_2a <- function() {
  fib(n) %::% numeric : numeric
  fib(0) %as% 2
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) - fib(n-2) }
  fib(n) %as% { fib(n-1) + fib(n-2) }

  fib(n) %::% character : numeric
  fib(n) %as% { fib(as.numeric(n)) }

  fib(n) %::% numeric : numeric
  fib(0) %as% 1
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
  act <- fib("3")
  checkEquals(act, 3)
}

# This is currently failing due to type indices not matching properly
# with the 0 argument function
fail.auto_replace_2b <- function() {
  fib() %::% numeric
  fib() %as% 3

  fib(n) %::% numeric : numeric
  fib(0) %as% 2
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) - fib(n-2) }
  fib(n) %as% { fib(n-1) + fib(n-2) } 
  fib(n) %::% character : numeric
  fib(n) %as% { fib(as.numeric(n)) }

  fib(n) %::% numeric : numeric
  fib(0) %as% 1

  fib() %as% 5
  seal(fib)

  act <- fib(3)
  checkEquals(act, 3)
  act <- fib("3")
  checkEquals(act, 3)
  act <- fib()
  checkEquals(act, 5)
}

