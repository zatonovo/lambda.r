assert('auto_replace.types_2a', {
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

  act.1 <- fib(3)
  act.2 <- fib("3")

  (act.1 == 3)
  (act.2 == 3)
})

assert('auto_replace.types_2b', {
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

  act.1 <- fib(3)
  act.2 <- fib("3")
  act.3 <- fib()

  (act.1 == 3)
  (act.2 == 3)
  (act.3 == 5)
})

assert('auto_replace.types_2c', {
  fib() %::% numeric
  fib() %as% 3
  fib() %as% 5
  seal(fib)

  act <- fib()
  act == 5
})

test.auto_replace.types_2d <- function() {
  fib() %::% numeric
  fib() %as% 3

  fib(n) %::% numeric : numeric
  fib(n) %as% n

  fib() %as% 5
  seal(fib)

  act.1 <- fib()
  act.2 <- fib(4)

  (act.1 == 5)
  (act.2 == 4)
}

