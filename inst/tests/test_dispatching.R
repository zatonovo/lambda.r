context("dispatching")

test_that("fibonacci with guards and no type declaration", {
  is.wholenumber <- function(x, tol=.Machine$double.eps^0.5)
    abs(x - round(x)) < tol

  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %when% {
    is.wholenumber(n)
  } %as% {
    fib(n-1) + fib(n-2)
  }

  expect_that(fib(5), equals(8))
})

test_that("fibonacci with built-in types", {
  fib(n) %::% numeric : numeric
  fib(0) %as% Integer(1)
  fib(1) %as% Integer(1)
  fib(n) %as% { Integer(fib(n-1) + fib(n-2)) }

  expect_that(fib(5), equals(8))
  expect_that(fib("a"), throws_error())
})

test_that("fibonacci with custom types", {
  Integer(x) %as% x

  fib(n) %::% Integer : Integer
  fib(0) %as% Integer(1)
  fib(1) %as% Integer(1)
  fib(n) %as% { Integer(fib(n-1) + fib(n-2)) }

  expect_that(fib(Integer(5)), equals(Integer(8)))
  expect_that(fib(5), throws_error())
})

test_that("complex guards", {
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

  expect_that(abs_max(2,-3), equals(3))
  expect_that(abs_max("3","-4"), equals(4))

  a <- c(1,2,5,6,3,2,1,3)
  expect_that(abs_max(a), equals(6))
})
