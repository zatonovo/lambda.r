context("dispatching")

is.wholenumber <- function(x, tol=.Machine$double.eps^0.5)
  abs(x - round(x)) < tol

fib(0) %as% 1
fib(1) %as% 1
fib(n) %when% {
  is.wholenumber(n)
} %as% {
  fib(n-1) + fib(n-2)
}
seal(fib)

expect_that(fib(5), equals(8))

Integer(x) %as% x

fib.a(n) %::% Integer : Integer
fib.a(0) %as% Integer(1)
fib.a(1) %as% Integer(1)
fib.a(n) %as% { Integer(fib(n-1) + fib(n-2)) }
seal(fib.a)

expect_that(fib.a(Integer(5)), equals(Integer(8)))
expect_that(fib.a(5), throws_error())

##############################################################################
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
 
