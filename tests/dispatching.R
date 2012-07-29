cat("Test 1\n")
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

fib(0) %as% 1
fib(1) %as% 1
fib(n) %when% {
  is.wholenumber(n)
} %as% {
  fib(n-1) + fib(n-2)
}
seal(fib)

expect_that(fib(5), equals(8))

cat("Test 2\n")
Integer(x) %as% x

fib.a(n) %::% Integer : Integer
fib.a(0) %as% Integer(1)
fib.a(1) %as% Integer(1)
fib.a(n) %as% { Integer(fib(n-1) + fib(n-2)) }
seal(fib.a)

expect_that(fib.a(Integer(5)), equals(Integer(8)))
expect_that(fib.a(5), throws_error())
