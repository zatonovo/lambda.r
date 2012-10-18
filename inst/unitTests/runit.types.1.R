A(x) %as% x
B(x) %as% x

f(a,b) %::% A : B : numeric
f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + 2; unclass(z * 2) }
f(a,b) %when% { a < 0 } %as% { unclass(abs(a) + b) }
f(a,b) %as% { unclass(a + b) }

seal(A)
seal(B)
seal(f)

test.types_1 <- function() {
  act.1 <- tryCatch(f(2,3), error=function(x) 'error')
  checkEquals(act.1, 'error')
  a <- A(2)
  b <- B(3)
  act.2 <- f(a,b)
  checkEquals(act.2, 5)
}
