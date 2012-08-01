f(a,b) %::% A : B : numeric
f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + 2; z * 2 }
f(a,b) %when% { a < 0 } %as% { abs(a) + b }
f(a,b) %as% { a + b }

A(x) %as% x
B(x) %as% x

seal(f)
seal(A)
seal(B)

cat("Test 1\n")
expect_that(f(2,3), throws_error())
expect_that(f(A(2),B(3)) == 5, is_true())

Point(x,y) %as% list(x=x,y=y)
Polar(r,theta) %as% list(r=r,theta=theta)

distance(a,b) %::% Point : Point : numeric
distance(a,b) %as% { ((a$x - b$x)^2 + (a$y - b$y)^2)^.5 } 

distance(a,b) %::% Polar : Polar : numeric
distance(a,b) %as%
{
  (a$r^2 + b$r^2 - 2 * a$r * b$r * cos(a$theta - b$theta))^.5
}

seal(Point)
seal(Polar)
seal(distance)

cat("Test 2\n")
point.1 <- Point(2,3)
point.2 <- Point(5,7)
expect_that(distance(point.1,point.2) == 5, is_true())

cat("Test 3\n")
point.3 <- Polar(3,pi/2)
point.4 <- Polar(4,pi)
expect_that(distance(point.3,point.4) == 5, is_true())


