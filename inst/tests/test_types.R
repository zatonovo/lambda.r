context("types")

test_that("simple types", {
  f(a,b) %::% A : B : numeric
  f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + 2; unclass(z * 2) }
  f(a,b) %when% { a < 0 } %as% { unclass(abs(a) + b) }
  f(a,b) %as% { unclass(a + b) }

  A(x) %as% x
  B(x) %as% x

  expect_that(f(2,3), throws_error())
  a <- A(2)
  b <- B(2)
  expect_that(f(a,b) == 5, is_true())
})

test_that("types with multipart definitions", {
  Point(x,y) %as% list(x=x,y=y)
  Polar(r,theta) %as% list(r=r,theta=theta)

  distance(a,b) %::% Point : Point : numeric
  distance(a,b) %as% { ((a$x - b$x)^2 + (a$y - b$y)^2)^.5 } 

  distance(a,b) %::% Polar : Polar : numeric

  distance(a,b) %as%
  {
    (a$r^2 + b$r^2 - 2 * a$r * b$r * cos(a$theta - b$theta))^.5
  }

  point.1 <- Point(2,3)
  point.2 <- Point(5,7)
  expect_that(distance(point.1,point.2) == 5, is_true())

  point.3 <- Polar(3,pi/2)
  point.4 <- Polar(4,pi)
  expect_that(distance(point.3,point.4) == 5, is_true())
})


