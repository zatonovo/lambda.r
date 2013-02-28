# vim: set filetype=R

test.types_1 <- function() {
  A(x) %as% x
  B(x) %as% x

  f(a,b) %::% A : B : numeric
  f(a,0) %when% { a < 5; a > 0 } %as% { z <- a + 2; unclass(z * 2) }
  f(a,b) %when% { a < 0 } %as% { unclass(abs(a) + b) }
  f(a,b) %as% { unclass(a + b) }

  seal(A)
  seal(B)
  seal(f)

  act.1 <- tryCatch(f(2,3), error=function(x) 'error')
  cat("[test.types_1] act.1 =",act.1,"\n")
  checkEquals(act.1, 'error')
  a <- A(2)
  b <- B(3)
  act.2 <- f(a,b)
  checkEquals(act.2, 5)
}


test.types_2.1 <- function() {
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

  point.1 <- Point(2,3)
  point.2 <- Point(5,7)
  checkEquals(distance(point.1,point.2), 5)
}

test.types_2.2 <- function() {
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

  point.3 <- Polar(3,pi/2)
  point.4 <- Polar(4,pi)
  checkEquals(distance(point.3,point.4), 5)
}
