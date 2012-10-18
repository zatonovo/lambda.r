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

test.types_2.1 <- function() {
  point.1 <- Point(2,3)
  point.2 <- Point(5,7)
  checkEquals(distance(point.1,point.2), 5)
}

test.types_2.2 <- function() {
  point.3 <- Polar(3,pi/2)
  point.4 <- Polar(4,pi)
  checkEquals(distance(point.3,point.4), 5)
}
