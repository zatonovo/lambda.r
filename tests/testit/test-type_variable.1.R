rm(list=ls())
test.type_variable_1 <- function() {
  fib(n) %::% a : a
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  #act <- tryCatch(f(2,3), error=function(x) 'error')
  #checkEquals(act, 'error')
  act <- fib(3)
  (act == 3)
}

rm(list=ls())
ignore.type_variable_2 <- function() {
  fib(n) %::% b : a
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- tryCatch(f(2), error=function(x) 'error')
  #cat("\ntest.type_variable_2: act =",act,"\n")
  ('error' == act)
}

rm(list=ls())
ignore.type_variable_3 <- function() {
  fib(n) %::% a : b
  fib(0) %as% 1
  fib(1) %as% 1
  fib(n) %as% { fib(n-1) + fib(n-2) }
  seal(fib)

  act <- tryCatch(f(2), error=function(x) 'error')
  ('error' == act)
}

rm(list=ls())
test.type_variable_4 <- function() {
  hypotenuse(a,b) %::% a : a : a
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  #act <- tryCatch(f(2), error=function(x) 'error')
  #checkEquals(act, 'error')
  act <- hypotenuse(3,4)
  (act ==5)
}

rm(list=ls())
test.type_variable_5 <- function() {
  hypotenuse(a,b) %::% a : b : a
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- tryCatch(hypotenuse(5,12), error=function(x) 'error')
  (act == 'error')
}

rm(list=ls())
test.type_variable_6 <- function() {
  hypotenuse(a,b) %::% a : a : b
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- tryCatch(hypotenuse(5,12), error=function(x) 'error')
  (act == 'error')
}

rm(list=ls())
test.mixed_type_variable_1 <- function() {
  Point(x,y) %as% list(x=x,y=y)
  distance(a,b) %::% Point : Point : z 
  distance(a,b) %as% { ((a$x - b$x)^2 + (a$y - b$y)^2)^.5 }
  seal(distance)

  point.1 <- Point(2, 2)
  point.2 <- Point(1, 1)

  act <- distance(point.1, point.2)
  (act == sqrt(2))
}
