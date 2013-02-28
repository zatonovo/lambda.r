test.zero <- function() {
  zero() %::% Function
  zero() %as% { function() 1 }

  act <- zero()
  checkEquals(act(), 1)
}

test.one_application <- function() {
  fn.0 <- function() 0

  one_application(x) %::% Function : numeric
  one_application(x) %as% { x() }

  act <- one_application(fn.0)
  checkEquals(act, 0)
}

test.one_identity <- function() {
  fn.0 <- function() 0

  one_identity(x) %::% Function : Function
  one_identity(x) %as% { x }

  act <- one_identity(fn.0)
  checkEquals(act, fn.0)
}

test.two_application <- function() {
  fn.1 <- function(x) x

  two_application(x,y) %::% Function : numeric : numeric
  two_application(x,y) %as% { x(y) }

  two_application(y,x) %::% numeric : Function : numeric
  two_application(y,x) %as% { x(y) }

  act <- two_application(fn.1,2)
  checkEquals(act, 2)

  act <- two_application(4,fn.1)
  checkEquals(act, 4)
}

test.two_identity <- function() {
  fn.0 <- function() 0
  fn.1 <- function(x) x

  two_identity(x,y) %::% Function : numeric : Function
  two_identity(x,y) %as% { x }

  two_identity(y,x) %::% numeric : Function : Function
  two_identity(y,x) %as% { x }

  act <- two_identity(fn.0, 1)
  checkEquals(act, fn.0)

  act <- two_identity(2, fn.1)
  checkEquals(act, fn.1)
}

