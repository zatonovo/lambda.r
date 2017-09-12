test.function_args_1 <- function() {
  f() %as% 1
  seal(f)
  act <- f()
  checkEquals(act,1)
}

test.function_args_2 <- function() {
  f() %::% numeric
  f() %as% 1
  seal(f)
  act <- f()
  checkEquals(act,1)
}

test.function_args_3 <- function() {
  f() %::% numeric
  f() %as% 1
  f(a) %::% numeric : numeric
  f(a) %as% a
  seal(f)

  act <- f()
  checkEquals(act,1)
  act <- f(3)
  checkEquals(act,3)
}

test.function_args_4 <- function() {
  f() %::% numeric
  f() %:=% 1
  f(a) %::% numeric : numeric
  f(a) %:=% a
  seal(f)

  act <- f()
  checkEquals(act,1)
  act <- f(3)
  checkEquals(act,3)
}

