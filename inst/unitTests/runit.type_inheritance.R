

test.inheritance_one_arg <- function() {
  Base(x) %as% x
  A(x) %as% { Base(x) }
  B(x) %as% { A(x) }
  E(x) %as% { Base(x) }

  one.arg(x) %::% A : character
  one.arg(x) %as% { "a" }

  one.arg(x) %::% Base : character
  one.arg(x) %as% { "base" }

  seal(Base)
  seal(A)
  seal(B)
  seal(E)
  seal(one.arg)

  a <- A(1)
  b <- B(2)
  c <- E(3)
  act.a <- one.arg(a)
  checkEquals(act.a, "a")
  act.b <- one.arg(b)
  checkEquals(act.b, "a")
  act.c <- one.arg(c)
  checkEquals(act.c, "base")
}

test.inheritance_two_arg <- function() {
  Base(x) %as% x
  A(x) %as% { Base(x) }
  B(x) %as% { A(x) }
  E(x) %as% { Base(x) }

  two.arg(x,y) %::% A : B : character
  two.arg(x,y) %as% { "a" }

  two.arg(x,y) %::% Base : Base : character
  two.arg(x,y) %as% { "base" }

  seal(Base)
  seal(A)
  seal(B)
  seal(E)
  seal(two.arg)

  a <- A(1)
  b <- B(2)
  c <- E(3)
  act.a <- two.arg(a,b)
  checkEquals(act.a, "a")
  act.b <- two.arg(b,b)
  checkEquals(act.b, "a")
  act.c <- two.arg(c,b)
  checkEquals(act.c, "base")
}


test.inheritance_with_type_variable <- function() {
  Base(x) %as% x
  A(x) %as% { Base(x) }
  B(x) %as% { A(x) }
  E(x) %as% { Base(x) }

  two.arg(x,y) %::% a : B : character
  two.arg(x,y) %as% { "a" }

  two.arg(x,y) %::% Base : Base : character
  two.arg(x,y) %as% { "base" }

  seal(Base)
  seal(A)
  seal(B)
  seal(E)
  seal(two.arg)

  a <- A(1)
  b <- B(2)
  c <- E(3)
  act.a <- two.arg(a,b)
  checkEquals(act.a, "a")
  act.b <- two.arg(b,b)
  checkEquals(act.b, "a")
  act.c <- two.arg(c,b)
  checkEquals(act.c, "a")
}


test.inheritance_with_ellipsis_1 <- function() {
  Base(x, ...) %as% list(x=x, ...)
  A(x, z) %as% { Base(x, z=z) }

  seal(Base)
  seal(A)

  a <- A(1, 2)
  checkEquals(a$x, 1)
  checkEquals(a$z, 2)
}

test.inheritance_with_ellipsis_2 <- function() {
  Base(x=1, ...) %as% list(x=x, ...)
  A(z) %as% { Base(z=z) }

  seal(Base)
  seal(A)

  a <- A(2)
  checkEquals(a$x, 1)
  checkEquals(a$z, 2)
}
