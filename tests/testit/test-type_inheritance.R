

rm(list=ls())
assert('inheritance_one_arg', {
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
  (act.a == "a")
  act.b <- one.arg(b)
  (act.b == "a")
  act.c <- one.arg(c)
  (act.c == "base")
})


rm(list=ls())
assert('inheritance_two_arg', {
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
  (act.a == "a")
  act.b <- two.arg(b,b)
  (act.b == "a")
  act.c <- two.arg(c,b)
  (act.c == "base")
})


rm(list=ls())
assert('inheritance_with_type_variable', {
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
  (act.a == "a")
  act.b <- two.arg(b,b)
  (act.b == "a")
  act.c <- two.arg(c,b)
  (act.c == "a")
})


rm(list=ls())
assert('inheritance_with_ellipsis_1', {
  Base(x, ...) %as% list(x=x, ...)
  A(x, z) %as% { Base(x, z=z) }

  seal(Base)
  seal(A)

  a <- A(1, 2)
  (a$x == 1)
  (a$z == 2)
})


rm(list=ls())
assert('inheritance_with_ellipsis_2', {
  Base(x=1, ...) %as% list(x=x, ...)
  A(z) %as% { Base(z=z) }

  seal(Base)
  seal(A)

  a <- A(2)
  (a$x == 1)
  (a$z == 2)
})
