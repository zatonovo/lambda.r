test.infix.1 <- function() {
  a %mod% b %:=% { a %/% b }
  seal(`%mod%`)

  act <- 5 %mod% 2
  exp <- 5 %/% 2
  checkEquals(act, exp)
}

test.infix.2 <- function() {
  a %mod% b %as% { a %/% b }
  seal(`%mod%`)

  act <- 5 %mod% 2
  exp <- 5 %/% 2
  checkEquals(act, exp)
}

