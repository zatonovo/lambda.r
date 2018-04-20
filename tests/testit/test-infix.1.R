rm(list=ls())

assert('infix.1', {
  a %mod% b %:=% { a %/% b }
  seal(`%mod%`)

  act <- 5 %mod% 2
  exp <- 5 %/% 2
  (act == exp)
})

assert('infix.2', {
  a %mod% b %as% { a %/% b }
  seal(`%mod%`)

  act <- 5 %mod% 2
  exp <- 5 %/% 2
  (act == exp)
})

