# :vim set filetype=R


rm(list=ls())
assert('pattern_null', {
  fold(f, x, acc) %as% acc
  fold(f, NULL, acc) %as% acc
  
  act <- fold(function(x,y) x + y, NULL, 5)
  (5 == act)
})

rm(list=ls())
assert('pattern_na', {
  fold(f, x, acc) %as% acc
  fold(f, NA, acc) %as% acc
  
  act <- fold(function(x,y) x + y, NA, 5)
  (5 == act)
})

rm(list=ls())
assert('pattern_empty', {
  fold(f, EMPTY, acc) %as% acc
  fold(f,x,acc) %as% { fold(f,x[-1], f(x[1],acc)) }
  plus <- function(x,y) x + y
  
  act <- fold(plus, 1:5, 0)
  (15 == act)
})
