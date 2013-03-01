# :vim set filetype=R

test.pattern_null <- function()
{
  fold(f, x, acc) %as% acc
  fold(f, NULL, acc) %as% acc
  
  act <- fold(function(x,y) x + y, NULL, 5)
  checkEquals(5, act)
}

test.pattern_na <- function()
{
  fold(f, x, acc) %as% acc
  fold(f, NA, acc) %as% acc
  
  act <- fold(function(x,y) x + y, NA, 5)
  checkEquals(5, act)
}

test.pattern_empty <- function()
{
  fold(f, EMPTY, acc) %as% acc
  fold(f,x,acc) %as% { fold(f,x[-1], f(x[1],acc)) }
  plus <- function(x,y) x + y
  
  act <- fold(plus, 1:5, 0)
  checkEquals(15, act)
}
