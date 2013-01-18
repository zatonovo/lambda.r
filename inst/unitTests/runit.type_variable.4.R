test.type_variable_4 <- function() {
  hypotenuse(a,b) %::% a : a : a
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  #act <- tryCatch(f(2), error=function(x) 'error')
  #checkEquals(act, 'error')
  act <- hypotenuse(3,4)
  checkEquals(act,5)
}

