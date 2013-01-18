test.type_variable_6 <- function() {
  hypotenuse(a,b) %::% a : a : b
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- tryCatch(hypotenuse(5,12), error=function(x) 'error')
  checkEquals(act, 'error')
}


