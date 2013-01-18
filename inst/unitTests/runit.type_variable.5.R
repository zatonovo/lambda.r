test.type_variable_5 <- function() {
  hypotenuse(a,b) %::% a : b : a
  hypotenuse(a,b) %as% { (a^2 + b^2)^.5 }
  seal(hypotenuse)

  act <- tryCatch(hypotenuse(5,12), error=function(x) 'error')
  checkEquals(act, 'error')
}

