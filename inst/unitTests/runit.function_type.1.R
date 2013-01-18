test.function_type_1 <- function() {
  seq.gen(start) %::% a : Function
  seq.gen(start) %as%
  {
    value <- start - 1
    function() {
      value <<- value + 1
      return(value)
    }
  }
  seal(seq.gen)

  act <- seq.gen(1)
  checkTrue('function' %in% class(act))
}
