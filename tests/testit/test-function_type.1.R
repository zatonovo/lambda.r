rm(list=ls())

assert('function_type_1', {
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
  ('function' %in% class(act))
})
