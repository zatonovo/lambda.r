assert('example_1', {
  reciprocal(x) %::% numeric : numeric
  reciprocal(x) %when% { x != 0 } %as% { 1 / x }
  reciprocal(x) %::% character : numeric
  reciprocal(x) %as% { reciprocal(as.numeric(x)) }

  act.1 <- reciprocal(4)
  act.2 <- reciprocal("4")

  (act.1 == 0.25)
  (act.2 == 0.25)
})
