test.example_1 <- function()
{
  reciprocal(x) %::% numeric : numeric
  reciprocal(x) %when% { x != 0 } %as% { 1 / x }
  reciprocal(x) %::% character : numeric
  reciprocal(x) %as% { reciprocal(as.numeric(x)) }

  act <- reciprocal(4)
  checkEquals(act, 0.25)

  act <- reciprocal("4")
  checkEquals(act, 0.25)
}
