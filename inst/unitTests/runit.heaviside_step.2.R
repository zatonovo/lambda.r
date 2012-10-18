test.heaviside_2 <- function() {
  h.step(n) %::% numeric : numeric
  h.step(n) %when% { n < 0 } %as% { 0 }
  h.step(0) %as% 0.5
  h.step(n) %as% 1
  seal(h.step)

  checkEquals(h.step(-1), 0)
  checkEquals(h.step(0), 0.5)
  checkEquals(h.step(1), 1)
  # TODO: This throws an error in the shell but not via RUnit
  #checkException(h.step("a"))
}
