test.heaviside_1 <- function() {
  h.step(n) %when% { n < 0 } %as% { 0 }
  h.step(0) %as% 0.5
  h.step(n) %as% 1
  seal(h.step)

  checkTrue(h.step(-1) == 0)
  checkTrue(h.step(0) == 0.5)
  checkTrue(h.step(1) == 1)
}
