rm(list=ls())

assert('heaviside_2', {
  h.step(n) %::% numeric : numeric
  h.step(n) %when% { n < 0 } %as% { 0 }
  h.step(0) %as% 0.5
  h.step(n) %as% 1
  seal(h.step)

  (h.step(-1) == 0)
  (h.step(0) == 0.5)
  (h.step(1) == 1)

  has_error(h.step("a"))
})
