assert('heaviside_1', {
  h.step(n) %when% { n < 0 } %as% { 0 }
  h.step(0) %as% 0.5
  h.step(n) %as% 1
  seal(h.step)

  (h.step(-1) == 0)
  (h.step(0) == 0.5)
  (h.step(1) == 1)
})
