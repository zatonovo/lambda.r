context("example - heaviside")

test_that("heaviside step function without types", {
  h.step(n) %when% { n < 0 } %as% { 0 }
  h.step(0) %as% 0.5
  h.step(n) %as% 1

  expect_that(h.step(-1), equals(0))
  expect_that(h.step(0), equals(0.5))
  expect_that(h.step(1), equals(1))
})

test_that("heaviside step function with types", {
  h.step(n) %::% numeric : numeric
  h.step(n) %when% { n < 0 } %as% { 0 }
  h.step(0) %as% 0.5
  h.step(n) %as% 1

  expect_that(h.step(-1), equals(0))
  expect_that(h.step(0), equals(0.5))
  expect_that(h.step(1), equals(1))
  # TODO: This throws an error in the shell but not via testthat
  #expect_that(h.step("a"), throws_error())
})

