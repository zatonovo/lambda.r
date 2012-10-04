context("example - factorial")

test_that("factorial without types", {
  fac(1) %as% 1
  fac(n) %when% { n > 0 } %as% { n * fac(n - 1) }

  expect_that(fac(1), equals(1))
  expect_that(fac(5), equals(120))
  expect_that(fac(0), throws_error())
})

test_that("factorial with types", {
  WholeNumber(x) %when% { x > 0 } %as% x

  fac(n) %::% numeric : WholeNumber
  fac(n) %as% fac(WholeNumber(n))

  fac(n) %::% WholeNumber : WholeNumber
  fac(1) %as% WholeNumber(1)
  fac(n) %when% { n > 0 } %as% { n * fac(n - 1) }

  expect_that(fac(WholeNumber(1)), equals(WholeNumber(1)))
  expect_that(fac(WholeNumber(5)), equals(WholeNumber(120)))
  expect_that(fac(1), equals(WholeNumber(1)))
  expect_that(fac(5), equals(WholeNumber(120)))
  expect_that(fac(0), throws_error())
})
