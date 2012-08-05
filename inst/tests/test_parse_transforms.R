context("parse transforms")

test_that("parse transforms 1", {
  Prices(series) %as% 
  {
    series@asset.class <- 'equity'
    series@periodicity <- 'daily'
    series
  }

  ps <- Prices(rnorm(50))
  expect_that(attr(ps,'asset.class') == 'equity', is_true())
  expect_that(attr(ps,'periodicity') == 'daily', is_true())

  returns(x) %when% {
    x@asset.class == "equity"
    x@periodicity == "daily"
  } %as% {
    x[2:length(x)] / x[1:(length(x) - 1)] - 1
  }

  expect_that(length(returns(ps)) == length(ps) - 1, is_true())
})


test_that("parse transforms 2", {
  Temperature(x, system, units) %as%
  {
    x@system <- system
    x@units <- units
    x
  }

  freezing(x) %when% {
    x@system == 'metric'
    x@units == 'celsius'
  } %as% {
    if (x < 0) { TRUE }
    else { FALSE }
  }

  temp <- Temperature(20, 'metric', 'celsius')
  expect_that(attr(temp,'system') == 'metric', is_true())
  expect_that(attr(temp,'units') == 'celsius', is_true())

  expect_that(freezing(temp), is_false())
})

