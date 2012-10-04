context("optional arguments")

test_that("optional arguments with guards 1", {
  Prices(series, asset.class='equity', periodicity='daily') %as% 
  {
    series@asset.class <- asset.class
    series@periodicity <- periodicity
    series
  }

  returns(x) %when% {
    x@asset.class == "equity"
    x@periodicity == "daily"
  } %as% {
    x[2:length(x)] / x[1:(length(x) - 1)] - 1
  }

  ps <- Prices(abs(rnorm(50)))
  expect_that(attr(ps,'asset.class'), equals('equity'))
  expect_that(attr(ps,'periodicity'), equals('daily'))

  ps <- Prices(abs(rnorm(50)), 'fx')
  expect_that(attr(ps,'asset.class'), equals('fx'))
  expect_that(attr(ps,'periodicity'), equals('daily'))

  ps <- Prices(abs(rnorm(50)), periodicity='monthly')
  expect_that(attr(ps,'asset.class'), equals('equity'))
  expect_that(attr(ps,'periodicity'), equals('monthly'))

  ps <- Prices(periodicity='monthly', series=abs(rnorm(50)))
  expect_that(attr(ps,'asset.class'), equals('equity'))
  expect_that(attr(ps,'periodicity'), equals('monthly'))


  expect_that(returns(ps), throws_error())

  ps <- Prices(abs(rnorm(50)))
  expect_that(length(returns(ps)), equals(length(ps) - 1))
})

test_that("optional arguments with guards 2", {
  Temperature(x, system='metric', units='celsius') %as%
  {
    x@system <- system
    x@units <- units
    x
  }

  freezing(x) %::% Temperature : logical
  freezing(x) %when% {
    x@system == 'metric'
    x@units == 'celsius'
  } %as% {
    if (x < 0) { TRUE }
    else { FALSE }
  }

  freezing(x) %when% {
    x@system == 'metric'
    x@units == 'kelvin'
  } %as% {
    if (x < 273) { TRUE }
    else { FALSE }
  }

  ctemp <- Temperature(20)
  expect_that(freezing(ctemp), is_false())

  ktemp <- Temperature(20, units='kelvin')
  expect_that(freezing(ktemp), is_true())
})


test_that("optional arguments with function ref as default", {
  avg(x, fun=mean) %as% { fun(x) }

  a <- 1:4
  a.mean <- avg(a)
  expect_that(a.mean, equals(2.5))

  a.med <- avg(a, median)
  expect_that(a.med, equals(2.5))
})

