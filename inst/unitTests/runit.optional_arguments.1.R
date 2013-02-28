# vim: set filetype=R

test.optional_arguments_1a <- function() {
  Prices(series, asset.class='equity', periodicity='daily') %as% {
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
  seal(Prices)
  seal(returns)

  ps <- Prices(abs(rnorm(50)))
  checkEquals(attr(ps,'asset.class'), 'equity')
  checkEquals(attr(ps,'periodicity'), 'daily')

  ps <- Prices(abs(rnorm(50)), 'fx')
  checkEquals(attr(ps,'asset.class'), 'fx')
  checkEquals(attr(ps,'periodicity'), 'daily')

  ps <- Prices(abs(rnorm(50)), periodicity='monthly')
  checkEquals(attr(ps,'asset.class'), 'equity')
  checkEquals(attr(ps,'periodicity'), 'monthly')

  ps <- Prices(periodicity='monthly', series=abs(rnorm(50)))
  checkEquals(attr(ps,'asset.class'), 'equity')
  checkEquals(attr(ps,'periodicity'), 'monthly')

  err <- tryCatch(returns(ps), error=function(x) 'error')
  checkEquals(err, 'error')

  ps <- Prices(abs(rnorm(50)))
  checkEquals(length(returns(ps)), length(ps) - 1)

}


test.optional_arguments_1b <- function() {
  Temperature(x, system='metric', units='celsius') %as% {
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
  seal(Temperature)
  seal(freezing)

  ctemp <- Temperature(20)
  checkTrue(! freezing(ctemp))

  ktemp <- Temperature(20, units='kelvin')
  checkTrue(freezing(ktemp))
}


test.optional_arguments_1c <- function() {
  avg(x, fun=mean) %as% { fun(x) }

  a <- 1:4
  a.mean <- avg(a)
  checkEquals(a.mean, 2.5)

  a.med <- avg(a, median)
  checkEquals(a.med, 2.5)
}
