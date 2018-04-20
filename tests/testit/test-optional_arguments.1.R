# vim: set filetype=R

rm(list=ls())
assert('optional_arguments_1a', {
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
  (attr(ps,'asset.class') == 'equity')
  (attr(ps,'periodicity') == 'daily')

  ps <- Prices(abs(rnorm(50)), 'fx')
  (attr(ps,'asset.class') == 'fx')
  (attr(ps,'periodicity') == 'daily')

  ps <- Prices(abs(rnorm(50)), periodicity='monthly')
  (attr(ps,'asset.class') == 'equity')
  (attr(ps,'periodicity') == 'monthly')

  ps <- Prices(periodicity='monthly', series=abs(rnorm(50)))
  (attr(ps,'asset.class') == 'equity')
  (attr(ps,'periodicity') == 'monthly')

  err <- tryCatch(returns(ps), error=function(x) 'error')
  (err == 'error')

  ps <- Prices(abs(rnorm(50)))
  (length(returns(ps)) == length(ps) - 1)

})


rm(list=ls())
assert('optional_arguments_1b', {
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
  (! freezing(ctemp))

  ktemp <- Temperature(20, units='kelvin')
  (freezing(ktemp))
})


rm(list=ls())
assert('optional_arguments_1c', {
  avg(x, fun=mean) %as% { fun(x) }

  a <- 1:4
  a.mean <- avg(a)
  (a.mean == 2.5)

  a.med <- avg(a, median)
  (a.med == 2.5)
})
