test.optional_arguments_1 <- function() {
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
