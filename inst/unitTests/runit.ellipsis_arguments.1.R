Prices(series, asset.class, periodicity) %as% 
{
  series@asset.class <- asset.class
  series@periodicity <- periodicity
  series@visualize <- TRUE
  series
}

visualize(x, ...) %when% {
  x@visualize == TRUE
} %as% {
  plot(x, ...)
}
seal(Prices)
seal(visualize)

test.ellipsis_arguments_1 <- function() {
  ps <- Prices(rnorm(50), 'equity', 'daily')
  visualize(ps, main='Prices', xlab='time')

  scatter <- matrix(rnorm(200), ncol=2)
  act <- tryCatch(visualize(scatter), error=function(x) 'error')
  checkEquals(act, 'error')

  attr(scatter,'visualize') <- TRUE
  visualize(scatter)

  visualize(scatter, main='random')
}
