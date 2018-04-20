rm(list=ls())

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

dummy(x, ...) %as% { list(...) }
seal(dummy)


assert('ellipsis_arguments_1', {
  ps <- Prices(rnorm(50), 'equity', 'daily')
  visualize(ps, main='Prices', xlab='time')

  scatter <- matrix(rnorm(200), ncol=2)
  act <- tryCatch(visualize(scatter), error=function(x) 'error')

  attr(scatter,'visualize') <- TRUE
  visualize(scatter)

  visualize(scatter, main='random')

  (act == 'error')
})

assert('ellipsis_unnamed_arguments 1', {
  act <- dummy(1,2)
  all.equal(act, list(2))
})

assert('ellipsis_unnamed_arguments 2', {
  act <- dummy(1,2,3,4)
  all.equal(act, list(2,3,4))
})

