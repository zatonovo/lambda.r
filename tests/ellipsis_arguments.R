Prices(series, asset.class, periodicity) %as% 
{
  series@asset.class <- asset.class
  series@periodicity <- periodicity
  series
}

visualize(x, ...) %when% {
  x@visualize == TRUE
} %as% {
  plot(x, ...)
}

ps <- Prices(rnorm(50), 'equity', 'daily')
visualize(ps, main='Prices')
