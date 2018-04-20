rm(list=ls())

assert('parse_transforms_1', {
  Prices(series) %as% 
  {
    series@asset.class <- 'equity'
    series@periodicity <- 'daily'
    series
  }

  returns(x) %when% {
    x@asset.class == "equity"
    x@periodicity == "daily"
  } %as% {
    x[2:length(x)] / x[1:(length(x) - 1)] - 1
  }

  ps <- Prices(rnorm(50))
  (attr(ps,'asset.class') == 'equity')
  (attr(ps,'periodicity') == 'daily')

  rs <- returns(ps)
  (length(rs) == length(ps) - 1)
})
