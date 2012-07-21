Prices(series) %as% 
{
  series@asset.class <- 'equity'
  series@periodicity <- 'daily'
  series
}

ps <- Prices(rnorm(50))

returns(x) %when% {
  x@asset.class == "equity"
  x@periodicity == "daily"
} %as% {
  x[2:length(x)] / x[1:(length(x) - 1)] - 1
}

returns(ps)

##############################################################################
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
freezing(temp)

