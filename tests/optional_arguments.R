Prices(series, asset.class='equity', periodicity='daily') %as% 
{
  series@asset.class <- asset.class
  series@periodicity <- periodicity
  series
}


ps <- Prices(rnorm(50))                        # OK
ps <- Prices(rnorm(50), 'fx')                  # OK
ps <- Prices(rnorm(50), periodicity='monthly') # OK
ps <- Prices(periodicity='monthly', series=rnorm(50)) # OK

returns(x) %when% {
  x@asset.class == "equity"
  x@periodicity == "daily"
} %as% {
  x[2:length(x)] / x[1:(length(x) - 1)] - 1
}

returns(ps)

##############################################################################
Temperature(x, system="metric", units='celsius') %as%
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

freezing(x) %when% {
  x@system == 'metric'
  x@units == 'kelvin'
} %as% {
  if (x < 273) { TRUE }
  else { FALSE }
}

ctemp <- Temperature(20)
freezing(ctemp)

ktemp <- Temperature(20, units='kelvin')
freezing(ktemp)



