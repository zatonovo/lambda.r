rm(list=ls())

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

assert('parse_transforms_2', {
  temp <- Temperature(20, 'metric', 'celsius')
  (attr(temp,'system') == 'metric')
  (attr(temp,'units') == 'celsius')

  (! freezing(temp))
})
