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

test.parse_transforms_2 <- function() {
  temp <- Temperature(20, 'metric', 'celsius')
  checkEquals(attr(temp,'system'), 'metric')
  checkEquals(attr(temp,'units'), 'celsius')

  checkTrue(! freezing(temp))
}
