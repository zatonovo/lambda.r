test.optional_arguments_2 <- function() {
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
  checkTrue(! freezing(ctemp))

  ktemp <- Temperature(20, units='kelvin')
  checkTrue(freezing(ktemp))
}
