test.dispatching_4 <- function() {
  abs_max(a,b) %::% numeric : numeric : numeric
  abs_max(a,b) %when% {
    a != b
  } %as% {
    pmax(abs(a), abs(b))
  }

  abs_max(a,b) %::% character : character : numeric
  abs_max(a,b) %as%
  {
    abs_max(as.numeric(a), as.numeric(b))
  }

  abs_max(a) %as% { max(abs(a)) }
  seal(abs_max)

  checkEquals(abs_max(2,-3), 3)
  checkEquals(abs_max("3","-4"), 4)

  a <- c(1,2,5,6,3,2,1,3)
  checkEquals(abs_max(a), 6)
}
