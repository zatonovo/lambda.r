# For some reason these variables have to be declared explicitly and globally, 
# otherwise the scoping within RUnit breaks the tests.
absdiff <<- function(...) UseFunction('absdiff', ...)

absdiff %when% (is.numeric(a) && is.numeric(b))
absdiff %also% (a > b)
absdiff %as% function(a, b) a - b

absdiff %when% (is.numeric(a) && is.numeric(b))
absdiff %also% (a < b)
absdiff %as% function(a, b) b - a

absdiff %when% (! is.numeric(a) || ! is.numeric(b))
absdiff %as% function(a, b) absdiff(as.double(a), as.double(b))

# This converts o integers. This will be called if c and 
# optionally d is named
absdiff %when% TRUE
absdiff %as% function(c, d) absdiff(as.integer(c), as.integer(d))

# This uses log vlues. Note that this will only be executed if
# l is explicitly named (and all preceding 2 argument functions
# are strict).
absdiff %when% TRUE
absdiff %as% function(a, l) absdiff(log(a), log(l))

absdiff %when% (a > 5)
absdiff %as% function(a) a * 2

absdiff %when% TRUE
absdiff %as% function(a) a + 1

cat("Objects after definitions:\n")
cat(ls())
cat("\n")

# This doesn't work either even though absdiff was created??
#cat("Prepopulating results\n")
#result.1 <<- absdiff(18,6)
#result.2 <<- absdiff(6,18)
#result.21.a <<- absdiff('6', 18)
#result.21.b <<- absdiff(6, '18')
#result.21.c <<- absdiff('6', '18')
#cat("Done prepopulating\n")

## TESTS
# Use this construction to get around runit environment cleaning. This way we
# actually test that the parent function is being defined and the concrete 
# ones are being attached properly.
test.absdiff.1 <- function()
{
  checkTrue(12 == absdiff(18,6))
  #checkTrue(12 == result.1)
}

test.absdiff.2 <- function()
{
  checkTrue(12 == absdiff(6,18))
  #checkTrue(12 == result.2)
}

test.absdiff.21 <- function()
{
  checkTrue(12 == absdiff('6', 18))
  checkTrue(12 == absdiff(6,'18'))
  checkTrue(12 == absdiff('6','18'))
  #checkTrue(12 == result.21.a)
  #checkTrue(12 == result.21.b)
  #checkTrue(12 == result.21.c)
}

test.absdiff.3 <- function()
{
  checkTrue(12 == absdiff(6))
}

test.absdiff.4 <- function()
{
  checkTrue(6 == absdiff(5))
  checkTrue(0 == absdiff(-1))
}

test.absdiff.int <- function()
{
  checkTrue(12 == absdiff(c=3.5, 15.1))
  checkTrue(12 == absdiff(c=3.5, d=15.1))
  checkTrue(12 == absdiff(d=3.5, c=15.1))
}

test.absdiff.int.x <- function()
{
  cat("Expecting exception\n")
  checkException(12 == absdiff(c=3.5, a=15.1))
}

test.absdiff.log <- function()
{
  checkTrue(1.461932 - absdiff(3.5, l=15.1) < 0.000001)
  checkTrue(1.461932 - absdiff(l=3.5, 15.1) < 0.000001)
}


## DEFINITIONS FOR ADVANCED GUARDS (INCOMPLETE)
interpolate <<- function(...) UseFunction('interpolate',...)

interpolate %when% (cfg %isa% 'linear')
interpolate %as% function(cfg, a,b) { }


## TESTS FOR ENSURE
logarithm <<- function(...) UseFunction('logarithm',...)

logarithm %when% is.numeric(x)
logarithm %must% (! is.nan(result) && ! is.infinite(result))
logarithm %as% function(x) logarithm(x, exp(1))

logarithm %when% TRUE
logarithm %as% function(x) logarithm(as.numeric(x))

logarithm %when% (is.numeric(x) && is.numeric(y))
logarithm %as% function(x,y) log(x, base=y)

logarithm %when% TRUE
logarithm %as% function(x,y) logarithm(as.numeric(x), as.numeric(y))


test.logarithm.int <<- function()
{
  checkTrue(0 == logarithm(1,5))
  checkTrue(3 == logarithm(y=2, 8))
}

test.logarithm.neg <<- function()
{
  cat("Expecting failed assertion\n")
  checkException(logarithm(-1))
}

test.logarithm.paths <<- function()
{
  # Assertion is on logarithm.1
  checkException(logarithm(-1))
  # But not on logarithm.base
  checkTrue(is.infinite(logarithm(5,1)))
}


