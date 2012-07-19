o <- create(Dummy, a=3, c=5)

test.hasa.single <- function()
{
  checkTrue(o %hasa% a)
  checkTrue(o %hasa% c)
  checkTrue(! o %hasa% b)
}

test.hasa.multiple <- function()
{
  checkTrue(all(c(TRUE,FALSE) == o %hasa% c(a,b) ))
  checkTrue(all(c(TRUE,TRUE) == o %hasa% c(a,c) ))
}

test.hasall.single <- function()
{
  checkTrue(o %hasall% a)
  checkTrue(o %hasall% c)
  checkTrue(! o %hasall% b)
}

test.hasall.multiple <- function()
{
  checkTrue(! o %hasall% c(a,b))
  checkTrue(o %hasall% c(a,c))
}

