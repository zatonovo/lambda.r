# vim: set filetype=R

test.optional_arguments_function <- function()
{
  f(x, y=runif(5)) %as% { x + y }
  seal(f)
  act <- f(1)
  checkTrue(length(act) == 5)
}

test.optional_arguments_function_named <- function()
{
  f(y=runif(5), x) %as% { x + y }
  seal(f)
  act <- f(x=1)
  checkTrue(length(act) == 5)
}
