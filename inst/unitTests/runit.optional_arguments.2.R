# vim: set filetype=R

test.optional_arguments_no_args <- function()
{
  f(name='ROOT') %as% 1
  seal(f)
  checkEquals(f(), 1)
  checkEquals(f('a'), 1)
}

test.optional_arguments_no_args_type_constraint <- function()
{
  f(name) %::% character : numeric
  f(name='ROOT') %as% 1
  seal(f)
  checkEquals(f(), 1)
  checkEquals(f('a'), 1)
}

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

test.optional_arguments_reference_var <- function()
{
  f(y=min(x), x) %as% { x + y }
  seal(f)
  act <- f(x=1:5)
  checkTrue(length(act) == 5)
  checkEquals(act, 2:6)
}
