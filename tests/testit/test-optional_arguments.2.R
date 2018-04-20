# vim: set filetype=R
rm(list=ls())
assert('optional_arguments_no_args', {
  f(name='ROOT') %as% 1
  seal(f)
  (f() == 1)
  (f('a') == 1)
})

rm(list=ls())
assert('optional_arguments_no_args_type_constraint', {
  f(name) %::% character : numeric
  f(name='ROOT') %as% 1
  seal(f)
  (f() == 1)
  (f('a') == 1)
})

rm(list=ls())
assert('optional_arguments_function', {
  f(x, y=runif(5)) %as% { x + y }
  seal(f)
  act <- f(1)
  (length(act) == 5)
})

rm(list=ls())
assert('optional_arguments_function_named', {
  f(y=runif(5), x) %as% { x + y }
  seal(f)
  act <- f(x=1)
  (length(act) == 5)
})

rm(list=ls())
assert('optional_arguments_reference_var', {
  f(y=min(x), x) %as% { x + y }
  seal(f)
  act <- f(x=1:5)
  (length(act) == 5)
  (act == 2:6)
})
