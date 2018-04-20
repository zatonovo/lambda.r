rm(list=ls())

assert('function_args_1', {
  f() %as% 1
  seal(f)
  act <- f()
  (act ==1)
})

assert('function_args_2', {
  f() %::% numeric
  f() %as% 1
  seal(f)
  act <- f()
  (act ==1)
})

assert('function_args_3', {
  f() %::% numeric
  f() %as% 1
  f(a) %::% numeric : numeric
  f(a) %as% a
  seal(f)

  act <- f()
  (act ==1)
  act <- f(3)
  (act ==3)
})

assert('function_args_4', {
  f() %::% numeric
  f() %:=% 1
  f(a) %::% numeric : numeric
  f(a) %:=% a
  seal(f)

  act <- f()
  (act ==1)
  act <- f(3)
  (act ==3)
})

