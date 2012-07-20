f(x) %when% { x@first == "benjamin" } %as% x

# Currently fails
f(x) %when% {
  x@first == 'benjamin'
  x@last == 'franklin'
} %as% x
