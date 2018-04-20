rm(list=ls())

WishartModel(n,m,Q,sd) %as% {
  x <- list()
  x@n <- n
  x@m <- m
  x@Q <- Q
  x@sd <- sd
  x
}

WishartMatrix(x, model) %as% {
  x@n <- model@n
  x@m <- model@m
  x@Q <- model@Q
  x@sd <- model@sd
  x
}


assert('parse_transforms_3', {
  model <- WishartModel(10,20,2,1)
  mat <- WishartMatrix(rnorm(10), model)

  (attr(mat,'n') == 10)
  (attr(mat,'m') == 20)
  (attr(mat,'Q') == 2)
  (attr(mat,'sd') == 1)
})
