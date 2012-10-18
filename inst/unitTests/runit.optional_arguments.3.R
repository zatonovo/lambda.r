test.optional_arguments_3 <- function() {
  avg(x, fun=mean) %as% { fun(x) }

  a <- 1:4
  a.mean <- avg(a)
  checkEquals(a.mean, 2.5)

  a.med <- avg(a, median)
  checkEquals(a.med, 2.5)
}
