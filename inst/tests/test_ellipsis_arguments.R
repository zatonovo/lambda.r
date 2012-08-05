context("ellipsis argument")

test_that("trailing ellipsis", {
  Prices(series, asset.class, periodicity) %as% 
  {
    series@asset.class <- asset.class
    series@periodicity <- periodicity
    series@visualize <- TRUE
    series
  }

  visualize(x, ...) %when% {
    x@visualize == TRUE
  } %as% {
    plot(x, ...)
  }
  seal(Prices)
  seal(visualize)

  ps <- Prices(rnorm(50), 'equity', 'daily')
  visualize(ps, main='Prices', xlab='time')

  scatter <- matrix(rnorm(200), ncol=2)
  expect_that(visualize(scatter), throws_error())

  attr(scatter,'visualize') <- TRUE
  visualize(scatter)

  visualize(scatter, main='random')
})

test_that("center ellipsis", {
  regress(formula, ..., na.action='na.fail') %as% {
    lm(formula, ..., na.action=na.action)
  }
  seal(regress)

  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  data <- data.frame(group=gl(2,10,20,labels=c("Ctl","Trt")), weight=c(ctl, trt))
  lm.1 <- regress(weight ~ group, data=data)
  lm.2 <- regress(data=data, formula=weight ~ group)
  expect_that(lm.2$coefficients == lm.1$coefficients, is_true())
  expect_that(lm.2$residuals == lm.1$residuals, is_true())
})
