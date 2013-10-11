# :vim set filetype=R
regress(formula, ..., na.action=na.fail) %as% {
  lm(formula, ..., na.action=na.action)
}
seal(regress)

test.ellipsis_arguments_2 <- function() {
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  data <- data.frame(group=gl(2,10,20,labels=c("Ctl","Trt")), weight=c(ctl, trt))
  lm.1 <- regress(weight ~ group, data=data)
  lm.2 <- regress(data=data, formula=weight ~ group)
  checkTrue(all(lm.2$coefficients == lm.1$coefficients))
  checkTrue(all(lm.2$residuals == lm.1$residuals))
}
