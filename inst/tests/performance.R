context("performance")

Returns(x) %as% x

lrsd(x) %as% { sd(x) }

lrsd1(x) %::% Returns : numeric
lrsd1(x) %as% { sd(x) }

lrsd1(x) %::% Dummy : numeric
lrsd1(x) %as% { sd(x) }

s3sd <- function(x,...) UseMethod('s3sd')
s3sd.default <- function(x) { sd(x) }

measure.call_time <- function(n=10000000, y=10000)
{
  data <- Returns(rnorm(y))
  
  method1 <- function(x) { for (i in n) s3sd(x) / i }
  method2 <- function(x) { for (i in n) lrsd(x) / i }
  method3 <- function(x) { for (i in n) lrsd1(x) / i }

  cat("UseFunction (untyped)\n")
  print(system.time(method2(data)))
  cat("\n")
  cat("UseFunction (typed)\n")
  print(system.time(method3(data)))
  cat("\n")
  cat("UseMethod\n")
  print(system.time(method1(data)))
  invisible()
}

measure.call_time()
