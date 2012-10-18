compare <- function(a,b, xs) {
  plot(xs, a(xs), type='l')
  lines(xs, b(xs), type='l', col='blue')
  invisible()
}

# f <- taylor(sin, pi)
# xs <- seq(2,4.5,0.02)
# compare(sin,f, xs)
#
# p <- function(x) x^4 + 3 * (x-2)^3 - 2 * x^2 + 1
# p1 <- function(x) 4*x^3 + 9*(x-2)^2 - 4*x
# p2 <- function(x) 12*x^2 + 18*(x-2) - 4
# p3 <- function(x) 24*x + 18
#
# f <- taylor(p, 1)
# xs <- seq(-5,5,0.02)
# compare(p,f, xs)
# 
# f(x) ~ f(a) + f'(a) * (x - a) + f''(a) / 2! * (x - a)^2 + ...
test.taylor_series_1 <- function() {
  seal(fac)
  fac(1) %as% 1
  fac(n) %when% { n > 0 } %as% { n * fac(n - 1) }

  # TODO: Implement this properly for k > 2
  d(f, 1, h=10^-9) %as% function(x) { (f(x + h) - f(x - h)) / (2*h) }
  d(f, 2, h=10^-9) %as% function(x) { (f(x + h) - 2*f(x) + f(x - h)) / h^2 }
   
  taylor(f, a, step=2) %as% taylor(f, a, step, 1, function(x) f(a))
  taylor(f, a, 0, k, g) %as% g
  taylor(f, a, step, k, g) %as% {
    df <- d(f,k)
    g1 <- function(x) { g(x) + df(a) * (x - a)^k / fac(k) }
    taylor(f, a, step-1, k+1, g1)
  }

  f <- taylor(sin, pi)
  v <- f(3.1)
  checkEquals(v, sin(3.1), tolerance=0.01)
}
