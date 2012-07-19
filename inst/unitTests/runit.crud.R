create.Pear <- function(klass, seeds=5, weight=10, type='bartlett')
{
  list(seeds=seeds, weight=weight, type=type)
}

test.create.default <- function()
{
  a <- create(Apple, seeds=10)
  checkTrue(isa(Apple,a))
  checkTrue(a$seeds == 10)
}


test.create.Pear <- function()
{
  b <- create('Pear', seeds=6)
  checkTrue(isa('Pear',b))
  checkTrue(b$seeds == 6)
  checkTrue(b$weight == 10)
  checkTrue(b$type == 'bartlett')
}

test.isa.symbol <- function()
{
  b <- create(Pear, seeds=6)
  checkTrue(isa(Pear,b))
  checkTrue(b$seeds == 6)
  checkTrue(b$weight == 10)
  checkTrue(b$type == 'bartlett')
}

