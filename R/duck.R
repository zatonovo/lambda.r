'%isa%' <- function(argument, type)
{
  type <- gsub('[\'"]','',deparse(substitute(type)))
  type %in% class(argument)
}

# Note this will produce a vector of results
'%hasa%' <- function(argument, property)
{
  property <- gsub('[\'"]','',deparse(substitute(property)))
  property <- gsub(' ','', property, fixed=TRUE)
  property <- sub('c(','', property, fixed=TRUE)
  property <- sub(')','', property, fixed=TRUE)
  props <- strsplit(property, ',', fixed=TRUE)[[1]]
  props %in% names(argument)
}

'%hasall%' <- function(argument, property)
{
  property <- gsub('[\'"]','',deparse(substitute(property)))
  property <- gsub(' ','', property, fixed=TRUE)
  property <- sub('c(','', property, fixed=TRUE)
  property <- sub(')','', property, fixed=TRUE)
  props <- strsplit(property, ',', fixed=TRUE)[[1]]
  all(props %in% names(argument))
}


