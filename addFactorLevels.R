#function that add some additional levels to the factor levels of a column in a dataframe

addFactorLevels <- function(z, levels){
  z <- factor(x=as.character(z), levels=c(levels(z), levels))
  z
}