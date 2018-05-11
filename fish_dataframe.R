#function that combines the 3 above function and outputs a final dataframe of the fish result for one patient
fish_dataframe <- function(char){
  data <- nucish(char)
  factor_df <- mutate(data, p=addFactorLevels(data$p, levels(data$m)))
  df <- excl_doubles(factor_df)
  return(df)
}