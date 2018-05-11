#function that combines the two above funvtion and outputs a final fish interpretation for one patient
fish_analysis <- function(df){
  x_outside <- x_outside_standardized(df)
  findings <- finding_fish(df)
  if((!is.null(unlist(x_outside))) & (!is.null(unlist(findings)))){
    result <- merge(x_outside, findings)
  } else if(is.null(unlist(x_outside))){
    result <- findings
  } else if(is.null(unlist(findings))){
    result <- x_outside
  }
  result
}