#function that removes the doubles in the m column in the fish dataframe 
#(number in and outside of the paranthesis)

excl_doubles <- function(df){
  levels <- levels(df[[3]])
  names <- as.character(str_extract_all(levels, "\\([^()]+\\)"))
  names(levels) <- names
  for(element in names(levels)){
    count <- 0
    for(row in df[[1]]){
      count <- count+1
      if(row==element){
        df[count,1] <- levels[element]
      }
    }
  }
  df[,1:2]
}