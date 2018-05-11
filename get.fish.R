#function that extract only fish results from the text for each patient

get.fish <- function(char){
  char <- str_replace_all(char, "[\r\n]" , "")
  char <- str_replace_all(char, " ", "")
  fish.text <- rm_between(char, "nucish", "studied", extract=TRUE)[[1]]
  if(is.na(fish.text)){
    fish.text <- rm_between(char, "nucish", "Note", extract=TRUE)[[1]]
  }
  if(is.na(fish.text)){
    fish.text <- rm_between(char, "nucish", "DISCLAIMER", extract=TRUE)[[1]]
  }
  fish.text
}