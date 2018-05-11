#Function that takes a string and extract everything in parantheseses and brackets and returns a dataframe

nucish <- function(char){
  char <- tolower(char)
  p <- str_extract_all(char, "\\([^()]+\\)")[[1]] 
  b <- str_extract_all(char, "\\[[^()]+\\]")[[1]]
  m <- str_extract_all(char, "\\([^()]+\\)x\\d")[[1]]
  d <- length(p)-length(m)
  m <- append(m, rep(NA, d))
  df <- data.frame(p,b,m)
  df
}

# references: 
# https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r