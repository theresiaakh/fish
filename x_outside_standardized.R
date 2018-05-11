#Updated function that makes it more compatible with finding fish: 
#function that looks at the fish results with the xNUM outside of the paranthesis, 
#and puts them as columns

x_outside_standardized <- function(df){
  x_outside <- str_extract(df$p, "\\)x")
  if(all(is.na(x_outside))){
    return(NULL)
  } else{
    index <- which(!is.na(x_outside))
    probe <- as.character(df[index,1])
    no_of_copies <- c()
    for(element in probe){
      no_of_copies <- append(no_of_copies, str_extract(str_match(element, "\\)x\\d"), "\\d"))
    }
    probe <- substring(probe, 2, nchar(probe)-3)
    names(no_of_copies) <- probe
    probes <- c()
    c <- 0
    no_of_cells <- c()
    for(element in names(no_of_copies)){
      c <- c+1
      sub <- strsplit(element, ",")[[1]]
      cells <- as.character(df[index[c],2])
      cells <- substring(cells, 2, nchar(cells)-1)
      cells <- rep(cells, length(sub))
      no_of_cells <- append(no_of_cells, cells)
      for(p in sub){
        probes <- append(probes, paste0(p, "x", no_of_copies[[element]], sep=""))
      }
    }
    newdf <- data.frame()
    newdf <- rbind(newdf, no_of_cells)
    names(newdf) <- probes
    newdf
  }
}