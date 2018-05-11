#function that looks at the fish results with numbers inside of paranthesis and puts the probes in columns

finding_fish <- function(df){
  x_outside <- str_extract(df$p, "\\)x")
  index <- which(is.na(x_outside))
  probe <- as.character(df[index,1])
  probe <- substring(probe, 2, nchar(probe)-1)  
  no_of_cells <- as.character(df[index,2])
  no_of_cells <- substring(no_of_cells, 2, nchar(no_of_cells)-1)
  names(no_of_cells) <- probe
  names(probe) <- no_of_cells
  probes <- strsplit(probe, ",")
  no_of_cells <- c()
  for(element in names(probes)){
    no_of_cells <- append(no_of_cells, rep(element, length(probes[[element]])))
  }
  probes <- unlist(probes)
  newdf <- data.frame()
  newdf <- rbind(newdf, no_of_cells)
  names(newdf) <- probes
  newdf
}
