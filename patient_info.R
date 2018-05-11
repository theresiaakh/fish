#function that loos extract relevant information from one patient and makes a table 
patient_info <- function(char){
  mrn <- str_extract(str_match(char, "\\MRN: \\d+"), "\\d+")
  collection_date <- str_extract(str_match(char, "\\Date of Receipt: \\d+/\\d+/\\d+"), "\\d+/\\d+/\\d+")
  fishtext <- get.fish(char)
  iscn <- str_extract(str_match(char, '\\[ISCN \\d+'), '\\ISCN \\d+') #cant do this when also SNP arrays..
  fish_df <- fish_dataframe(fishtext)
  fish_res <- fish_analysis(fish_df)
  result <- cbind.data.frame(mrn, collection_date, iscn, fish_res)
  result
}