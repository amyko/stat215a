# load ling data 
loadLingData <- function(path = "data/", source) {
  # Arguments:
  #   path: the path indicating the location of the `sonoma-data*` data files.
  #         Path should be relative to the lab1.Rnw file.
  # Returns:
  #   a data frame consisting of the specified dataset
  
  # load in the csv file
  ling_data = read.table(paste0(path,"ling", source ,".txt"), header = TRUE)
  return(ling_data)
}
