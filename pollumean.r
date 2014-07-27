pollutantmean <- function(directory, pollutant, id = 1:332) {
 pollutantData<-data.frame()
  for(file in rep(id)){
    pollutantData<-rbind(pollutantData,read.table(paste(directory,"/",sprintf("%03d", file),".csv",sep=""),sep=",",header=TRUE))
