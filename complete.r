complete <- function(directory, id = 1:332) {
total<-data.frame()
  for(file in rep(id)){
    pollutantData<-read.table(paste(directory,"/",sprintf("%03d", file),".csv",sep=""),sep=",",header=TRUE)
    total<-rbind(total,cbind(file,nrow(pollutantData[complete.cases(pollutantData),])))
  }
  colnames(total)<-c("id","nobs")
  total
}
