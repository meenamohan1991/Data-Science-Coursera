corr <- function(directory, threshold = 0) {
 
  total<-numeric()
  for(file in list.files(directory)){
    pollutantData<-read.table(paste(directory,"/",file,sep=""),sep=",",header=TRUE)
    comple<-nrow(pollutantData[complete.cases(pollutantData),])
    if(comple>threshold){
      #total<-rbind(total,cor(pollutantData$nitrate,pollutantData$sulfate,use="complete.obs"))
      total<-c(total,cor(pollutantData$nitrate,pollutantData$sulfate,use="complete.obs"))
    }
  }
  as.numeric(total)
}
