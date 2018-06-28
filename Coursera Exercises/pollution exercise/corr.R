corr <- function(directory, threshold = 0) {
  filesNames<-list.files(directory,full.names = TRUE)
  nobs_table<-complete("specdata",id=1:332)
  
  ids <- nobs_table$id[nobs_table$nobs>threshold]
  corr_res<-numeric()
  for(i in ids){
    data<-read.csv(filesNames[i])
  corr_res <- c(corr_res,cor(data$sulfate,data$nitrate,use="complete.obs"))
}
return(corr_res)
}
