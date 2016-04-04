get_data_from_local_files <- function (account){
  setwd(paste("~/Documents/r_projects/data/",account,sep=""))
  filelist = list.files(pattern = paste("*",account,"*", sep=""))
  datalist = lapply(filelist, function(x) read.table(x, sep = ",", header = TRUE, fill = TRUE)) 
  #datafr = do.call("rbind", datalist) 
  datafr = rbind_all( datalist) 
  return (datafr)
}

get_test_data_from_local_files <- function (account){
  setwd(paste("~/Documents/r_projects/data/",account,sep=""))
  filelist = list.files(pattern = paste("*",account,"_testing_set_","*", sep=""))
  datalist = lapply(filelist, function(x) read.table(x, sep = ",", header = TRUE, fill = TRUE)) 
  datafr = rbind_all(datalist)
  return (datafr)
}