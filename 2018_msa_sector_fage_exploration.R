df_MSF <- read.csv("data/bds2018_msa_sector_fage.csv")


#df_working_slice <-slice(df_MSF,(1:100000))
df_working_slice <- df_MSF

variables_req <- c("year","msa", "sector", "fage", "firms", "estabs", "emp", "estabs_entry" , "estabs_entry_rate", "estabs_exit", "estabs_exit_rate")
#variables_req <- c("year","msa", "sector", "fage")
df_useful <- data.frame(matrix(ncol = length(variables_req)))
colnames(df_useful) <- variables_req
for(i in 1:dim(df_working_slice)[1]){
  row <- df_working_slice[i,variables_req]
  
  if((!is_empty(list(row))) && (sum(is.na(row)) == 0)){
    df_useful <- rbind(df_useful, row)
  }
}

years <- list()
years_count <- list()
count <- 0
for (i in 1:dim(df_working_slice)[1]){
  year <- df_working_slice$year[i]
  
  #if year is not in list, add it
  if (! year %in% years){
    if (! count == 0 ){
      years_count <- c(years_count,count+1)
    }
    years <- c(years,year)
    count <- 0
  }else{ # if it is count how many times it appears
    count <- count + 1 
  }
}
years_count <- c(years_count,count)


colnames <- colnames(df_working)
df_working_count <- data.frame(matrix(nrow = length(years), ncol = length(colnames)))
colnames(df_working_count) <- colnames

for( i in 1:length(years)){
  df_working_count$year[i] <- years[[i]][1]
}





variables_req_tableau <- c("year","msa", "sector","net_job_creation_rate")


df_tableauA <- data.frame(matrix(ncol = length(variables_req_tableau)))
colnames(df_tableauA) <- variables_req_tableau
start_time <- Sys.time()
for(i in 1:dim(df_working_slice)[1]){
  row <- df_working_slice[i,variables_req_tableau]
  df_tableauA <- rbind(df_tableauA, row)
}
end_time <- Sys.time()
print(end_time-start_time)



df_tableauB <- data.frame(matrix(nrow = dim(df_working_slice)[1],ncol = length(variables_req_tableau)))
colnames(df_tableauB) <- variables_req_tableau
start_time <- Sys.time()
for(i in 1:dim(df_working_slice)[1]){
  df_tableauB[i,] <- df_working_slice[i,variables_req_tableau]
}
end_time <- Sys.time()
print(end_time-start_time)




#df_tableauC <- data.frame(matrix(nrow = dim(df_working_slice)[1],ncol = length(variables_req_tableau)))
df_tableauC <- data.frame(matrix(nrow = dim(df_working_slice)[1],ncol = length(variables_req_tableau)))
#colnames(df_tableauC) <- variables_req_tableau
dt_tableauC <- data.table(df_tableauC)
list_tableauC <- as.list(df_tableauC)


start_time <- Sys.time()
dt <- data.table(df_working_slice)
dt_tableauC <- dt[,.(year,msa, sector,net_job_creation_rate)]
df_tableauC <- data.frame(dt_tableauC)
end_time <- Sys.time()
print(end_time-start_time)
      
      
for(i in 1:dim(df_working_slice)[1]){
 # set(dt_tableauC,i,df_working_slice[i,variables_req_tableau])
  dt_tableauC[i,year := df_working_slice$year[i]]
}
df_tableauC <-as.data.frame(dt_tableauC)
colnames(df_tableauC) <- variables_req_tableau
end_time <- Sys.time()
print(end_time-start_time)




MSAs <- list()
start_time <- Sys.time()
for (i in 1:dim(df_working_slice)[1]){
  #if msa is not in list, add it
  if (! df_working_slice$msa[i] %in% MSAs){
    MSAs<-c(MSAs,df_working_slice$msa[i])
  }
}
end_time <- Sys.time()
print(end_time-start_time)



MSAs2 <- list()
start_time <- Sys.time()
for (i in 1:dim(df_working_slice)[1]){
  #if msa is not in list, add it
  if (! any(df_working_slice$msa[i] == MSAs2)){
    MSAs2<-c(MSAs2,df_working_slice$msa[i])
  }
}
end_time <- Sys.time()
print(end_time-start_time)



numeric_colnames <- colnames(df_working_slice)[-c(1:4)]
for(ncol in numeric_colnames ){
  df_working_slice[[ncol]] <- as.numeric(df_working_slice[[ncol]])
}


variables_req <- c("year","msa", "sector","firms", "estabs", "emp", "net_estabs" , "net_estabs_rate", "net_job_creation", "net_job_creation_rate")
## loop thorugh working slice and reduce the 5 fage rows into one per each year-msa-sector
start_time <- Sys.time()
df_reduced_fage <- data.frame(matrix(nrow = (dim(df_working_slice)[1])/5,ncol = length(variables_req)))
colnames(df_reduced_fage) <- variables_req
rowID_og <- 1
rowID_simplified <- 1
while (rowID_og < 3383730 ){
  if((rowID_og %% 1000) == 0){
    print("rowID: ") 
    print(rowID_og)
  }
  
  if ( length(unique(df_working_slice$year[rowID_og:(rowID_og+4)])) == 1 &&
       length(unique(df_working_slice$msa[rowID_og:(rowID_og+4)])) == 1 &&
       length(unique(df_working_slice$sector[rowID_og:(rowID_og+4)])) == 1 &&
       length(unique(df_working_slice$fage[rowID_og:(rowID_og+4)])) == 5)
  {
    firms_sum = sum(df_working_slice$firms[rowID_og:(rowID_og+4)],na.rm=T)
    if (! firms_sum == 0){
      df_reduced_fage$year[rowID_simplified] <- df_working_slice$year[rowID_og]
      df_reduced_fage$msa[rowID_simplified] <- df_working_slice$msa[rowID_og]
      df_reduced_fage$sector[rowID_simplified] <- df_working_slice$sector[rowID_og]
      df_reduced_fage$firms[rowID_simplified] <- firms_sum
      
      df_reduced_fage$estabs[rowID_simplified] <- sum(df_working_slice$estabs[rowID_og:(rowID_og+4)],na.rm=T)
      df_reduced_fage$emp[rowID_simplified] <- sum(df_working_slice$estabs[rowID_og:(rowID_og+4)],na.rm=T)
      
      
      df_reduced_fage$net_estabs[rowID_simplified] <- sum(df_working_slice$estabs_entry[rowID_og:(rowID_og+4)],na.rm=T) - sum(df_working_slice$estabs_exit[rowID_og:(rowID_og+4)],na.rm=T)
      df_reduced_fage$net_estabs_rate[rowID_simplified] <- 100*(df_reduced_fage$net_estabs[rowID_simplified]/df_reduced_fage$estabs[rowID_simplified])
      df_reduced_fage$net_job_creation[rowID_simplified] <- sum(df_working_slice$net_job_creation[rowID_og:(rowID_og+4)],na.rm=T)
      df_reduced_fage$net_job_creation_rate[rowID_simplified] <- getweightedsum(df_working_slice$net_job_creation_rate[rowID_og:(rowID_og+4)],df_working_slice$estabs[rowID_og:(rowID_og+4)])
      
      rowID_simplified <- rowID_simplified + 1
      
    }else{
      #print("firms_sum == 0: ")
      #print(rowID_og)
      
    }
  }else{
    
    #((df_working_slice$year[rowID_og:(rowID_og+4)]))
    #((df_working_slice$msa[rowID_og:(rowID_og+4)])) 
    #((df_working_slice$sector[rowID_og:(rowID_og+4)]))
    #((df_working_slice$fage[rowID_og:(rowID_og+4)]))
    
  }
  rowID_og <- rowID_og + 5
}
end_time <- Sys.time()
print(end_time-start_time)



time_simplifying <- function(numrows){
  
  

}


listofrows <- list(100,500,1000,5000)
listoftimes <- list()
listoftimesperrow <- list()
for(i in 1:length(listofrows)){
  n = listofrows[[i]]
  proc_start <- proc.time()
  time_simplifying(n)
  time_elaplsed <- (proc.time()-proc_start)[2]
  time_perrow <- time_elaplsed / n
  listoftimes <- c(listoftimes, time_elaplsed)
  listoftimesperrow <- c(listoftimesperrow, time_perrow)
}

.timescale <- listoftimes/listofrows


getweightedsum <- function(listvalues, listestabs){
  
  if (! length(listvalues) == length(listestabs)){
    print("listvalues and listestabs length does not match")
    results <- NaN
  }else{
    result <- 0
    estabs_sum <- sum(listestabs,na.rm = T)
    for(i in 1:length(listvalues)){
      if (! is.na(listestabs[i]) && !is.na(listvalues[i])){
      result <- result + ((listestabs[i]/estabs_sum)*listvalues[i]) 
    }else{
      #print("At least one is NA")
      #print(listestabs[i])
      #print(listvalues[i])
    }
  }
  return(result)
  }
}


for(rowID_og in 1:100){
  print(rowID_og)
  print((df_working_slice$job_creation[rowID_og] -df_working_slice$job_destruction[rowID_og]) ==(df_working_slice$net_job_creation[rowID_og]))
}

print(getweightedsum(list(1,1,1,1),list(1,1,1,1)))
