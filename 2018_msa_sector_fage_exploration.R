df_MSF <- read.csv("data/bds2018_msa_sector_fage.csv")


df_working_slice <-slice(df_MSF,(1:5000))

variables_req <- c("year","msa", "sector", "fage", "firms", "estabs", "emp", "estabs_entry" , "estabs_entry_rate", "estabs_exit", "estabs_exit_rate")
#variables_req <- c("year","msa", "sector", "fage")
df_useful <- data.frame(matrix(ncol = length(variables_req)))
colnames(df_useful) <- variables_req
for(i in 1:length(df_working_slice)){
  row <- df_working_slice[i,variables_req]
  
  if((!is_empty(list(row))) && (sum(is.na(row)) == 0)){
    df_useful <- rbind(df_useful, row)
  }
}

years <- list()
years_count <- list()
count <- 0
for (i in 1:dim(df_working)[1]){
  year <- df_working$year[i]
  
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