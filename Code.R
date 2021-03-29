raw <- read_excel("Project/data/combine18.xlsx")
coln1 <- raw[5,]
coln2 <- raw[6,]

colnames <- list()
for (i in 1:length(coln2)) {
  print(i)
  if (!is.na(i)){
    colnames[i] <- paste(toString(coln1[i]),toString(coln2[i]),sep = " ")
  }else{
    colnames[i] <- toString(coln2[i])
  }
}

df <- raw[-c(1:6),]
colnames(df) <- colnames


