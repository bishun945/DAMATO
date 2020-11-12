library(openxlsx)
library(xlsx)
library(Hmisc)
library(do)

my_filter <- function(df,n){
  df <- read.xlsx2(choose.files(), 1)
  df[,3] <- as.numeric(df[,3])
  df[,3] <- as.Date(df[,3],origin = "1899-12-30")
  index <- which(is.na(df[,n]))
  number <- length(index)
  nrows <- length(df[,n])
  if(number == 0){
    print('There is not the value of na')
  }else{
    for(x in 1:nrows){
      if(is.na(df[x,n])){
        df[x,n] <- impute(df[x,n],unlist(df[x-1,n]))
      }
      else{print('It is ok!')}
    }
  }
  return(df)
}