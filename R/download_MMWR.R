Y <- 2017; w <- 1:31
fileList <- paste("MMWR",Y, w, "txt", sep=".")
queryMMWR <- function(Y, w){

paste0("https://wonder.cdc.gov/mmwr/mmwr_reps.asp?mmwr_year=", Y, "&mmwr_week=", sprintf("%02d", w), "&mmwr_table=2M&request=Export&mmwr_location=")
  
}
query2017 <- queryMMWR(2017, 1:31)
library(utils)
lapply(seq_along(query2017), function(i){
  download.file(query2017[i], fileList[i])
}
)
