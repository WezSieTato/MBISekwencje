
generateSequence <- function(n){
  nucl <- c("G", "A", "T", "C")
  los <- sample(1:4, n, replace=T)
  return(paste(nucl[los], collapse=''))
}