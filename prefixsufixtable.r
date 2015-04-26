method1 <- function(sequence){
  result <- c()
  n <- length(sequence)
  result[1] <- 0
  if(n == 1) {
    return(result)
  }
  t <- 0
  for( j in 2:n) {
    while( t >= 1 && sequence[t + 1] != sequence[j] )
      t <- result[t]
    
    t <- t + 1
    result[j] <- t
  }
  
  return(result)
}  
  