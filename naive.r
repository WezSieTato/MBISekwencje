
naive <- function(s, w){
  n <- nchar(s)
  m <- nchar(w)
  for( i in 1 : n - m + 1){
    if(substr(s, i, i + m - 1) == w)
      return(i)
  }
  return(0)
}

naive_table <- function(s, w){
  n <- length(s)
  m <- length(w)
  for( i in 1 : n){
    first <- i
    for( j in 1:m) {
      if(w[j] != s[i+j-1]){
        break
      } 
      if(is.na(w[j+1])) {
        return(first)
      }
    }
  }
  return(0)
}