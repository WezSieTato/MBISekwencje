
naive <- function(s, w){
  n <- nchar(s)
  m <- nchar(w)
  for( i in 1 : n - m + 1){
    if(substr(s, i, i + m - 1) == w)
      return(i)
  }
  return(0)
}

