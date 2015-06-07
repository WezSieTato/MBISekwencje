knuthamorissonapratta <- function(s, w, presuftable){
  
  n <- length(s)
  m <- length(w)
  i <- 1
  j <- 0
  while(i <= n - m + 1) {
    while(j <= m ) {
      if(w[j+1] != s[i+j] || is.na(s[i+j]) || is.na(w[j+1])) 
        break
      j = j + 1
    }
    if(j == m )
      return(i)
    if(j > 0) {
      i = i + j - presuftable[j]
      j = presuftable[j]
    } else {
      i = i + 1
    }
  }
  return(0)
}

