source('prefixsufixtable.r')

knuthamorissonapratta <- function(s, w, presuftable = method1){
  T <- presuftable(s)
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
      i = i + j - T[j]
      j = T[j]
    } else {
      i = i + 1
    }
  }
  return(0)
}

