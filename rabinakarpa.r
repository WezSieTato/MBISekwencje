source('hashString.r')

rabinakarpa <- function(s, w, hash = largePrimeHash){
  n <- nchar(s)
  m <- nchar(w)
  Hw <- hash(w)
  for( i in 1 : n - m + 1){
    pS <- substr(s, i, i + m - 1)
    Hs <- hash(pS)
    if(Hs == Hw){
      if(pS == w){
        return(i)
      }        
    }
  }
  return(0)
}