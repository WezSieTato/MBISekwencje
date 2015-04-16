source('hashString.r')

rabinakarpa <- function(s, w){
  n <- nchar(s)
  m <- nchar(w)
  hash <- largePrimeHash
  c <- 7
  Hw <- hash(w, c)
  code <- utf8ToInt(s)
  codeN <- code * ( c ^ (m - 1))
  Hs <- hash(substr(s, 1, m), c)
  for( i in 1:(n - m + 1)){
    pS <- substr(s, i, i + m - 1)
    if(i > 1){
      Hs <- ((c * (Hs - codeN[i - 1])) + code[i + m - 1])
    }
    if(Hs == Hw && pS == w){
        return(i)
    }
  }
  return(0)
}

rabinakarpaChooseHash <- function(s, w, hash = largePrimeHash){
  n <- nchar(s)
  m <- nchar(w)
  Hw <- hash(w)
  for( i in 1 : n - m + 1){
    pS <- substr(s, i, i + m - 1)
    Hs <- hash(pS)
    if(Hs == Hw && pS == w){
        return(i)
    }
  }
  return(0)
}

rabinakarpaBadHash <- function(s, w){
  return (rabinakarpaChooseHash(s, w, asciSumHash))
}