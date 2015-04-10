asciSumHash <-function(string){
  return(sum(utf8ToInt(string)))
}

largePrimeHash <-function(string, c = 101){
  n <- nchar(string)
  code <- utf8ToInt(string)
  hash <- 0
  for(i in 1 : n){
    hash <- hash + code[i] * (c ^ (n - i))
  }
  return(hash)
}