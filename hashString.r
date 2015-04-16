asciSumHash <-function(string){
  return(sum(utf8ToInt(string)))
}

largePrimeHash <-function(string, c = 101){
  n <- nchar(string)
  code <- utf8ToInt(string)
  hash <- (c ^ ((n-1) : 0)) * utf8ToInt(string)

  return(sum(hash))
}