asciSumHash <-function(string){
  return(sum(utf8ToInt(string)))
}

largePrimeHash <-function(string, c = 101){
  n <- nchar(string)
  code <- utf8ToInt(string)
  hash <- (c ^ ((n-1) : 0)) * utf8ToInt(string)

  return(sum(hash))
}

primeHash <- function(table, n, c = 31) {
  hash <- 0
  for(i in 1:n) {
    hash <- c*hash + utf8ToInt(table[i])
  }
  return(hash)
}

compareString <- function(s,index, m,w) {
  for(i in 1:m) {
    if(s[index + i -1] != w[i]) {
      return(FALSE)
    }
  }
  return(TRUE)
}