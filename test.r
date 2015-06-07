source ('generation.r')
source ('rabinakarpa.r')
source ('naive.r')
source('knuthamorissonapratta.r')
source('prefixsufixtable.r')

#metoda do test??w
# n - d??ugo???? sekwencji, kt??ra b??dzie przeszukiwana
# m - d??ugo???? sekwencji, kt??ra b??dzie szukana
# metod - wska??nik na funkcj??, kt??ra ma by?? testowana
# funkcja zwraca czas wykonania
testMethod <-function(n, m, metod){
  S <- generateSequence(n)
  i <- sample(n-m,1)
  W <- substr(S, i, 1+m-1)
#  print(S)
#  print(W)
  t <- system.time({
      metod(S, W)
     })
  
  return(t[[3]]);
}

#metoda do wielokrotnych test??w
# n - d??ugo???? sekwencji, kt??ra b??dzie przeszukiwana
# m - d??ugo???? sekwencji, kt??ra b??dzie szukana
# metod - wska??nik na funkcj??, kt??ra ma by?? testowana
# t - ilosc powtorzen testu
# funkcja zwraca sredni czas wykonania
avgTestMethod <-function(n, m, t, metod){
  vec <- c()
  for( i in 1:t){
    vec[i] <- testMethod(n, m, metod)
  }
  
  return(mean(vec))
}


testMethods <-function(n, m, numberOfMethod = 6){
  S <- generateSequence(n)
  i <- sample(n-m,1)
  W <- substr(S, i, i+m-1)
 #   print(S)
#    print(W)
  iteration <- 0

  c <- c()
  Sc <-strsplit(S, "")[[1]]
  Wc <- strsplit(W, "")[[1]]
  t <- system.time({
    naive_table(Sc, Wc)
  })
  iteration <- iteration + 1
  c[iteration] <- t[[3]];
  if(iteration  == numberOfMethod)
      return(c);

  t <- system.time({
    naive(S, W)
  })
 iteration <- iteration + 1
 c[iteration] <- t[[3]];
  if(iteration  == numberOfMethod)
    return(c);
  
  table <- createprefixsufixtable(Sc)
  t <- system.time({
    knuthamorissonapratta(Sc, Wc, table)
  })
  iteration <- iteration + 1
  c[iteration] <- t[[3]];
  if(iteration  == numberOfMethod)
    return(c);

  t <- system.time({
    rabinakarpa(S, W)
  })
  iteration <- iteration + 1
  c[iteration] <- t[[3]];
  if(iteration  == numberOfMethod)
   return(c);

  t <- system.time({
   rabinakarpa_table(Sc, Wc)
  })
  iteration <- iteration + 1
  c[iteration] <- t[[3]];
  if(iteration  == numberOfMethod)
   return(c);

  t <- system.time({
    rabinakarpaBadHash(S, W)
  })
  iteration <- iteration + 1
  c[iteration] <- t[[3]];

  if(iteration  == numberOfMethod)
    return(c);
}

avgTestMethods <-function(n, m, t){
  vec <- matrix( 
       nrow=t,              # number of rows 
       ncol=4,              # number of columns 
       byrow = TRUE)
  for( i in 1:t){
    vec[i,] <- testMethods(n, m)
  }
  
  return(colMeans(vec))
}

# Funkcja do wy??wietlania histagram??w
# n - d??ugo???? sekwencji, kt??ra b??dzie przeszukiwana
# m - d??ugo???? sekwencji, kt??ra b??dzie szukana
# t - ilo???? iteracji
# numberOfMethod - ilo???? metod testowanych, musi  si?? zgadza?? z liczb??
# wykorzystywanych metod w testMethods
histTestMethods <- function(n, m, t, numberOfMethod = 6) {
  vec <- matrix( 
    nrow=t,              # number of rows 
    ncol=numberOfMethod, # number of columns 
    byrow = TRUE)
  for(i in 1:t) {
    ve <- testMethods(n, m, numberOfMethod)
    vec[i,] <- ve
  }
  names <- c("Naiwny - wektor", 
        "Naiwny - ciag znakow", 
        "Knutha Morissa Pratta", 
        "Rabina Karpa - ciag znakow",
        "Rabina Karpa - wektor", 
        "Rabina Karpa sum hash")
  
  values <- c()
  
  for(i in 1:ncol(vec)) {
    hist(vec[,i],main = names[i])
    values[i * 4 - 3] <- min (vec[,i])
    values[i * 4 - 2] <- mean (vec[,i])
    values[i * 4 - 1] <- median (vec[,i])
    values[i * 4 ] <- max (vec[,i])
    
  }
  
  namesShort <- c("N-Table", 
             "N-String", 
             "KMP", 
             "RK-String", 
             "RK-Table", 
             "RK-sum hash")
  
  smoke <- matrix(values,ncol = numberOfMethod ,byrow=FALSE)
  colnames(smoke) <- namesShort[1:numberOfMethod]
  rownames(smoke) <- c("min", "average", "median", "max")
  smoke <- as.table(smoke)
  return (smoke)
}
