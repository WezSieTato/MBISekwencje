source ('generation.r')
source ('rabinakarpa.r')
source ('naive.r')
source('knuthamorissonapratta.r')

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


testMethods <-function(n, m){
  S <- generateSequence(n)
  i <- sample(n-m,1)
  W <- substr(S, i, i+m-1)
 #   print(S)
#    print(W)
  c <- c()
  t <- system.time({
    naive(S, W)
  })
  c[1] <- t[[3]];

  t <- system.time({
    rabinakarpa(S, W)
  })
  c[2] <- t[[3]];
  
  Sc <-strsplit(S, "")[[1]]
  Wc <- strsplit(W, "")[[1]]
  t <- system.time({
    knuthamorissonapratta(Sc, Wc)
  })
  c[3] <- t[[3]];

  t <- system.time({
    rabinakarpaBadHash(S, W)
  })
  c[4] <- t[[3]];

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

