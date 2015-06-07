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


testMethods <-function(n, m){
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

  t <- system.time({
    naive(S, W)
  })
 iteration <- iteration + 1
 c[iteration] <- t[[3]];

  t <- system.time({
    rabinakarpa(S, W)
  })
 iteration <- iteration + 1
 c[iteration] <- t[[3]];
  
  Sc <-strsplit(S, "")[[1]]
  Wc <- strsplit(W, "")[[1]]
  table <- createprefixsufixtable(Sc)
  t <- system.time({
    knuthamorissonapratta(Sc, Wc, table)
  })
  iteration <- iteration + 1
  c[iteration] <- t[[3]];

  t <- system.time({
    rabinakarpaBadHash(S, W)
  })
  iteration <- iteration + 1
  c[iteration] <- t[[3]];

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

# Funkcja do wyświetlania histagramów
# n - d??ugo???? sekwencji, kt??ra b??dzie przeszukiwana
# m - d??ugo???? sekwencji, kt??ra b??dzie szukana
# t - ilość iteracji
# numberOfMethod - ilość metod testowanych, musi  się zgadzać z liczbą
# wykorzystywanych metod w testMethods
histTestMethods <- function(n, m, t, numberOfMethod = 5) {
  vec <- matrix( 
    nrow=t,              # number of rows 
    ncol=numberOfMethod, # number of columns 
    byrow = TRUE)
  for(i in 1:t) {
    vec[i,] <- testMethods(n, m)
  }
  s = c("Naiwny tablicowy", 
        "Naiwny string", 
        "Rabina Karpa", 
        "Knutha Morissona Pratta", 
        "Rabina Karpa s??aby hash")
  for(i in 1:ncol(vec)) {
    hist(vec[,i],main = s[i])
  }
  
}
