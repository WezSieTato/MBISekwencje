source ('generation.r')
source ('rabinakarpa.r')
source ('naive.r')

#metoda do test??w
# n - d??ugo???? sekwencji, kt??ra b??dzie przeszukiwana
# m - d??ugo???? sekwencji, kt??ra b??dzie szukana
# metod - wska??nik na funkcj??, kt??ra ma by?? testowana
# funkcja zwraca czas wykonania
testMethod <-function(n, m, metod){
  S <- generateSequence(n)
  W <- generateSequence(m)
  t <- system.time({
      metod(S, W)
     })
  
  return(t[[3]]);
}