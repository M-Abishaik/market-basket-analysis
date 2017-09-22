addthree <- function(x,y,z){
  x+y+z
}


above_n <- function(x, n = 10) {
  use <- x > n
  
  print(x[use])
}

analyzemean <- function(x, removeNA = TRUE) {
  nocol <- ncol(x)
  means <- numeric(nocol)
  for (i in 1:nocol) {
    means[i] <- mean(x[, i], na.rm = removeNA)
  }
  means
}

make.power <- function(n){
  pow <- function(x){
    x ^ n;
  }
  pow
}







