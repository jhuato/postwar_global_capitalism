A <- matrix(c(1,3,5,2,4,6), nrow= 3)
A
B <- matrix(c(8,8,8,8,8,8), nrow= 2)
B
require(matrixcalc)
C <- A %*% B
matrix.trace(C)

curve(exp(-(1/2)*(x^2)/sqrt(2*pi)), from=-4, to=4, type="l")
      