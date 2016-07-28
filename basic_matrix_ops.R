# Experimenting with R
# Julio Huato
A <- matrix(c(1,2,3,0),nrow=2,ncol=2) # filled by column
B <- matrix(c(0,3,2,1),nrow=2,ncol=2,byrow=T) # filled by row
C<-A%*%B # matrix multiplication
x=c(5,2)
t(C) # transpose of C
10*A # Hadamard product
