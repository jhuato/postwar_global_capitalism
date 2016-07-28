# data
x<-c(609, 629, 620, 564, 645, 493, 606, 629, 660, 630, 660, 629)
y<-c(241, 222, 233, 207, 247, 189, 226, 240, 226, 215, 226, 226)
z<-c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0)

regd<-lm(y~x+z)
summary(regd)
int0<-regd$coefficients[1]
int1<-regd$coefficients[1]+regd$coefficients[3]
int0
slop<-regd$coefficients[2]
slop
fit0<-cbind(x, int0+slop*x)
fit1<-cbind(x, int1+slop*x)
fit0
fit1

plot(x,y)
lines(fit0, col="red")
lines(fit1, col="blue")
legend("topleft", legend=c("z=1", "z=0"), col=c("blue", "red"))
require(scatterplot3d)
scatterplot3d(x,z,y, main="3D Scatterplot", highlight.3d=TRUE)
