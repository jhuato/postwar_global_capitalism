x<-1:20
w<-1+sqrt(x)/2
dummy<-data.frame(x=x,y=x+rnorm(x)*w)
View(dummy)
fm<-lm(y~x, data=dummy)
summary(fm)
attach(dummy)
lrf<-lowess(dummy$x,dummy$y) # I need this function to fit a polynominal curve on e, rho, r, etc.
plot(dummy$x,dummy$y)
lines(dummy$x,lrf$y)
abline(0, 1, lty=3) # Intercept=0, slope=1, dotted straight line
abline(coef(fm),col="red")  # the straight line fitted by OLS in red
