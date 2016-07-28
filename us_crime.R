us_crime <- read.csv("~/Downloads/us_crime.csv")
summary(us_crime)
fit<-lm(us_crime$Violence~ us_crime$Unemployment)
summary(fit)
plot(fit$residuals)
abline(h=0)

plot(us_crime$Unemployment, us_crime$Violence)
abline(fit)
