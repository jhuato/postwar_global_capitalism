# DH GC
# Julio Huato
# June 30, 2016
usda_ers <- read.csv("~/Downloads/usda_ers.csv")
yr <- usda_ers$year
y <- usda_ers$real.gdp.billions.2010.USD
grln <- usda_ers$real.gdp.annual.growth.rate.linear
grlg <- usda_ers$real.gdp.annual.growth.rate.logarithmic
ym <- mean(y)
grlnm <- mean(grln, na.rm = TRUE)
grlgm <- mean(grlg, na.rm = TRUE)
ysd <- sd(y)
grlnsd <- sd(grln, na.rm = TRUE)
grlgsd <- sd(grlg, na.rm = TRUE)

plot(yr, y, type="l", lwd=2)
logyhat <- lm(log(y) ~ yr)
logyhat$coefficients
yhat <- exp(fitted.values(logyhat))
lines(yr,yhat, col="red")
legend("topleft", c("Annual trend: 2.99%"), lty=1, col="red") 


fity <- lm(log(y)~yr)

plot(predict(fity), type="l")
lines(predict(fity), col="red")

y.ts <- ts(y, start=1969, end=2014, frequency=1)
plot(y.ts)
ycomp <- HoltWinters(y.ts, gamma=FALSE)
plot(ycomp)

grln.ts <- ts(grln, start=1970, end=2014, frequency=1)
plot(grln.ts)
fitgrln <- lm(grln.ts~yr[c(-1)])
abline(fitgrln, col="red")

grlg.ts <- ts(grlg, start=1970, end=2014, frequency=1)
plot(grlg.ts)
fitgrlg <- lm(grlg.ts~yr[c(-1)])
abline(fitgrlg, col="red")

hist(grln, breaks = 20, prob=TRUE, col="light gray", xlab="Global real GDP growth (linear)", ylim=c(0, .6), main=" ")
curve(dnorm(x, mean=grlnm, sd=grlnsd), col="blue", add=TRUE,xlim = c(-2,6), ylim=c(0,.6))
lines(density(grln, na.rm=TRUE), col="red")

hist(grlg, breaks = 20, prob=TRUE, col="light gray", xlab="Global real GDP growth (log)", ylim=c(0, .6), main=" ")
curve(dnorm(x, mean=grlgm, sd=grlgsd), col="blue", add=TRUE,xlim = c(-2,6), ylim=c(0,.6))
lines(density(grlg, na.rm=TRUE), col="red")
