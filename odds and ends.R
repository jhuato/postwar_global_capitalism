ngyeexp<-lm(log(pwt9nwg$wye) ~ c(1950:2014))
summary(ngyeexp) 
plot(c(1950:2014),pwt9nwg$wye, type="l", col="black", lwd=2, main="N-weighted global Y/E", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.w.")
lines(c(1950:2014), exp(fitted(ngyeexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-weighted global Y/E","Y/E trend=exp(-24.54-0.0173*Yr)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(ngyeexp), type="l", col="black", lwd=2, main="N-weighted global Y/E: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)

ngylexp<-lm(log(pwt9nwg$wyl) ~ c(1950:2014))
summary(ngylexp) 
plot(c(1950:2014),pwt9nwg$wyl, type="l", col="black", lwd=2, main="N-weighted global Y/L", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.h. ")
lines(c(1950:2014), exp(fitted(ngylexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-weighted global Y/L","Y/L trend=exp(-21.91-0.0125*Yr)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(ngylexp), type="l", col="black", lwd=2, main="N-weighted global Y/L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(ngylexp$residuals)

ngyalexp<-lm(log(pwt9nwg$wyal) ~ c(1950:2014))
summary(ngyalexp) 
plot(c(1950:2014),pwt9nwg$wyal, type="l", col="black", lwd=2, main="N-w H-adj global Y/L", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), exp(fitted(ngyalexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w H-adj global Y/L","Y/L trend=exp(-17.89-0.01*Yr)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(ngyalexp), type="l", col="black", lwd=2, main="N-w H-adj global Y/L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(ngyalexp$residuals)

nglabshlin<-lm(pwt9nwg$wlabsh ~ c(1950:2014))
summary(nglabshlin) 
plot(c(1950:2014),pwt9nwg$wlabsh, type="l", col="black", lwd=2, main="N-weighted global W/Y", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(nglabshlin), type="l", lwd=2, col="black", lty=3)
legend("topright", c("N-w global W/Y","W/Y trend= 3.95 - 0.0017*Yr"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))
# Statistically significant decrease in omega!
plot(c(1950:2014),residuals(nglabshlin), type="l", col="black", lwd=2, main="N-weighted global W/Y: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(nglabshlin$residuals)

