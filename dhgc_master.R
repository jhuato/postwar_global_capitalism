# DHGC Julio Huato
# PWT
cat("\014") # This clears the consol
library(plyr)

# Loads, summarizes data --------------------------------------------------
pwt9 <- read.csv("~/Downloads/pwt9.csv") # data
str(pwt9) # data frame structure
summary(pwt9) # i = 1, \ldots, 182 (countries), t = 1950, ..., 2014 (65 years)
# n = i \times t = 11830 obs.

# Diagnostics on omega and c ranges ---------------------------------------
range(pwt9$labsh, na.rm=TRUE) # checks the range of \omega
range(pwt9$csh_c, na.rm=TRUE) # checks the range of c
coutl <- pwt9[ which(csh_c<0 | csh_c > 1),] # the c outliers
summary(coutl) # small
hist(coutl$csh_c, breaks=100) 
coutln <- pwt9[ which(csh_c<0),] # the 4 negative c outliers (Bermuda 1999-2001,2003)
summary(coutln) 
# There are 240 positive outliers, most just shy of 1.  
# I can expand the acceptable range to 1.5.
coutlpp <- pwt9[ which(csh_c>1.5),] # the very positive c outliers c > 1.5
summary(coutlpp) # 30 very positive c outliers

# PRIMARY VARIABLES: CALCULATION ------------------------------------------

pwt9$Y <- (pwt9$rgdpe + pwt9$rgdpo)/2 # average of expenditure and output side of RGDP
pwt9$W <- (pwt9$labsh * pwt9$Y) # Wage
pwt9no <- pwt9
pwt9no$csh_c[pwt9no$csh_c<0 | pwt9no$csh_c>1] <- NA
summary(pwt9no) # dataframe with no c outliers
pwt9no$C <- (pwt9no$csh_c * pwt9no$rgdpo) # Consumption
summary(pwt9no) # dataframe with no c outliers

# PRIMARY VARIABLES: GLOBALLY AGGREGATED ----------------------------------

yrN<-tapply(pwt9no$pop, pwt9no$year, sum, na.rm=TRUE) # Population
Nexp<-lm(log(yrN) ~ c(1950:2014))
summary(Nexp)
plot(c(1950:2014),yrN, type="l", col="black", lwd=2, main="Global N", sub="Source: PWT 9.0", xlab="Year", ylab="Millions", ylim= c(1000,9000))
lines(c(1950:2014), exp(fitted(Nexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global N","Trend = exp(-36.16 + 0.0224*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(Nexp), type="l", col="black", lwd=2, main="Log of global N: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)

yrE<-tapply(pwt9no$emp, pwt9no$year, sum, na.rm=TRUE) # Employment
Eexp<-lm(log(yrE) ~ c(1950:2014))
summary(Eexp) # Incorporation of women into the labor force
plot(c(1950:2014),yrE, type="l", col="black", lwd=2, main="Global E", sub="Source: PWT 9.0", xlab="Year", ylab="Millions", ylim=c(500,4000))
lines(c(1950:2014), exp(fitted(Eexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global E","Trend = exp(-43.49 + 0.0257*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(Eexp), type="l", col="black", lwd=2, main="Log of global E: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)

pwt9no$L <- pwt9no$emp*pwt9no$avh # Labor hours
yrL<-tapply(pwt9no$L, pwt9no$year, sum, na.rm=TRUE)
Lexp<-lm(log(yrL) ~ c(1950:2014))
summary(Lexp) # Incorporation of women into the labor force
plot(c(1950:2014),yrL, type="l", col="black", lwd=2, main="Global L", sub="Source: PWT 9.0", xlab="Year", ylab="Million hours", ylim=c(500000,4500000))
lines(c(1950:2014), exp(fitted(Lexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global L","Trend = exp(-46.12 + 0.03*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(Lexp), type="l", col="black", lwd=2, main="Log of global L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
# Data availability accounts for sudden jumps
# For E, L, and H-adjusted L, subset 1970-2014 and 1980-2014

pwt9no$aL <- pwt9no$L*pwt9no$hc # H-adjusted Labor hours
yraL<-tapply(pwt9no$aL, pwt9no$year, sum, na.rm=TRUE)
aLexp<-lm(log(yraL) ~ c(1950:2014))
summary(aLexp) # Incorporation of women into the labor force
plot(c(1950:2014),yraL, type="l", col="black", lwd=2, main="Global H-adjusted L", sub="Source: PWT 9.0", xlab="Year", ylab="Index", ylim=c(1e+06,1.1e+07))
lines(c(1950:2014), exp(fitted(aLexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global H-adjusted L","Trend = exp(-50.14 + 0.0329*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(aLexp), type="l", col="black", lwd=2, main="Log of global H-adjusted L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
# Smoother than L, because the countries that joined global capitalism along the way were the small ones!
# Data availability accounts for sudden jumps

yrY<-tapply(pwt9no$Y, pwt9no$year, sum, na.rm=TRUE) # Y
yrrgdpo<-tapply(pwt9no$rgdpo, pwt9no$year, sum, na.rm=TRUE) # Yo
Yexp<-lm(log(yrY) ~ c(1950:2014))
summary(Yexp)
plot(c(1950:2014),yrY, type="l", col="black", lwd=2, main="Global Y", sub="Source: PWT 9.0", xlab="Year", ylab="Millions 2005 $")
lines(c(1950:2014), exp(fitted(Yexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global Y","Trend = exp(-68.03 + 0.0429*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(Yexp), type="l", col="black", lwd=2, main="Log of global Y: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)

yrK<-tapply(pwt9no$rkna, pwt9no$year, sum, na.rm=TRUE) # K
Kexp<-lm(log(yrK) ~ c(1950:2014))
summary(Kexp)
plot(c(1950:2014),yrK, type="l", col="black", lwd=2, main="Global K", sub="Source: PWT 9.0", xlab="Year", ylab="Millions 2005 $")
lines(c(1950:2014), exp(fitted(Kexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global K","K trend = Exp(-73.00 + 0.04609*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(Kexp), type="l", col="black", lwd=2, main="Log of global K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)

yrW<-tapply(pwt9no$W, pwt9no$year, sum, na.rm=TRUE) # Y, W, and C
Wexp<-lm(log(yrW) ~ c(1950:2014))
summary(Wexp)
yrC<-tapply(pwt9no$C, pwt9no$year, sum, na.rm=TRUE)
Cexp<-lm(log(yrC) ~ c(1950:2014))
summary(Cexp)

plot(c(1950:2014),yrY, type="l", col="black", lwd=3, main="Global Y, W, and C", sub="Source: PWT 9.0", xlab="Year", ylab="Millions 2005 $")
lines(c(1950:2014), exp(fitted(Yexp)), type="l", lwd=3, col="black", lty=3)
lines(c(1950:2014),yrW, type="l", col="black", lwd=2)
lines(c(1950:2014), exp(fitted(Wexp)), type="l", lwd=2, col="black", lty=3)
lines(c(1950:2014),yrC, type="l", col="black", lwd=1)
lines(c(1950:2014), exp(fitted(Cexp)), type="l", lwd=1, col="black", lty=3)
legend("topleft", c("Y","Y trend: 4.29% p.a.", "W","W trend: 4.00% p.a.", "C","C trend: 4.11% p.a."), lty=c(1,3,1,3,1,3), lwd=c(3,3,2,2,1,1), col=c("black","black","black","black","black","black"))
# Clearly, the global rate of exploitation increased, whether measured on W or C.
# But K expanded faster.  What happened to r?

plot(c(1950:2014), log(yrY), type="l", col="black", lwd=3, main="Logs of global Y, W, C", sub="Source: PWT 9.0", xlab="Year", ylab="Log of index", ylim=c(15,19))
lines(c(1950:2014), log(yrW), type="l", lwd=2, col="black")
lines(c(1950:2014), log(yrC), type="l", lwd=1, col="black")
legend("topleft", c("Y", "W", "C"), lwd=c(3,2,1), lty=c(1,1,1), col=c("black","black", "black"))
# Global (i.e. weighted by $): C runs slightly ahead of W.

plot(c(1950:2014),log(yrY), type="l", col="black", lwd=3, main="Logs of global Y, W, C, and trends", sub="Source: PWT 9.0", xlab="Year", ylab="Log of index", ylim=c(15,19))
lines(c(1950:2014), fitted(Yexp), type="l", lwd=3, lty=2, col="black")
lines(c(1950:2014), log(yrW), type="l", lwd=2, col="black")
lines(c(1950:2014), fitted(Wexp), type="l", lwd=2, lty=2, col="black")
lines(c(1950:2014), log(yrC), type="l", lwd=1, col="black")
lines(c(1950:2014), fitted(Cexp), type="l", lwd=1, lty=2, col="black")
legend("topleft", c("Y", "Y trend 4.29% p.a.", "W", "W trend: 4.00% p.a.", "C", "C trend: 4.11% p.a."), lwd=c(3,3,2,2,1,1), lty=c(1,2,1,2,1,2), col=c("black","black", "black"))

# SECONDARY VARIABLES: GLOBALLY AGGREGATED --------------------------------

yry <- yrY/yrN # Y/N per-capita Y
yexp<-lm(log(yry) ~ c(1950:2014))
summary(yexp) 
plot(c(1950:2014),yry, type="l", col="black", lwd=3, main="Global Y/N", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.c.")
lines(c(1950:2014), exp(fitted(yexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global Y/N","Trend = exp(-31.88 + 0.0205*Year)"), lwd=c(3,3), lty=c(1,3), col=c("black","black"))
# Note how sensitive trend growth rates are to choice of period.

plot(c(1950:2014),residuals(yexp), type="l", col="black", lwd=2, main="Global Y/N: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
# Per-capita. 1950-1960: down. 1961-1973: up.  
# 1974-1987: down.  1988-2004: stagnation.  2005-2012: up.

yrye <- yrY/yrE # Y/E per-worker Y
yeexp<-lm(log(yrye) ~ c(1950:2014))
summary(yeexp) 
plot(c(1950:2014),yrye, type="l", col="black", lwd=3, main="Global Y/E", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.w.")
lines(c(1950:2014), exp(fitted(yeexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global Y/E","Trend = exp(-24.55 + 0.017*Year)"), lwd=c(3,3), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(yeexp), type="l", col="black", lwd=2, main="Global Y/E: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
# Dependency (child birth) has decreased a lot.  That's why Y/E is much slower than Y/N
# Similar story to Y/N
# Note the autocorrelation

yryl <- yrY/yrL # Y/L per-hour Y
ylexp<-lm(log(yryl) ~ c(1950:2014))
summary(ylexp) 
plot(c(1950:2014),yryl, type="l", col="black", lwd=3, main="Global Y/L", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.h.")
lines(c(1950:2014), exp(fitted(ylexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global Y/L","Trend = exp(-21.9 + 0.0125*Year)"), lwd=c(3,3), lty=c(1,3), col=c("black","black"))
# 1970 data availability!  Chop off before 1970!
# If we take 1970 and after, much more dynamism.

plot(c(1950:2014),residuals(ylexp), type="l", col="black", lwd=2, main="Global Y/L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
# Similar story as p.c and p.w.

yryal <- yrY/yraL # Y/H-adj L (per-H-adjusted-hour Y)
yalexp<-lm(log(yryal) ~ c(1950:2014))
summary(yalexp) 
plot(c(1950:2014),yryal, type="l", col="black", lwd=3, main="Global Y/H-adjusted L", sub="Source: PWT 9.0", xlab="Year", ylab="Index")
lines(c(1950:2014), exp(fitted(yalexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global Y/H-adjusted L","Trend = exp(-17.89 + 0.01*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))
# 1970 data availability!  Chop off before 1970!
# If we take 1970 and after, much more dynamism.  Same as Y/L!!

plot(c(1950:2014),residuals(yalexp), type="l", col="black", lwd=2, main="Global Y/H-adjusted L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
# Similar story as p.c and p.w.

yrk <- yrK/yrN # K/N per-capita K
kexp<-lm(log(yrk) ~ c(1950:2014))
summary(kexp) 
plot(c(1950:2014),yrk, type="l", col="black", lwd=3, main="Global K/N", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.c.")
lines(c(1950:2014), exp(fitted(kexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global K/N","Trend = exp(-36.84 + 0.02366*Year)"), lwd=c(3,3), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(kexp), type="l", col="black", lwd=2, main="Global K/N: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
# Clear cycle, if one removes the period before 1960.

yrke <- yrK/yrE # per-worker K
keexp<-lm(log(yrke) ~ c(1950:2014))
summary(keexp) 
plot(c(1950:2014),yrke, type="l", col="black", lwd=3, main="Global K/E", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.w.")
lines(c(1950:2014), exp(fitted(keexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global K/E","Trend =exp(-29.51 + 0.0204*Year)"), lwd=c(3,3), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(keexp), type="l", col="black", lwd=2, main="Global K/E: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
# Clear cycle, if one removes the period before 1960. Similar.

yrkl <- yrK/yrL # per-hour L
klexp<-lm(log(yrkl) ~ c(1950:2014))
summary(klexp) 
plot(c(1950:2014),yrkl, type="l", col="black", lwd=3, main="Global K/L", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.h.")
lines(c(1950:2014), exp(fitted(klexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global K/L","Trend =exp(-26.87 + 0.0156*Year)"), lwd=c(3,3), lty=c(1,3), col=c("black","black"))
# Data availability 1970!

plot(c(1950:2014),residuals(klexp), type="l", col="black", lwd=2, main="Global K/L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
# Clear cycle, if one removes the period before 1960. Similar.

yrkal <- yrK/yraL # per-H-adj-hour K
kalexp<-lm(log(yrkal) ~ c(1950:2014))
summary(kalexp) 
plot(c(1950:2014),yrkal, type="l", col="black", lwd=3, main="Global K/H-adjusted L", sub="Source: PWT 9.0", xlab="Year", ylab="Index")
lines(c(1950:2014), exp(fitted(kalexp)), type="l", lwd=3, col="black", lty=3)
legend("topleft", c("Global K/H-adjusted L","Trend =exp(-22.85 + 0.0132*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(kalexp), type="l", col="black", lwd=2, main="Global K/H-adjusted L: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
# Similar to previous.

yrlabsh <- yrW/yrY # omega (wage share)
labshlin<-lm(yrlabsh ~ c(1950:2014))
summary(labshlin) 
plot(c(1950:2014),yrlabsh, type="l", col="black", lwd=3, main="Global W/Y", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(labshlin), type="l", lwd=3, col="black", lty=3)
legend("topright", c("Global W/Y","Trend = 3.95 - 0.0017*Year"), lwd=c(3,3), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(labshlin), type="l", col="black", lwd=2, main="Global W/Y: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
# Clear cycle, if one removes the period before 1960. Similar.

yre <- (yrY-yrW)/yrW # e (rate of exploitation)
eexp<-lm(log(yre) ~ c(1950:2014))
summary(eexp) 
plot(c(1950:2014),yre, type="l", col="black", lwd=2, main="Global (Y-W)/W", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), exp(fitted(eexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global W/(Y-W)","Trend = exp(-14.21 + 0.007*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))
# What happened 1985-1991 to stagnate e?

plot(c(1950:2014),residuals(eexp), type="l", col="black", lwd=2, main="Global (Y-W)/W: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)

elin<-lm(yre ~ c(1950:2014))
summary(elin) 
plot(c(1950:2014),yre, type="l", col="black", lwd=2, main="Global (Y-W)/W", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(elin), type="l", lwd=2, col="black", lty=3) # linear trend
legend("topleft", c("Global W/(Y-W)","Trend = -9.348 + 0.0051*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(elin), type="l", col="black", lwd=2, main="Global W/(Y-W): Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)

yreps <- (yrrgdpo-yrC)/yrC # eps (C-based rate of exploitation)
epsexp<-lm(log(yreps) ~ c(1950:2014))
summary(epsexp)
plot(c(1950:2014),yreps, type="l", col="black", lwd=2, main="Global C/(Yo-W)", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), exp(fitted(epsexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global (Yo-C)/C","Trend = exp(-9.887 + 0.00479*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))
# What happened 1970-2000 to stagnate eps?

plot(c(1950:2014),residuals(epsexp), type="l", col="black", lwd=2, main="Global (Yo-C)/C: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)

epslin<-lm(yreps ~ c(1950:2014))
summary(epslin) 
plot(c(1950:2014),yreps, type="l", col="black", lwd=2, main="Global (Yo-C)/C", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(epslin), type="l", lwd=2, col="black", lty=3) # linear trend
legend("topleft", c("Global (Yo-C)/C","Trend = -5.498 + 0.0031*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(epslin), type="l", col="black", lwd=2, main="Global (Yo-C)/C: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)

yrc <- yrC/yrrgdpo # C/Yo = c (consumption share)
clin<-lm(yrc ~ c(1950:2014))
summary(clin) 
plot(c(1950:2014),yrc, type="l", col="black", lwd=2, main="Global C/Yo", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(clin), type="l", lwd=2, col="black", lty=3)
legend("topright", c("Global C/Yo","C/Yo trend = 2.857 - 0.00114*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(clin), type="l", col="black", lwd=2, main="Global C/Yo: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)

yrrho <- yrY/yrK # rho = Y/K (capital productivity)
rholin<-lm(yrrho ~ c(1950:2014))
summary(rholin) 
plot(c(1950:2014),yrrho, type="l", col="black", lwd=2, main="Global Y/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(rholin), type="l", lwd=2, col="black", lty=3)
legend("topright", c("Global Y/K","Y/K trend = 2.139 -0.0093*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(rholin), type="l", col="black", lwd=2, main="Global Y/K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
# rho drop dramatically 1973-1983
# Plateu before 1973 and after 1983

yrr <- (yrY-yrW)/yrK # r = (Y-W)/K (profit rate)
rlin<-lm(yrr ~ c(1950:2014))
summary(rlin) 
plot(c(1950:2014),yrr, type="l", col="black", lwd=2, main="Global (Y-W)/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(rlin), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global (Y-W)/K","Trend = -1.266 + 0.00013*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(rlin), type="l", col="black", lwd=2, main="Global (Y-W)/K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
# Global r has increased a tiny bit, but statistically significant!

yrkap <- (yrrgdpo-yrC)/yrK # kappa = (Yo-C)/K
kaplin<-lm(yrkap ~ c(1950:2014))
summary(kaplin) 
plot(c(1950:2014),yrkap, type="l", col="black", lwd=2, main="Global (Yo-C)/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(kaplin), type="l", lwd=2, col="black", lty=3)
legend("topright", c("Global (Yo-C)/K","Trend=1.554-0.00019*Yr (NSS)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))
# Not significant downtrend! Stagnant over 1950-2014!

plot(c(1950:2014),residuals(rlin), type="l", col="black", lwd=2, main="Global (Y-W)/K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)

plot(c(1950:2014),yrlabsh, type="l", col="black", lwd=3, main="Global W/Y and C/Yo", sub="Source: PWT 9.0", xlab="Year", ylab="", ylim=c(.5,.72))
lines(c(1950:2014), yrc, type="l", lwd=1, col="black")
legend("topright", c("W/Y","C/Yo"), lwd=c(3,1), lty=c(1,1), col=c("black","black"))

plot(c(1950:2014),yre, type="l", col="black", lwd=3, main="Global (Y-W)/W and (Yo-C)/C", sub="Source: PWT 9.0", xlab="Year", ylab="", ylim=c(.45,.95))
lines(c(1950:2014), yreps, type="l", lwd=1, col="black")
legend("topleft", c("(Y-W)/W","(Yo-C)/C"), lwd=c(3,1), lty=c(1,1), col=c("black","black"))
# The ``official'' (W-based) rate of exploitation was flat 1970-late 1990s.
# It then increased in the 2000s.
# The C-based rate of exploitation actually dropped 1970-late 1990s, and
# then increased in the 2000s.  This is the period of BRICS expansion!
# Expanded access to credit for the working poor masked things temporarily, during the credit boom,
# but they chickes come home to roost... (See eps catching up with e by 2010.)
# C-exploitation rate is more volatile.
# Or did they?  Lots of write-offs (mainly US and Western Europe), though incredibly painful to workers.

plot(c(1950:2014),yrrho, type="l", col="black", lwd=3, main="Global Y/K, (Y-W)/K, and (Yo-C)/K", sub="Source: PWT 9.0", xlab="Year", ylab="", ylim = c(0.075,.375))
lines(c(1950:2014), yrr, type="l", lwd=2, col="black")
lines(c(1950:2014), yrkap, type="l", lwd=1, col="black")
legend("topright", c("Y/K", "(Y-W)/K","(Yo-C)/K"), lwd=c(3,2,1), lty=c(1,1,1), col=c("black","black", "black"))
# Globally, though pi (exploitation) has increased, that increase has not been
# able to arrest the decline in global rho, and as a result the decline of r, 
# kappa since 1970.

# INTERNATIONAL VARIABLES
# This computes country-year \Pi, I, \pi, 
# \rho, c, s, e, \epsilon, r, \kappa=I/K.
# For consumption and gross accum shares, using no-outliers data.
pwt9no$pi <- ((pwt9no$Y - pwt9no$W)/pwt9no$Y)
pwt9no$pi1 <- (1 - pwt9no$labsh)
cor(pwt9no$pi,pwt9no$pi1, use = "complete")

pwt9no$c <- (pwt9no$C/pwt9no$rgdpo)
pwt9no$e <- (pwt9no$Y-pwt9no$W)/pwt9no$W
pwt9no$rho <- (pwt9no$Y/pwt9no$rkna)
pwt9no$eps <- (pwt9no$rgdpo-pwt9no$C)/pwt9no$C
pwt9no$r <- (pwt9no$Y-pwt9no$W)/pwt9no$rkna
pwt9no$kap <- (pwt9no$rgdpo-pwt9no$C)/pwt9no$rkna

cor(pwt9no$r,pwt9no$kap, use = "complete")
cor(log(pwt9no$r),log(pwt9no$kap), use = "complete")
# My two proxies for the profit rate

# INTERNATIONAL (UNWEIGHTED MEANS, "ONE COUNTRY, ONE VOTE")
yrlabsh <-tapply(pwt9no$labsh, pwt9no$year, mean, na.rm=TRUE)
yrc<-tapply(pwt9no$csh_c, pwt9no$year, mean, na.rm=TRUE)
plot(c(1950:2014),yrlabsh, type="l", col="black", lwd=3,  main="Int'l (unweighted mean) W/Y and C/Yo", sub="Source: PWT 9.0", xlab="Year", ylim=c(.5,.7))
lines(c(1950:2014),yrc, type="l", col="black", lwd=1)
legend("topright", c("W/Y","C/Yo"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))
# This shows how important, globally, is my point about who has the upper hand
# when a credit crisis erupts.  It depends on political initiative, on whether
# the ideology of capital (the force of ownership) sticks or whether necessity
# prevails.

yrrho<-tapply(pwt9no$rho, pwt9no$year, mean, na.rm=TRUE)
plot(c(1950:2014), yrrho, type="l", col="black", lwd=2,  main="Int'l (unweighted mean) Y/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=mean(yrrho),lty=2, lwd=1)
legend("bottomleft", "1950-2011 Mean of (unw int'l mean) rho",lty=2, lwd=1,col="black")

yre<-tapply(pwt9no$e, pwt9no$year, mean, na.rm=TRUE)
yreps <-tapply(pwt9no$eps, pwt9no$year, mean, na.rm=TRUE)
plot(c(1950:2014),yre, type="l", col="black", lwd=3,  main="Int'l (unweighted mean) (Y-W)/W and (Yo-C)/C", sub="Source: PWT 9.0", xlab="Year", ylab="", ylim=c(.5,1.2))
lines(c(1950:2014),yreps, type="l", col="black", lwd=1)
legend("topleft", c("e=(Y-W)/W","epsilon=(Yo-C)/C"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

yrr <-tapply(pwt9no$r, pwt9no$year, mean, na.rm=TRUE)
yrkap <-tapply(pwt9no$kap, pwt9no$year, mean, na.rm=TRUE)
plot(c(1950:2014), yrrho, type="l", col="black", lwd=3,  main="Int'l (unweighted mean) Y/K, (Y-W)/K, and (Yo-C)/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ", ylim=c(0.1,0.6))
lines(c(1950:2014),yrr, type="l", col="black", lwd=2)
lines(c(1950:2014),yrkap, type="l", col="black", lwd=1)
legend("topright", c("Y/K", "(Y-W)/K","(Yo-C)/K"), lty=c(1,1,1), lwd=c(3,2,1),col=c("black","black", "black"))

# DISTRIBUTION OF INTERNATIONAL PRIMARY AND SECONDARY VARIABLES
# This subsets the relevant data for 1974, 1994, 2014:
Y74<-log(na.omit(subset(pwt9no$Y, pwt9no$year==1974)))
Y94<-log(na.omit(subset(pwt9no$Y, pwt9no$year==1994)))
Y14<-log(na.omit(subset(pwt9no$Y, pwt9no$year==2014)))

K74<-log(na.omit(subset(pwt9no$rkna, pwt9no$year==1974)))
K94<-log(na.omit(subset(pwt9no$rkna, pwt9no$year==1994)))
K14<-log(na.omit(subset(pwt9no$rkna, pwt9no$year==2014)))

W74<-log(na.omit(subset(pwt9no$W, pwt9no$year==1974)))
W94<-log(na.omit(subset(pwt9no$W, pwt9no$year==1994)))
W14<-log(na.omit(subset(pwt9no$W, pwt9no$year==2014)))

C74<-log(na.omit(subset(pwt9no$C, pwt9no$year==1974)))
C94<-log(na.omit(subset(pwt9no$C, pwt9no$year==1994)))
C14<-log(na.omit(subset(pwt9no$C, pwt9no$year==2014)))

labsh74<-log(na.omit(subset(pwt9no$labsh, pwt9no$year==1974)))
labsh94<-log(na.omit(subset(pwt9no$labsh, pwt9no$year==1994)))
labsh14<-log(na.omit(subset(pwt9no$labsh, pwt9no$year==2014)))

c74<-log(na.omit(subset(pwt9no$csh_c, pwt9no$year==1974)))
c94<-log(na.omit(subset(pwt9no$csh_c, pwt9no$year==1994)))
c14<-log(na.omit(subset(pwt9no$csh_c, pwt9no$year==2014)))

e74<-log(na.omit(subset(pwt9no$e, pwt9no$year==1974)))
e94<-log(na.omit(subset(pwt9no$e, pwt9no$year==1994)))
e14<-log(na.omit(subset(pwt9no$e, pwt9no$year==2014)))

eps74<-log(na.omit(subset(pwt9no$eps, pwt9no$year==1974)))
eps94<-log(na.omit(subset(pwt9no$eps, pwt9no$year==1994)))
eps14<-log(na.omit(subset(pwt9no$eps, pwt9no$year==2014)))

rho74<-log(na.omit(subset(pwt9no$rho, pwt9no$year==1974)))
rho94<-log(na.omit(subset(pwt9no$rho, pwt9no$year==1994)))
rho14<-log(na.omit(subset(pwt9no$rho, pwt9no$year==2014)))

r74<-log(na.omit(subset(pwt9no$r, pwt9no$year==1974)))
r94<-log(na.omit(subset(pwt9no$r, pwt9no$year==1994)))
r14<-log(na.omit(subset(pwt9no$r, pwt9no$year==2014)))

kap74<-log(na.omit(subset(pwt9no$kap, pwt9no$year==1974)))
kap94<-log(na.omit(subset(pwt9no$kap, pwt9no$year==1994)))
kap14<-log(na.omit(subset(pwt9no$kap, pwt9no$year==2014)))

# Multi plot function: input of the function MUST be a numeric list
plot.multi.dens <- function(s)
{
  junk.x = NULL
  junk.y = NULL
  for(i in 1:length(s))
  {
    junk.x = c(junk.x, density(s[[i]])$x)
    junk.y = c(junk.y, density(s[[i]])$y)
  }
  xr <- range(junk.x)
  yr <- range(junk.y)
  plot(density(s[[1]]), xlim = xr, ylim = yr, main = "", xlab = "")
  for(i in 1:length(s))
  {
    lines(density(s[[i]]), xlim = xr, ylim = yr, lwd=i)
  }
}

plot.multi.dens(list(Y74,Y94,Y14))
title(main = "Density of log of Y_i (i=1, ..., 182)", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(K74,K94,K14))
title(main = "Density of log of K_i (i=1, ..., 182)", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(W74,W94,W14))
title(main = "Density of log of W_i (i=1, ..., 182)", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(C74,C94,C14))
title(main = "Density of log of C_i (i=1, ..., 182)", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(labsh74,labsh94,labsh14))
title(main = "Density of log of omega_i", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(c74,c94,c14))
# segments(x0=mean(c74),y0=0,x1=mean(c74),y1=1.5,col="black", lwd=1, lty=2)
# segments(x0=mean(c94),y0=0,x1=mean(c94),y1=1.5,col="black", lwd=2, lty=2)
# segments(x0=mean(c14),y0=0,x1=mean(c14),y1=1.5,col="black", lwd=3, lty=2)
title(main = "Density of log of c_i", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))
# In int'l unweighted, c drops in 2014 compared to 1994 because of outliers
# on the left tail: Which countries did this?

plot.multi.dens(list(e74,e94,e14))
title(main = "Density of log of e_i", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(eps74,eps94,eps14))
title(main = "Density of log of epsilon_i", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(rho74,rho94,rho14))
title(main = "Density of log of rho_i", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))
# Dispersion decreases

plot.multi.dens(list(r74,r94,r14))
title(main = "Density of log of r_i", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(kap74,kap94,kap14))
title(main = "Density of log of kappa_i", sub="Source: PWT 9.0")
legend("topleft",legend=c("1974","1994","2014"), lwd=(1:3),lty =c(1,1,1))

hist(labsh74, main="Histogram of log of omega 1974", breaks=seq(-2,0,by=.1), col="blue")
hist(labsh94, main="Histogram of log of omega 1994", breaks=seq(-2,0,by=.1), col="blue")
hist(labsh14, main="Histogram of log of omega 2014", breaks=seq(-2,0,by=.1), col="blue")

hist(e74, main="Histogram of log of e 1974", breaks=seq(-2,2,by=.2), col="red")
hist(e94, main="Histogram of log of e 1994", breaks=seq(-2,2,by=.2), col="red")
hist(e14, main="Histogram of log of e 2014", breaks=seq(-2,2,by=.2), col="red")

hist(eps74, main="Histogram of log of epsilon 1974", breaks=seq(-5,4,by=.2), col="dark gray", xlim = c(-4,3))
hist(eps94, main="Histogram of log of epsilon 1994", breaks=seq(-5,4,by=.2), col="dark gray", xlim = c(-4,3))
hist(eps14, main="Histogram of log of epsilon 2014", breaks=seq(-5,4,by=.2), col="dark gray", xlim = c(-4,3))

hist(rho74, main="Histogram of log of rho 1974", breaks=seq(-4,4,by=.2), col="green", xlim = c(-4,4))
hist(rho94, main="Histogram of log of rho 1994", breaks=seq(-4,4,by=.2), col="green", xlim = c(-4,4))
hist(rho14, main="Histogram of log of rho 2014", breaks=seq(-4,4,by=.2), col="green", xlim = c(-4,4))
# Note the reduced dispersion!  Globalization?

hist(r74, main="Histogram of log of r 1974", breaks=20, col="gray", xlim = c(-4,2))
hist(r94, main="Histogram of log of r 1994", breaks=20, col="gray", xlim = c(-4,2))
hist(r14, main="Histogram of log of r 2014", breaks=20, col="gray", xlim = c(-4,2))
# Note the reduced dispersion: globalized finance.

hist(kap74, main="Histogram of log of kappa 1974", breaks=seq(-6,4,by=.2), col="gray", xlim = c(-4,2))
hist(kap94, main="Histogram of log of kappa 1994", breaks=seq(-6,4,by=.2), col="gray", xlim = c(-4,2))
hist(kap14, main="Histogram of log of kappa 2014", breaks=seq(-6,4,by=.2), col="gray", xlim = c(-4,2))


# GLOBAL VARIABLES AGAIN --------------------------------------------------
# Subsetting to remove derived variables.
pwt9nog <- subset(pwt9no, select = c(year, pop, emp, L, aL, Y, rgdpo, rkna, W, C))
str(pwt9nog)
summary(pwt9nog)

# Aggregation (by sum) of global primary variables.
pwt9g <-aggregate(pwt9nog, by=list(pwt9nog$year), na.rm=TRUE, "sum")
str(pwt9g)
summary(pwt9g)

# THERE IS AN ERROR SOMEWHERE HERE.

pwt9g$yrY <- yrY
pwt9g$yrrgdpo <- yrrgdpo
pwt9g$yrK <- yrr
pwt9g$yrlabsh <- yrlabsh
pwt9g$yrc <- yrc
pwt9g$yrW <- yrW
pwt9g$yrC <- yrC


# Global secondary variables.
pwt9g$y <- pwt9g$Y/pwt9g$pop
pwt9g$ye <- pwt9g$Y/pwt9g$emp
pwt9g$yl <- pwt9g$Y/pwt9g$L
pwt9g$yal <- pwt9g$Y/pwt9g$aL
pwt9g$labsh <- pwt9g$W/pwt9g$Y
pwt9g$c <- pwt9g$C/pwt9g$rgdpo
pwt9g$e <- (pwt9g$Y - pwt9g$W)/pwt9g$W
pwt9g$eps <- (pwt9g$rgdpo - pwt9g$C)/pwt9g$C
pwt9g$rho <- pwt9g$Y/pwt9g$rkna
pwt9g$r <- (pwt9g$Y - pwt9g$W)/pwt9g$rkna
pwt9g$kap <- (pwt9g$rgdpo - pwt9g$C)/pwt9g$rkna

cor(yrY,pwt9g$Y)
cor(yrrgdpo,pwt9g$rgdpo)
cor(yraL,pwt9g$aL)
cor(yryal,pwt9g$yal)
cor(yrK,pwt9g$rkna)
cor(yrC,pwt9g$C)
cor(yrW,pwt9g$W)
cor(yrlabsh,pwt9g$labsh)
cor(yrc,pwt9g$c)
cor(yrkap,pwt9g$kap)
cor(yre,pwt9g$e)
cor(yrrho,pwt9g$rho)
cor(yrr,pwt9g$r)


# INTERNATIONAL WEIGHTED BY POP
View(pwt9nog)
str(pwt9nog)
pwt9nwg <- ddply(pwt9nog, .(year), function(x) data.frame(wY=weighted.mean(x$Y, x$pop, na.rm=TRUE), wrgdpo=weighted.mean(x$rgdpo, x$pop, na.rm=TRUE), wK=weighted.mean(x$rkna, x$pop, na.rm=TRUE), wW=weighted.mean(x$W, x$pop, na.rm=TRUE), wC=weighted.mean(x$C, x$pop, na.rm=TRUE)))
str(pwt9nwg)
summary(pwt9nwg)
View(pwt9nwg)

wgYexp<-lm(log(pwt9nwg$wY) ~ c(1950:2014))
summary(wgYexp)
plot(c(1950:2014),pwt9nwg$wY, type="l", col="black", lwd=2, main="N-w international Y", sub="Source: PWT 9.0", xlab="Year", ylab="Index")
lines(c(1950:2014), exp(fitted(wgYexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w int'l Y","Y trend = exp(-61.38 + 0.0381*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wgYexp), type="l", col="black", lwd=2, main="Log of N-w international Y: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
View(wgYexp$residuals)

wgKexp<-lm(log(pwt9nwg$wK) ~ c(1950:2014))
summary(wgKexp)
plot(c(1950:2014),pwt9nwg$wK, type="l", col="black", lwd=2, main="N-w international K", sub="Source: PWT 9.0", xlab="Year", ylab="Index")
lines(c(1950:2014), exp(fitted(wgKexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w int'l K","K trend = exp(-69.26 + 0.0426*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wgKexp), type="l", col="black", lwd=2, main="Log of N-w international K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
View(wgKexp$residuals)

wgWexp<-lm(log(pwt9nwg$wW) ~ c(1950:2014))
summary(wgWexp)
plot(c(1950:2014),pwt9nwg$wW, type="l", col="black", lwd=2, main="N-w international W", sub="Source: PWT 9.0", xlab="Year", ylab="Index")
lines(c(1950:2014), exp(fitted(wgWexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w int'l W","W trend = exp(-59.44 + 0.0369*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wgWexp), type="l", col="black", lwd=2, main="Log of N-w international W: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
View(wgWexp$residuals)

wgCexp<-lm(log(pwt9nwg$wC) ~ c(1950:2014))
summary(wgCexp)
plot(c(1950:2014),pwt9nwg$wC, type="l", col="black", lwd=2, main="N-w international C", sub="Source: PWT 9.0", xlab="Year", ylab="Index")
lines(c(1950:2014), exp(fitted(wgCexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w int'l C","C trend = exp(-50.79 + 0.0325*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wgCexp), type="l", col="black", lwd=2, main="Log of N-w international C: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
abline(h=0, lwd=2, lty=3)
View(wgWexp$residuals)

plot(c(1950:2014),pwt9nwg$wY, type="l", col="black", lwd=3, main="N-w international Y, W, C", sub="Source: PWT 9.0", xlab="Year", ylab="Index")
lines(c(1950:2014), pwt9nwg$wW, type="l", lwd=2, col="black")
lines(c(1950:2014), pwt9nwg$wC, type="l", lwd=1, col="black")
legend("topleft", c("Y","W", "C"), lwd=c(3,2,1), lty=c(1,1,1), col=c("black","black", "black"))

plot(c(1950:2014),log(pwt9nwg$wY), type="l", col="black", lwd=3, main="Logs of N-w international Y, W, C", sub="Source: PWT 9.0", xlab="Year", ylab="Log of index", ylim=c(12.5,15.6))
lines(c(1950:2014), log(pwt9nwg$wW), type="l", lwd=2, col="black")
lines(c(1950:2014), log(pwt9nwg$wC), type="l", lwd=1, col="black")
legend("topleft", c("Y", "W", "C"), lwd=c(3,2,1), lty=c(1,1,1), col=c("black","black", "black"))
# Conjecture: 
# When you weight this by N so you give more weight to large countries (China, India, Indonesia, Brazil, etc.), 
# there's a lot of workers' saving (w.r.t. wages) going on, forced by weakness of financial systems.
# This is the opposite of the US, where workers consume above their wages.

plot(c(1950:2014),log(pwt9nwg$wY), type="l", col="black", lwd=3, main="Logs of N-w international Y, W, C, and trends", sub="Source: PWT 9.0", xlab="Year", ylab="Log of index", ylim=c(12,16))
lines(c(1950:2014), fitted(wgYexp), type="l", lwd=3, lty=2, col="black")
lines(c(1950:2014), log(pwt9nwg$wW), type="l", lwd=2, col="black")
lines(c(1950:2014), fitted(wgWexp), type="l", lwd=2, lty=2, col="black")
lines(c(1950:2014), log(pwt9nwg$wC), type="l", lwd=1, col="black")
lines(c(1950:2014), fitted(wgCexp), type="l", lwd=1, lty=2, col="black")
legend("topleft", c("Y", "Y trend: 3.81% p.a.", "W", "W trend: 3.69% p.a.", "C", "C trend: 3.25% p.a."), lwd=c(3,2,1), lty=c(2,2,2), col=c("black","black", "black"))

# N-w international W/Y, (Y-W)/W, C/rgdpo, (rgdpo-C)/C, Y/K, (Y-W)/K, (rgdpo-C)/K
pwt9nwg$ww <- pwt9nwg$wW/pwt9nwg$wY
View(pwt9nwg$ww)
wwlin<-lm(pwt9nwg$ww ~ c(1950:2014))
summary(wwlin) 
cor(pwt9nwg$ww, yrlabsh) # Checks correlation with global var
plot(c(1950:2014),pwt9nwg$ww, type="l", col="black", lwd=2, main="N-w international W/Y", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(wwlin), type="l", lwd=2, col="black", lty=3)
legend("bottomleft", c("N-w int'l W/Y","W/Y trend = 2.23 - 0.0008*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wwlin), type="l", col="black", lwd=2, main="N-w international W/Y: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(wwlin$residuals)

pwt9nwg$we <- (pwt9nwg$wY-pwt9nwg$wW)/pwt9nwg$wW
View(pwt9nwg$we)
welin<-lm(pwt9nwg$we ~ c(1950:2014))
summary(welin) 
cor(pwt9nwg$we, yre) # Checks correlation with global var
plot(c(1950:2014),pwt9nwg$we, type="l", col="black", lwd=2, main="N-w international (Y-W)/W", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(welin), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w int'l (Y-W)/W","(Y-W)/W trend = -3.02 - 0.002*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(welin), type="l", col="black", lwd=2, main="N-w international (Y-W)/W: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(welin$residuals)

# C/rgdpo, (rgdpo-C)/C, Y/K, (Y-W)/K, (rgdpo-C)/K
pwt9nwg$wc <- pwt9nwg$wC/pwt9nwg$wrgdpo
View(pwt9nwg$wc)
wclin<-lm(pwt9nwg$wc ~ c(1950:2014))
summary(wclin) 
cor(pwt9nwg$wc, yrc) # Checks correlation with global var
plot(c(1950:2014),pwt9nwg$wc, type="l", col="black", lwd=2, main="N-w international C/Y", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(wclin), type="l", lwd=2, col="black", lty=3)
legend("bottomleft", c("N-w int'l C/Yo","C/Yo trend = 7.268 - 0.00336*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wclin), type="l", col="black", lwd=2, main="N-w international C/Y: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(wclin$residuals)

pwt9nwg$weps <- (pwt9nwg$wrgdpo-pwt9nwg$wC)/pwt9nwg$wC
View(pwt9nwg$weps)
wepslin<-lm(pwt9nwg$weps ~ c(1950:2014))
summary(wepslin) 
cor(pwt9nwg$weps, yreps) # Checks correlation with global var
plot(c(1950:2014),pwt9nwg$weps, type="l", col="black", lwd=2, main="N-w international (Yo-C)/C", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(wepslin), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w int'l (Yo-C)/C","(Yo-C)/C trend = -18.17 - 0.0095*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(welin), type="l", col="black", lwd=2, main="N-w international (Y-W)/W: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(welin$residuals)

pwt9nwg$wrho <- pwt9nwg$wY/pwt9nwg$wK
View(pwt9nwg$wrho)
wrholin<-lm(pwt9nwg$wrho ~ c(1950:2014))
summary(wrholin) 
cor(pwt9nwg$wrho, yrrho) # Checks correlation with global var
plot(c(1950:2014),pwt9nwg$wrho, type="l", col="black", lwd=2, main="N-w international Y/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(wrholin), type="l", lwd=2, col="black", lty=3)
legend("topright", c("N-w int'l Y/K","Y/K trend = 3.43 - 0.0016*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wrholin), type="l", col="black", lwd=2, main="N-w international Y/K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(welin$residuals)

pwt9nwg$wr <- (pwt9nwg$wY-pwt9nwg$wW)/pwt9nwg$wK
View(pwt9nwg$wr)
wrlin<-lm(pwt9nwg$wr ~ c(1950:2014))
summary(wrlin) 
cor(pwt9nwg$wr, yrr) # Checks correlation with global var
plot(c(1950:2014),pwt9nwg$wr, type="l", col="black", lwd=2, main="N-w international (Y-W)/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(wrlin), type="l", lwd=2, col="black", lty=3)
legend("topright", c("N-w int'l (Y-W)/K","(Y-W)/K trend = 58.63 - 0.00024*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wrlin), type="l", col="black", lwd=2, main="N-w international (Y-W)/K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(wrlin$residuals)

pwt9nwg$wkap <- (pwt9nwg$wrgdpo-pwt9nwg$wC)/pwt9nwg$wK
View(pwt9nwg$wkap)
wkaplin<-lm(pwt9nwg$wkap ~ c(1950:2014))
summary(wkaplin) 
cor(pwt9nwg$wkap, yrkap) # Checks correlation with global var
plot(c(1950:2014),pwt9nwg$wkap, type="l", col="black", lwd=2, main="N-w international (Yo-C)/K", sub="Source: PWT 9.0", xlab="Year", ylab=" ")
lines(c(1950:2014), fitted(wkaplin), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-w int'l (Yo-C)/K","(Yo-C)/K trend = -91.52 + 0.00053*Year"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(wkaplin), type="l", col="black", lwd=2, main="N-w international (Yo-C)/K: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(wkaplin$residuals)

# W/Y and C/Yo together
plot(c(1950:2014),pwt9nwg$ww, type="l", col="black", lwd=3, main="N-w international W/Y and C/Yo", sub="Source: PWT 9.0", xlab="Year", ylim=c(.45,.75), ylab="")
lines(c(1950:2014), pwt9nwg$wc, type="l", lwd=1, col="black")
legend("bottomleft", c("N-w int'l W/Y","N-w int'l C/Yo"), lwd=c(2,2), lty=c(1,1), col=c("black","black"))

plot(c(1950:2014),pwt9nwg$ww, type="l", col="black", lwd=3, main="N-w international W/Y and C/Yo", sub="Source: PWT 9.0", xlab="Year", ylim=c(.45,.75), ylab="")
lines(c(1950:2014), fitted(wwlin), type="l", lwd=3, lty=2, col="black")
lines(c(1950:2014), pwt9nwg$wc, type="l", lwd=1, col="black")
lines(c(1950:2014), fitted(wclin), type="l", lwd=1, lty=2, col="black")
legend("bottomleft", c("N-w int'l W/Y","N-w int'l W/Y trend: -0.07765% p.a.","N-w int'l C/Yo","N-w int'l C/Yo trend: -0.3358% p.a."), lwd=c(3,3,1,1), lty=c(1,2,1,2), col=c("black","black"))

# (Y-W)/W and (Yo-C)/C together
plot(c(1950:2014),pwt9nwg$we, type="l", col="black", lwd=3, main="N-w international (Y-W)/W and (Yo-C)/C", sub="Source: PWT 9.0", xlab="Year", ylab="", ylim=c(.3,1.1))
lines(c(1950:2014), pwt9nwg$weps, type="l", lwd=1, col="black")
legend("topleft", c("N-w int'l (Y-W)/W","N-w int'l (Yo-C)/C"), lwd=c(2,2), lty=c(1,1), col=c("black","black"))

# The N-w rate of exploitation, when viewed in terms of actual C, is staggering.
# This suggests people in large countries forcing themselves to save to afford 
# durables (e.g. furnished housing) as a sort of insurance in the face of 
# weak social insurance.  Their savings don't effectively become capital for them,
# obviously.  But they become capital when concentrated under bank control.
# Informal financial networks get formalized via concentration by loan sharks,
# money laundering systems, etc.

plot(c(1950:2014),pwt9nwg$we, type="l", col="black", lwd=3, main="N-w international (Y-W)/W and (Yo-C)/C", sub="Source: PWT 9.0", xlab="Year", ylab="", ylim=c(.3,1.1))
lines(c(1950:2014), fitted(welin), type="l", lwd=3, lty=2, col="black")
lines(c(1950:2014), pwt9nwg$weps, type="l", lwd=1, col="black")
lines(c(1950:2014), fitted(wepslin), type="l", lwd=1, lty=2, col="black")
legend("topleft", c("N-w int'l (Y-W)/W", "N-w int'l (Y-W)/W trend: +0.175% p.a.", "N-w int'l (Yo-C)/C", "N-w int'l (Yo-C)/C trend: +0.950% p.a."), lwd=c(3,3,1,1), lty=c(1,2,1,2), col=c("black","black"))
summary(wepslin)
# Remarkable expansion in the ``effective'' (C-defined) rate of exploitation.
# It doesn't appear formally under W.

# Y/K, (Y-W)/K, and (Yo-C)/K together
plot(c(1950:2014),pwt9nwg$wrho, type="l", col="black", lwd=3, main="N-w international Y/K, (Y-W)/K, and (Yo-C)/K", sub="Source: PWT 9.0", xlab="Year", ylab="", ylim = c(0,.45))
lines(c(1950:2014), pwt9nwg$wr, type="l", lwd=2, col="black")
lines(c(1950:2014), pwt9nwg$wkap, type="l", lwd=1, col="black")
legend("topleft", c("N-w int'l Y/K", "N-w int'l (Y-W)/K","N-w int'l (Yo-C)/C"), lwd=c(3,2,1), lty=c(1,1,1), col=c("black","black", "black"))








ngyeexp<-lm(log(pwt9nwg$wye) ~ c(1950:2014))
summary(ngyeexp) 
plot(c(1950:2014),pwt9nwg$wye, type="l", col="black", lwd=2, main="N-weighted global Y/E", sub="Source: PWT 9.0", xlab="Year", ylab="2005 $ p.w.")
lines(c(1950:2014), exp(fitted(ngyeexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("N-weighted global Y/E","Y/E trend=exp(-24.54-0.0173*Yr)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

plot(c(1950:2014),residuals(ngyeexp), type="l", col="black", lwd=2, main="N-weighted global Y/E: Residuals", sub="Source: PWT 9.0", xlab="Year", ylab="")
abline(h=0, lwd=2, lty=3)
View(ngyeexp$residuals)

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


# TIME SERIES
pwt9nwg.ts <- ts(pwt9nwg, start=1950, end=2014, frequency=1) 

# RICH VS. POOR
# REGIONS (WB)


# Takes 2010 as base year for country classification:
pwt2010 <- subset(pwt9nog,year==2010)
summary(pwt2010)
dh_country_class <- read.csv("~/Downloads/dh_country_class.csv")
# dh_country <- read.csv("https://sites.google.com/site/juliohuatopwt/pwt81/dh_country_class.csv")
# Merges pop_2010 and dh_country:
dh_country <- dh_country_class
pwt9 <- merge(pwt2010, dh_country, by="countrycode")
summary(pwt9)

# The population threshold for the +/-30 n-largest countries:
n_pwt9 <- sort(-pwt9$pop)
View(n_pwt9) # n<45
pwt9$n_larg[pwt9$pop < 45] <- 0
pwt9$n_larg[pwt9$pop >= 40.5] <- 1
summary(pwt9$n_larg)

# The Y threshold for the +/-30 y-largest countries:
y_pwt9 <- sort(-pwt9$y)
# y_pwt10 # 3.9e+05
pwt10$y_larg[pwt10$y < 3.9e+05 ] <- 0
pwt10$y_larg[pwt10$y >= 3.9e+05 ] <- 1
summ_pwt10_y_larg <- summary(pwt10$y_larg)
# summ_pwt10_y_larg
# The Y/N threshold for the 30 ypc-largest countries:
ypc_pwt10 <- sort(-pwt10$ypc)
# ypc_pwt10 # 28000
pwt10$ypc_larg[pwt10$ypc < 28000 ] <- 0
pwt10$ypc_larg[pwt10$ypc >= 28000 ] <- 1
summ_pwt10_ypc_larg <- summary(pwt10$ypc_larg)
# summ_pwt10_ypc_larg
# The Y/E threshold for the 30 ypw-largest countries:
ypw_pwt10 <- sort(-pwt10$ypw)
# ypw_pwt10 # 60000
pwt10$ypw_larg[pwt10$ypw < 60000] <- 0
pwt10$ypw_larg[pwt10$ypw >= 60000] <- 1
summ_pwt10_ypw_larg <- summary(pwt10$ypw_larg)
# summ_pwt10_ypw_larg
# The Y/L threshold for the 10 tilde_y-largest countries:
tilde_y_pwt10 <- sort(-pwt10$tilde_y)
# tilde_y_pwt10 # 45
pwt10$tilde_y_larg[pwt10$tilde_y < 45 ] <- 0
pwt10$tilde_y_larg[pwt10$tilde_y >= 45 ] <- 1
summ_pwt10_tilde_y_larg <- summary(pwt10$tilde_y_larg)
# summ_pwt10_tilde_y_larg
# The K/N threshold for the 30 kpc-largest countries:
kpc_pwt10 <- sort(-pwt10$kpc)
# kpc_pwt10 # 82000
pwt10$kpc_larg[pwt10$kpc <  82000] <- 0
pwt10$kpc_larg[pwt10$kpc >= 82000] <- 1
summ_pwt10_kpc_larg <- summary(pwt10$kpc_larg)
# summ_pwt10_kpc_larg
# The K/E threshold for the 30 kpw-largest countries:
kpw_pwt10 <- sort(-pwt10$kpw)
# kpw_pwt10 # 200000
pwt10$kpw_larg[pwt10$kpw < 200000] <- 0
pwt10$kpw_larg[pwt10$kpw >= 200000] <- 1
summ_pwt10_kpw_larg <- summary(pwt10$kpw_larg)
# summ_pwt10_kpw_larg
# The K/L threshold for the 10 tilde_k-largest countries:
tilde_k_pwt10 <- sort(-pwt10$tilde_k)
# tilde_k_pwt10 # 152
pwt10$tilde_k_larg[pwt10$tilde_k < 152] <- 0
pwt10$tilde_k_larg[pwt10$tilde_k >= 152] <- 1
summ_pwt10_tilde_k_larg <- summary(pwt10$tilde_k_larg)
# summ_pwt10_tilde_k_larg
# The K/bar_L threshold for the 10 bar_k-largest countries:
bar_k_pwt10 <- sort(-pwt10$bar_k)
# bar_k_pwt10 # 49
pwt10$bar_k_larg[pwt10$bar_k < 49] <- 0
pwt10$bar_k_larg[pwt10$bar_k >= 49] <- 1
summ_pwt10_bar_k_larg <- summary(pwt10$bar_k_larg)
# summ_pwt10_bar_k_larg
# The K/H threshold for the 20 check_k-largest countries:
check_k_pwt10 <- sort(-pwt10$check_k)
# check_k_pwt10 # 34500
pwt10$check_k_larg[pwt10$check_k < 34500] <- 0
pwt10$check_k_larg[pwt10$check_k >= 34500] <- 1
summ_pwt10_check_k_larg <- summary(pwt10$check_k_larg)
# summ_pwt10_check_k_larg
# summary(pwt10)
# Merges pwt10 and pwt2: 
pwt10a <- pwt10[c(-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31, -32, -33, -34, -35, -36, -37, -38, -39, -40)]
pwt <- merge(pwt10a, pwt2, by="countrycode")
summ_pwt <- summary(pwt)
names(pwt)
# str(pwt)
# summ_pwt
