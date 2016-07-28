# Julio Huato
# 2016 Eastern Economics Association paper
# The Deep History of Postwar Global Capitalism
# R code (shorter version)
# Start date: 1/6/2016
today <- Sys.Date()
format(today, format="%B %d %Y")

# PWT8.1 all available indicators 1950-2011
# http://www.rug.nl/research/ggdc/data/penn-world-table
# This reads the data file and checks its structure.
pwtx <- read.csv("https://sites.google.com/site/juliohuatopwt/pwt81/pwt81.csv")

# This computes country-year rgdp (Y).
pwtx$rgdps<-pwtx$rgdpo + pwtx$rgdpe
pwtx$rgdp<-(1/2)*pwtx$rgdps

# This computes country-year rgdp (Y), wage (W, wagee), consumption (C, ce), 
# gross capital accumulation (Y-C, se), profit.
pwtx$wage <- (pwtx$labsh * pwtx$rgdp)
pwtx$c <- (pwtx$csh_c * pwtx$rgdp)
pwtx$csh_s <- (1 - pwtx$csh_c)
pwtx$s <- (pwtx$csh_s * pwtx$rgdp)
pwtx$profit <- (pwtx$rgdp - pwtx$wage)
pwtx$test <- (pwtx$profit + pwtx$wage)
pwtx$s1 <- (pwtx$rgdp - pwtx$c)
pwtx$test1 <- (pwtx$s + pwtx$c)
pwtx$test2 <- (pwtx$s1 + pwtx$c)
cor(pwtx$test,pwtx$rgdp, use = "complete")
cor(pwtx$test1,pwtx$rgdp, use = "complete")
cor(pwtx$test2,pwtx$rgdp, use = "complete")

# This drops some vars.
pwtx0 <- subset(pwtx, select = c(countrycode, year, rgdp, rkna, labsh, wage, profit, c, s, csh_c, csh_s))
# Data frame without NAs.
newpwtx0 <- na.omit(pwtx0)

# GLOBAL PRIMARY VARIABLES
# Global real GDP (Y)
yrgdp<-tapply(pwtx0$rgdp, pwtx0$year, sum, na.rm=TRUE)
yexp<-lm(log(yrgdp) ~ c(1:62))
# summary(yexp)
plot(c(1950:2011),yrgdp, type="l", col="black", lwd=2, main="Global real GDP", sub="Source: PWT 8.1", xlab="Year", ylab="Millions 2005 $")
lines(c(1950:2011), exp(fitted(yexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global real GDP","Y trend = Exp(15.554 + 0.04304*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

# The capital stock path looks too smooth. 
# Data sparseness aside, this must be an artifact of the 
# perpetual-inventory method of estimating the capital stock.
yrkna<-tapply(pwtx0$rkna, pwtx0$year, sum, na.rm=TRUE)
kexp<-lm(log(yrkna)~ c(1:62))
# summary(kexp)
plot(pwtx0$year[1:62],yrkna, type="l", col="black", lwd=2,  main="Global real capital stock", sub="Source: PWT8.1", ylim=c(0,2.75e+08), xlab="Year", ylab="Millions 2005 $")
lines(c(1950:2011), exp(fitted(kexp)), type="l", lwd=2, col="black", lty=3)
legend("topleft", c("Global real capital stock","K trend = Exp(0.1651 + 0.0459*Year)"), lwd=c(2,2), lty=c(1,3), col=c("black","black"))

# Global real GDP, Consumption and Wage
ywage<-tapply(pwtx0$wage, pwtx0$year, sum, na.rm=TRUE)
yrc<-tapply(pwtx0$c, pwtx0$year, sum, na.rm=TRUE)
cexp<-lm(log(yrc) ~ c(1:62))
wexp<-lm(log(ywage) ~ c(1:62))
# summary(cexp)
# summary(wexp)
plot(pwtx0$year[1:62],yrgdp, type="l", col="black", lwd=3,  main="Global real GDP, Consumption & Wages", sub="Source: PWT 8.1", xlab="Year", ylab="Millions 2005 $", ylim=c(0,8.2e+07))
lines(c(1950:2011), exp(fitted(yexp)), type="l", lwd=3, col="black", lty=3)
lines(pwtx0$year[1:62],yrc, type="l", col="black", lwd=2)
lines(c(1950:2011), exp(fitted(cexp)), type="l", lwd=2, col="black", lty=3)
lines(pwtx0$year[1:62],ywage, type="l", col="black", lwd=1)
lines(c(1950:2011), exp(fitted(wexp)), type="l", lwd=1, col="black", lty=3)
legend("topleft", c("Y","Y trend: annual growth rate 4.304%", "C","C trend: annual growth rate 4.052%", "W","W trend: annual growth rate 3.989%"), lty=c(1,3,1,3,1,3), lwd=c(3,3,2,2,1,1),col=c("black","black","black","black","black","black"))

# INTERNATIONAL SECONDARY VARIABLES
# This computes country-year profit, gross capital accum, and wage shares,
# capital productivity, rate of exploitation, accum-consumption ratio,
# profit rate, and accum rate.
# For consumption and gross accum shares, I use no-outliers data.
# Note the weak negative correlation between (Y-C)/C and (Y-W)/W.
pwtx0$pshare <- (pwtx0$profit/pwtx0$rgdp)
pwtx0$sshare <- (pwtx0$s/pwtx0$rgdp)
pwtx0$wshare <- (pwtx0$wage/pwtx0$rgdp)
pwtx0$kprod <- (pwtx0$rgdp/pwtx0$rkna)
pwtx0$exploit <- (pwtx0$profit/pwtx0$wage)
pwtx0$exploits <- (pwtx0$s/pwtx0$c)
pwtx0$prate <- (pwtx0$profit/pwtx0$rkna)
pwtx0$srate <- (pwtx0$s/pwtx0$rkna)

# Correlations:
# (Y-W) is highly correlated with (Y-C)
# and C is highly correlated with W.
cor(pwtx0$profit,pwtx0$s, use = "complete")
cor(pwtx0$wage,pwtx0$c, use = "complete")

# (Y-W) is highly correlated with adj (Y-C), where adj=After dropping out of
# (0,1)-bounds values. Also, after removing NAs.  Log-log is also strong.
# Similar with W and C, W and adj C, and after removing NAs.
cor(pwtx0$profit,pwtx0$s, use = "complete")
cor(log(pwtx0$profit),log(pwtx0$s), use = "complete")
cor(pwtx0$wage,pwtx0$c, use = "complete")
cor(log(pwtx0$wage),log(pwtx0$c), use = "complete")

# This plots profit share and gross capital accumulation share:
ypshare<-tapply(pwtx0$pshare, pwtx0$year, mean, na.rm=TRUE)
ysshare<-tapply(pwtx0$sshare, pwtx0$year, mean, na.rm=TRUE)
plot(c(1950:2011),ypshare, type="l", col="black", lwd=3,  main="Int'l (mean) profit and gross capital accumulation shares", sub="Source: PWT 8.1", xlab="Year",ylim=c(0.1,0.6))
lines(c(1950:2011),ysshare, type="l", col="black", lwd=1)
legend("topleft", c("(Y-W)/Y","(Y-C)/Y"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

# This plots capital productivity. Outliers cause the spike on 2000.
# International means are more volatile, as small countries with volatile
# incomes and capital stocks have same weight as larger, more stable countries.
ykprod<-tapply(pwtx0$kprod, pwtx0$year, mean, na.rm=TRUE)
plot(c(1950:2011), ykprod, type="l", col="black", lwd=2,  main="Int'l (mean) capital productivity (Y/K)", sub="Source: PWT 8.1", xlab="Year", ylab=" ")
abline(h=mean(ykprod),lty=2, lwd=1)
legend("topleft", "1950-2011 Mean of (mean) Y/K",lty=2, lwd=1,col="black")

# International e and beta.
yexploit<-tapply(pwtx0$exploit, pwtx0$year, mean, na.rm=TRUE)
yexploits<-tapply(pwtx0$exploits, pwtx0$year, mean, na.rm=TRUE)
plot(c(1950:2011),yexploit, type="l", col="black", lwd=3,  main="Int'l exploit rate & gross capital accumulation-consumption ratio", sub="Source: PWT 8.1", xlab="Year", ylab="", ylim=c(-0.8,1.4))
lines(c(1950:2011),yexploits, type="l", col="black", lwd=1)
legend("topleft", c("e=(Y-W)/W","beta=(Y-C)/C"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

# International r and kappa.
yprate <-tapply(pwtx0$prate, pwtx0$year, mean, na.rm=TRUE)
ysrate <-tapply(pwtx0$srate, pwtx0$year, mean, na.rm=TRUE)
plot(c(1950:2011), ykprod, type="l", col="black", lwd=3,  main="Int'l (mean) rho, r, and kappa", sub="Source: PWT 8.1", xlab="Year", ylab=" ", ylim=c(0.05,0.85))
lines(c(1950:2011),yprate, type="l", col="black", lwd=2,  main="Int'l (mean) r and kappa", sub="Source: PWT 8.1", xlab="Year",ylim=c(0.05,0.55))
lines(c(1950:2011),ysrate, type="l", col="black", lwd=1)
legend("topleft", c("rho=Y/K", "r=(Y-W)/K","kappa=(Y-C)/K"), lty=c(1,1,1), lwd=c(3,2,1),col=c("black","black", "black"))

# DISTRIBUTION OF INTERNATIONAL PRIMARY AND SECONDARY VARIABLES
# This subsets the relevant data for 1961, 1971, 1981, 1991, 2001, 2011:
rgdp71<-log(na.omit(subset(pwtx0$rgdp, pwtx0$year==1971)))
rgdp91<-log(na.omit(subset(pwtx0$rgdp, pwtx0$year==1991)))
rgdp11<-log(na.omit(subset(pwtx0$rgdp, pwtx0$year==2011)))

wage71<-log(na.omit(subset(pwtx0$wage, pwtx0$year==1971)))
wage91<-log(na.omit(subset(pwtx0$wage, pwtx0$year==1991)))
wage11<-log(na.omit(subset(pwtx0$wage, pwtx0$year==2011)))

c71<-log(na.omit(subset(pwtx0$c, pwtx0$year==1971)))
c91<-log(na.omit(subset(pwtx0$c, pwtx0$year==1991)))
c11<-log(na.omit(subset(pwtx0$c, pwtx0$year==2011)))

profit71<-log(na.omit(subset(pwtx0$profit, pwtx0$year==1971)))
profit91<-log(na.omit(subset(pwtx0$profit, pwtx0$year==1991)))
profit11<-log(na.omit(subset(pwtx0$profit, pwtx0$year==2011)))

s71<-log(na.omit(subset(pwtx0$s, pwtx0$year==1971)))
s91<-log(na.omit(subset(pwtx0$s, pwtx0$year==1991)))
s11<-log(na.omit(subset(pwtx0$s, pwtx0$year==2011)))

pshare71<-na.omit(subset(pwtx0$pshare, pwtx0$year==1971))
pshare91<-na.omit(subset(pwtx0$pshare, pwtx0$year==1991))
pshare11<-na.omit(subset(pwtx0$pshare, pwtx0$year==2011))

sshare71<-na.omit(subset(pwtx0$sshare, pwtx0$year==1971))
sshare91<-na.omit(subset(pwtx0$sshare, pwtx0$year==1991))
sshare11<-na.omit(subset(pwtx0$sshare, pwtx0$year==2011))

kprod71<-log(na.omit(subset(pwtx0$kprod, pwtx0$year==1971)))
kprod91<-log(na.omit(subset(pwtx0$kprod, pwtx0$year==1991)))
kprod11<-log(na.omit(subset(pwtx0$kprod, pwtx0$year==2011)))

exploit71<-log(na.omit(subset(pwtx0$exploit, pwtx0$year==1971)))
exploit91<-log(na.omit(subset(pwtx0$exploit, pwtx0$year==1991)))
exploit11<-log(na.omit(subset(pwtx0$exploit, pwtx0$year==2011)))

exploits71<-log(na.omit(subset(pwtx0$exploits, pwtx0$year==1971)))
exploits91<-log(na.omit(subset(pwtx0$exploits, pwtx0$year==1991)))
exploits11<-log(na.omit(subset(pwtx0$exploits, pwtx0$year==2011)))

prate71<-log(na.omit(subset(pwtx0$prate, pwtx0$year==1971)))
prate91<-log(na.omit(subset(pwtx0$prate, pwtx0$year==1991)))
prate11<-log(na.omit(subset(pwtx0$prate, pwtx0$year==2011)))

srate71<-log(na.omit(subset(pwtx0$srate, pwtx0$year==1971)))
srate91<-log(na.omit(subset(pwtx0$srate, pwtx0$year==1991)))
srate11<-log(na.omit(subset(pwtx0$srate, pwtx0$year==2011)))

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

plot.multi.dens(list(rgdp71,rgdp91,rgdp11))
# segments(x0=mean(rgdp71),y0=0,x1=mean(rgdp71),y1=0.165,col="black", lwd=1, lty=2)
# segments(x0=mean(rgdp91),y0=0,x1=mean(rgdp91),y1=0.172,col="black", lwd=2, lty=2)
# segments(x0=mean(rgdp11),y0=0,x1=mean(rgdp11),y1=0.169,col="black", lwd=3, lty=2)
title(main = "Kernel density of log of national real GDP", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(wage71,wage91,wage11))
# Bimodal distributions supports Lenin's "labor aristocracy" hypothesis.
title(main = "Kernel density of log of national real wage", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(c71,c91,c11))
# No bimodal pattern here.
title(main = "Kernel density of log of national real consumption", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(profit71,profit91,profit11))
# Bimodal pattern emerging in 2011.
title(main = "Kernel density of log of national profit", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(na.omit(s71),na.omit(s91),na.omit(s11)))
# Bimodal pattern here in 2011, starting before.
# Note that I'm omitting the NAs generated by logs of negative values.
title(main = "Kernel density of national log of gross capital accumulation", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(pshare71,pshare91,pshare11))
title(main = "Kernel density of log of national profit share", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

# Outlier with near zero betas account for long left tail.
plot.multi.dens(list(sshare71,sshare91,sshare11))
title(main = "Kernel density of log of national beta", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(kprod71,kprod91,kprod11))
title(main = "Kernel density of log of national capital productivity", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(exploit71,exploit91,exploit11))
title(main = "Kernel density of log of national rate of exploitation", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(na.omit(exploits71),na.omit(exploits91),na.omit(exploits11)))
title(main = "Kernel density of log of national beta", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(na.omit(prate71),na.omit(prate91),na.omit(prate11)))
title(main = "Kernel density of log of national profit rate", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

plot.multi.dens(list(na.omit(srate71),na.omit(srate91),na.omit(srate11)))
title(main = "Kernel density of log of national kappa", sub="Source: PWT 8.1")
legend("topleft",legend=c("1971","1991","2011"), lwd=(1:3),lty =c(1,1,1))

# GLOBAL SECONDARY VARIABLES
# Subsetting to keep only the primary variables in the data frame.
# str(pwtx)
pwtx1<-subset(pwtx, select = c(year, rgdpe, rgdpo, rkna, labsh, csh_c))
str(pwtx1)

# Creation of needed primary variables.
pwtx1$rgdps<-pwtx1$rgdpo + pwtx1$rgdpe
pwtx1$rgdp<-(1/2)*pwtx1$rgdps
pwtx1$wage <- (pwtx1$labsh * pwtx1$rgdp)
pwtx1$c <- (pwtx1$csh_c * pwtx1$rgdp)
pwtx1$pshare <- (1 - pwtx1$labsh)
pwtx1$csh_s <- (1 - pwtx1$csh_c)
pwtx1$s <- (pwtx1$csh_s * pwtx1$rgdp)
pwtx1$profit <- (pwtx1$rgdp - pwtx1$wage)
pwtx1$profit0 <- (pwtx1$rgdp * pwtx1$pshare)
pwtx1$s0 <- (pwtx1$rgdp * pwtx1$csh_s)
pwtx1$test <- (pwtx1$profit + pwtx1$wage)
pwtx1$test01 <- (pwtx1$profit0 + pwtx1$wage)
pwtx1$test0 <- (pwtx1$s0 + pwtx1$c)
pwtx1$s1 <- (pwtx1$rgdp - pwtx1$c)
pwtx1$test1 <- (pwtx1$s + pwtx1$c)
pwtx1$test2 <- (pwtx1$s1 + pwtx1$c)
cor(pwtx1$test,pwtx1$rgdp, use = "complete")
cor(pwtx1$test01,pwtx1$rgdp, use = "complete")
cor(pwtx1$test0,pwtx1$rgdp, use = "complete")
cor(pwtx1$test1,pwtx1$rgdp, use = "complete")
cor(pwtx1$test2,pwtx1$rgdp, use = "complete")

# Subsetting to remove needless vars.
pwtx10 <- subset(pwtx1, select = c(year, rgdp, rkna, wage, c))
str(pwtx10)
summary(pwtx10)
# Data frame without NAs.
newpwtx10 <- na.omit(pwtx10)

# Aggregation of global primary variables.
sum(is.na(pwtx10))
pwtx1g <-aggregate(pwtx10, by=list(pwtx10$year), na.rm=TRUE, "sum")
str(pwtx1g)
summary(pwtx1g)

# Tests data consistency and computes secondary global variables.
pwtx1g$profit <- (pwtx1g$rgdp - pwtx1g$wage)
pwtx1g$s <- (pwtx1g$rgdp - pwtx1g$c)
pwtx1g$test <- (pwtx1g$profit + pwtx1g$wage)
pwtx1g$tests <- (pwtx1g$s + pwtx1g$c)
cor(pwtx1g$test,pwtx1g$rgdp)
cor(pwtx1g$tests,pwtx1g$rgdp)
pwtx1g$pshare <- (pwtx1g$profit/pwtx1g$rgdp)
pwtx1g$sshare <- (pwtx1g$s/pwtx1g$rgdp)
pwtx1g$kprod <- (pwtx1g$rgdp/pwtx1g$rkna)
pwtx1g$exploit <- (pwtx1g$profit/pwtx1g$wage)
pwtx1g$exploits <- (pwtx1g$s/pwtx1g$c)
pwtx1g$prate <- (pwtx1g$profit/pwtx1g$rkna)
pwtx1g$srate <- (pwtx1g$s/pwtx1g$rkna)
pwtx1g$wshare <- (pwtx1g$wage/pwtx1g$rgdp)
pwtx1g$cshare <- (pwtx1g$c/pwtx1g$rgdp)

# This plots global profit share and gross capital accumulation share:
plot(c(1950:2011),pwtx1g$pshare, type="l", col="black", lwd=3,  main="Global profit and gross capital accumulation shares", sub="Source: PWT 8.1", xlab="Year",ylim=c(0.3,0.5))
lines(c(1950:2011),pwtx1g$sshare, type="l", col="black", lwd=1)
legend("topleft", c("pi=(Y-W)/Y","iota=(Y-C)/Y"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

# This plots global e and beta:
plot(c(1950:2011),pwtx1g$exploit, type="l", col="black", lwd=3,  main="Global exploit rate & gross capital accumulation-consumption ratio", sub="Source: PWT 8.1", xlab="Year", ylab="", ylim=c(0.4,1))
lines(c(1950:2011),pwtx1g$exploits, type="l", col="black", lwd=1)
legend("topleft", c("e=(Y-W)/W","beta=(Y-C)/C"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

# This plots global rho.
# Note that global capital productivity is the maximum profit rate.
plot(c(1950:2011),pwtx1g$kprod, type="l", col="black", lwd=3,  main="Global capital productivity", sub="Source: PWT 8.1", xlab="Year", ylab="")
abline(h=mean(pwtx1g$kprod),lty=2, lwd=1)
legend("topright", "1950-2011 Mean of global Y/K",lty=2, lwd=1,col="black")

# This plots global rho, r and kappa:
plot(c(1950:2011),pwtx1g$kprod, type="l", col="black", lwd=3,  main="Global rho, r, and kappa", sub="Source: PWT 8.1", xlab="Year", ylab="", ylim=c(0.1,.4))
lines(c(1950:2011),pwtx1g$prate, type="l", col="black", lwd=2)
lines(c(1950:2011),pwtx1g$srate, type="l", col="black", lwd=1)
legend("topright", c("rho=Y/K", "r=(Y-W)/K","kappa=(Y-C)/K"), lty=c(1,1,1), lwd=c(3,2,1),col=c("black","black", "black"))

# This creates a txt (csv) file I can open on Excel.
# It can be found in the Parent Directory of this cloud.sagemath Project.
# str(pwtx1g)
pwtx1g0<-pwtx1g[-c(2,9,10)]
# str(pwtx1g0)
write.table(pwtx1g0, "pwtx1g0.txt", sep=",")
# End

