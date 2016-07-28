# BUGGY

# Julio Huato
# 1/13/2016
# 2016 Eastern Economics Association paper

# PWT8.1 all available indicators 1950-2011
# http://www.rug.nl/research/ggdc/data/penn-world-table
# This reads the data worksheet and checks its structure.
library(RCurl)
pwt <- getURL("https://sites.google.com/site/juliohuatopwt/pwt81/pwt81.csv")
pwtx <-  read.csv(text = pwt)
str(pwtx)

# This computes 'wage':
pwtx$wagee <- (pwtx$labsh * pwtx$rgdpe)
pwtx$wageo <- (pwtx$labsh * pwtx$rgdpo)

# This removes unnecessary columns from dataframe.
# The adjustment below removes two outliers (El Salvador & Bermuda 2000).
pwtx1 <- subset(pwtx, select = c(countrycode, year, rgdpe, rgdpo, rkna, labsh, wagee, wageo, rconna))
pwtx1a <- pwtx1[!((pwtx1$countrycode=="SLV" & pwtx1$year==2000) | (pwtx1$countrycode=="BMU" & pwtx1$year==2000) ),]
# No.  Replace these values with NA so the vectors keep their length.

# pwtx0 is adjusted for outliers.
pwtx0 <- subset(pwtx1a, select = c(year, rgdpe, rgdpo, rkna, labsh, wagee, wageo, rconna))
cor(pwtx0, use = "complete")
cor(log(pwtx0), use = "complete")

# Plot of variables by year.
yrgdpe<-tapply(pwtx0$rgdpe, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrgdpe, type="l", main="Global real GDP (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrgdpo<-tapply(pwtx0$rgdpo, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrgdpo, type="l", main="Global real GDP(o) (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrkna<-tapply(pwtx0$rkna, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrkna, type="l", main="Global real capital stock (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

ylabsh<-tapply(pwtx0$labsh, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],ylabsh, type="l", main="International (average) wage share", xlab="Year", ylab="")

ywagee<-tapply(pwtx0$wagee, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],ywagee, type="l", main="Global labor income (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

ywageo<-tapply(pwtx0$wageo, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],ywageo, type="l", main="Global labor income(o) (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrconna<-tapply(pwtx0$rconna, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrconna, type="l", main="Global consumption expenditure (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")


# INTERNATIONAL ANALYSIS

# This computes national 'profit' and runs a simple test to ensure national
# 'wage' and national 'profit' add up to national 'rdgpe'/'rdgpo' as they should:
pwtx0$profite <- (pwtx0$rgdpe - pwtx0$wagee)
pwtx0$profito <- (pwtx0$rgdpo - pwtx0$wageo)
pwtx0$teste <- (pwtx0$profite + pwtx0$wagee)
pwtx0$testo <- (pwtx0$profito + pwtx0$wageo)
cor(pwtx0$teste,pwtx0$rgdpe, use = "complete")
cor(pwtx0$testo,pwtx0$rgdpo, use = "complete")

# This computes national 'investment' and runs a simple test to ensure national
# 'consumption' and national 'investment' add up to national 'rdgpe'/'rdgpo' as they should:
pwtx0$rinve <- (pwtx0$rgdpe - pwtx0$rconna)
pwtx0$rinvo <- (pwtx0$rgdpo - pwtx0$rconna)
pwtx0$testec <- (pwtx0$rinve + pwtx0$rconna)
pwtx0$testoc <- (pwtx0$rinvo + pwtx0$rconna)
cor(pwtx0$testec,pwtx0$rgdpe, use = "complete")
cor(pwtx0$testoc,pwtx0$rgdpo, use = "complete")

# This computes national 'Profit share,' 'Wage share', 'Capital productivity'
# 'Rate of exploitation,' and 'Profit rate':
pwtx0$psharee <- (pwtx0$profite/pwtx0$rgdpe)
pwtx0$pshareo <- (pwtx0$profito/pwtx0$rgdpo)
pwtx0$wsharee <- (pwtx0$wagee/pwtx0$rgdpe)
pwtx0$wshareo <- (pwtx0$wageo/pwtx0$rgdpo)
pwtx0$kprode <- (pwtx0$rgdpe/pwtx0$rkna)
pwtx0$kprodo <- (pwtx0$rgdpo/pwtx0$rkna)
pwtx0$exploite <- (pwtx0$profite/pwtx0$wagee)
pwtx0$exploito <- (pwtx0$profito/pwtx0$wageo)
pwtx0$pratee <- (pwtx0$profite/pwtx0$rkna)
pwtx0$prateo <- (pwtx0$profito/pwtx0$rkna)

# This plots the variables created above:

yprofite<-tapply(pwtx0$profite, pwtx0$year, sum, na.rm=TRUE)
str(yprofite)
plot(pwtx0$year[1:62],yprofite, type="l", main="Global profits (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yprofito<-tapply(pwtx0$profito, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yprofito, type="l", main="Global profits(o) (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrinve<-tapply(pwtx0$rinve, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrinve, type="l", main="Global real investment expenditures (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrinvo<-tapply(pwtx0$rinvo, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrinvo, type="l", main="Global real investment expenditures(o) (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

ypsharee<-tapply(pwtx0$psharee, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ypsharee, type="l", main="International (average) profit share", xlab="Year", ylab="")

ypshareo<-tapply(pwtx0$pshareo, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ypshareo, type="l", main="International (average) profit share(o)", xlab="Year", ylab="")

ywsharee<-tapply(pwtx0$wsharee, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ywsharee, type="l", main="International (average) wage share", xlab="Year", ylab="")

ywshareo<-tapply(pwtx0$wshareo, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ywshareo, type="l", main="International (average) wage share(o)", xlab="Year", ylab="")

ykprode<-tapply(pwtx0$kprode, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ykprode, type="l", main="International (average) capital productivity", xlab="Year", ylab="Millions 2005 $")
# Bermuda 2000 and El Salvador 2000 messed this up.  Removing these two cases:
ykprodea<-tapply(pwtx0$kprodea, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ykprodea, type="l", main="International (average) capital productivity[a]", xlab="Year", ylab="Millions 2005 $")

ykprodo<-tapply(pwtx0$kprodo, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ykprodo, type="l", main="International (average) capital productivity(o)", xlab="Year", ylab="Millions 2005 $")
# Bermuda 2000 and El Salvador 2000 messed this up.  Removing these two cases:
ykprodoa<-tapply(pwtx0$kprodoa, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ykprodoa, type="l", main="International (average) capital productivity(o)[a]", xlab="Year", ylab="Millions 2005 $")

yexploite<-tapply(pwtx0$exploite, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],yexploite, type="l", main="International (average) rate of exploitation", xlab="Year", ylab="")

yexploito<-tapply(pwtx0$exploito, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],yexploito, type="l", main="International (average) rate of exploitation(o)", xlab="Year", ylab="")

ypratee<-tapply(pwtx0$pratee, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ypratee, type="l", main="International (average) profit rate", xlab="Year", ylab="")

yprateo<-tapply(pwtx0$prateo, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],yprateo, type="l", main="International (average) profit rate(o)", xlab="Year", ylab="")


# This subsets the relevant data for 1961, 1971, 1981, 1991, 2001, 2011:

rgdpe61<-log(na.omit(subset(pwtx0$rgdpe, pwtx0$year==1961)))
rgdpe71<-log(na.omit(subset(pwtx0$rgdpe, pwtx0$year==1971)))
rgdpe81<-log(na.omit(subset(pwtx0$rgdpe, pwtx0$year==1981)))
rgdpe91<-log(na.omit(subset(pwtx0$rgdpe, pwtx0$year==1991)))
rgdpe01<-log(na.omit(subset(pwtx0$rgdpe, pwtx0$year==2001)))
rgdpe11<-log(na.omit(subset(pwtx0$rgdpe, pwtx0$year==2011)))

rgdpo61<-log(na.omit(subset(pwtx0$rgdpo, pwtx0$year==1961)))
rgdpo71<-log(na.omit(subset(pwtx0$rgdpo, pwtx0$year==1971)))
rgdpo81<-log(na.omit(subset(pwtx0$rgdpo, pwtx0$year==1981)))
rgdpo91<-log(na.omit(subset(pwtx0$rgdpo, pwtx0$year==1991)))
rgdpo01<-log(na.omit(subset(pwtx0$rgdpo, pwtx0$year==2001)))
rgdpo11<-log(na.omit(subset(pwtx0$rgdpo, pwtx0$year==2011)))

wagee61<-log(na.omit(subset(pwtx0$wagee, pwtx0$year==1961)))
wagee71<-log(na.omit(subset(pwtx0$wagee, pwtx0$year==1971)))
wagee81<-log(na.omit(subset(pwtx0$wagee, pwtx0$year==1981)))
wagee91<-log(na.omit(subset(pwtx0$wagee, pwtx0$year==1991)))
wagee01<-log(na.omit(subset(pwtx0$wagee, pwtx0$year==2001)))
wagee11<-log(na.omit(subset(pwtx0$wagee, pwtx0$year==2011)))

wageo61<-log(na.omit(subset(pwtx0$wageo, pwtx0$year==1961)))
wageo71<-log(na.omit(subset(pwtx0$wageo, pwtx0$year==1971)))
wageo81<-log(na.omit(subset(pwtx0$wageo, pwtx0$year==1981)))
wageo91<-log(na.omit(subset(pwtx0$wageo, pwtx0$year==1991)))
wageo01<-log(na.omit(subset(pwtx0$wageo, pwtx0$year==2001)))
wageo11<-log(na.omit(subset(pwtx0$wageo, pwtx0$year==2011)))

profite61<-log(na.omit(subset(pwtx0$profite, pwtx0$year==1961)))
profite71<-log(na.omit(subset(pwtx0$profite, pwtx0$year==1971)))
profite81<-log(na.omit(subset(pwtx0$profite, pwtx0$year==1981)))
profite91<-log(na.omit(subset(pwtx0$profite, pwtx0$year==1991)))
profite01<-log(na.omit(subset(pwtx0$profite, pwtx0$year==2001)))
profite11<-log(na.omit(subset(pwtx0$profite, pwtx0$year==2011)))

profito61<-log(na.omit(subset(pwtx0$profito, pwtx0$year==1961)))
profito71<-log(na.omit(subset(pwtx0$profito, pwtx0$year==1971)))
profito81<-log(na.omit(subset(pwtx0$profito, pwtx0$year==1981)))
profito91<-log(na.omit(subset(pwtx0$profito, pwtx0$year==1991)))
profito01<-log(na.omit(subset(pwtx0$profito, pwtx0$year==2001)))
profito11<-log(na.omit(subset(pwtx0$profito, pwtx0$year==2011)))

psharee61<-na.omit(subset(pwtx0$psharee, pwtx0$year==1961))
psharee71<-na.omit(subset(pwtx0$psharee, pwtx0$year==1971))
psharee81<-na.omit(subset(pwtx0$psharee, pwtx0$year==1981))
psharee91<-na.omit(subset(pwtx0$psharee, pwtx0$year==1991))
psharee01<-na.omit(subset(pwtx0$psharee, pwtx0$year==2001))
psharee11<-na.omit(subset(pwtx0$psharee, pwtx0$year==2011))

pshareo61<-na.omit(subset(pwtx0$pshareo, pwtx0$year==1961))
pshareo71<-na.omit(subset(pwtx0$pshareo, pwtx0$year==1971))
pshareo81<-na.omit(subset(pwtx0$pshareo, pwtx0$year==1981))
pshareo91<-na.omit(subset(pwtx0$pshareo, pwtx0$year==1991))
pshareo01<-na.omit(subset(pwtx0$pshareo, pwtx0$year==2001))
pshareo11<-na.omit(subset(pwtx0$pshareo, pwtx0$year==2011))

kprode61<-log(na.omit(subset(pwtx0$kprode, pwtx0$year==1961)))
kprode71<-log(na.omit(subset(pwtx0$kprode, pwtx0$year==1971)))
kprode81<-log(na.omit(subset(pwtx0$kprode, pwtx0$year==1981)))
kprode91<-log(na.omit(subset(pwtx0$kprode, pwtx0$year==1991)))
kprode01<-log(na.omit(subset(pwtx0$kprode, pwtx0$year==2001)))
kprode11<-log(na.omit(subset(pwtx0$kprode, pwtx0$year==2011)))

kprodo61<-log(na.omit(subset(pwtx0$kprodo, pwtx0$year==1961)))
kprodo71<-log(na.omit(subset(pwtx0$kprodo, pwtx0$year==1971)))
kprodo81<-log(na.omit(subset(pwtx0$kprodo, pwtx0$year==1981)))
kprodo91<-log(na.omit(subset(pwtx0$kprodo, pwtx0$year==1991)))
kprodo01<-log(na.omit(subset(pwtx0$kprodo, pwtx0$year==2001)))
kprodo11<-log(na.omit(subset(pwtx0$kprodo, pwtx0$year==2011)))

exploite61<-log(na.omit(subset(pwtx0$exploite, pwtx0$year==1961)))
exploite71<-log(na.omit(subset(pwtx0$exploite, pwtx0$year==1971)))
exploite81<-log(na.omit(subset(pwtx0$exploite, pwtx0$year==1981)))
exploite91<-log(na.omit(subset(pwtx0$exploite, pwtx0$year==1991)))
exploite01<-log(na.omit(subset(pwtx0$exploite, pwtx0$year==2001)))
exploite11<-log(na.omit(subset(pwtx0$exploite, pwtx0$year==2011)))

exploito61<-log(na.omit(subset(pwtx0$exploito, pwtx0$year==1961)))
exploito71<-log(na.omit(subset(pwtx0$exploito, pwtx0$year==1971)))
exploito81<-log(na.omit(subset(pwtx0$exploito, pwtx0$year==1981)))
exploito91<-log(na.omit(subset(pwtx0$exploito, pwtx0$year==1991)))
exploito01<-log(na.omit(subset(pwtx0$exploito, pwtx0$year==2001)))
exploito11<-log(na.omit(subset(pwtx0$exploito, pwtx0$year==2011)))

pratee61<-log(na.omit(subset(pwtx0$pratee, pwtx0$year==1961)))
pratee71<-log(na.omit(subset(pwtx0$pratee, pwtx0$year==1971)))
pratee81<-log(na.omit(subset(pwtx0$pratee, pwtx0$year==1981)))
pratee91<-log(na.omit(subset(pwtx0$pratee, pwtx0$year==1991)))
pratee01<-log(na.omit(subset(pwtx0$pratee, pwtx0$year==2001)))
pratee11<-log(na.omit(subset(pwtx0$pratee, pwtx0$year==2011)))

prateo61<-log(na.omit(subset(pwtx0$prateo, pwtx0$year==1961)))
prateo71<-log(na.omit(subset(pwtx0$prateo, pwtx0$year==1971)))
prateo81<-log(na.omit(subset(pwtx0$prateo, pwtx0$year==1981)))
prateo91<-log(na.omit(subset(pwtx0$prateo, pwtx0$year==1991)))
prateo01<-log(na.omit(subset(pwtx0$prateo, pwtx0$year==2001)))
prateo11<-log(na.omit(subset(pwtx0$prateo, pwtx0$year==2011)))

# Multi plot function:
# The input of the function MUST be a numeric list
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

# Density plots:
plot.multi.dens(list(rgdpe71,rgdpe91,rgdpe11))
title(main = "K-density of log of Real GDP (Value Added)")
library(Hmisc)
le <- largest.empty(rgdpe61,rgdpe11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

plot.multi.dens(list(rgdpo71,rgdpo91,rgdpo11))
title(main = "K-density of log of Real GDP(o) (Value Added)")
le <- largest.empty(rgdpo71,rgdpo11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

plot.multi.dens(list(wagee71,wagee91,wagee11))
title(main = "K-density of log of Wages (Variable Capital)")
le <- largest.empty(wagee71,wagee11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

plot.multi.dens(list(wageo61,wageo71,wageo81,wageo91,wageo01,wageo11))
title(main = "K-density of log of Wages(o) (Variable Capital)")
le <- largest.empty(wageo61,wageo11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(profite61,profite71,profite81,profite91,profite01,profite11))
title(main = "K-density of log of Profits (Surplus Value)")
le <- largest.empty(profite61,profite11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(profito61,profito71,profito81,profito91,profito01,profito11))
title(main = "K-density of log of Wages(o) (Surplus Value)")
le <- largest.empty(profito61,profito11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(psharee61,psharee71,psharee81,psharee91,psharee01,psharee11))
title(main = "K-density of Profit Share")
le <- largest.empty(psharee61,psharee11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(pshareo61,pshareo71,pshareo81,pshareo91,pshareo01,pshareo11))
title(main = "K-density of Profit Share(o)")
le <- largest.empty(pshareo61,pshareo11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(kprode61,kprode71,kprode81,kprode91,kprode01,kprode11))
title(main = "K-density of the Log of Capital Productivity")
le <- largest.empty(kprode61,kprode11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(kprodo61,kprodo71,kprodo81,kprodo91,kprodo01,kprodo11))
title(main = "K-density of the Log of Capital Productivity(o)")
le <- largest.empty(kprodo61,kprodo11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(exploite61,exploite71,exploite81,exploite91,exploite01,exploite11))
title(main = "K-density of the Log of the Rate of Exploitation")
le <- largest.empty(exploite61,exploite11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(exploito61,exploito71,exploito81,exploito91,exploito01,exploito11))
title(main = "K-density of the Log of the Rate of Exploitation(o)")
le <- largest.empty(exploito61,exploito11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(pratee61,pratee71,pratee81,pratee91,pratee01,pratee11))
title(main = "K-density of the Log of the Profit Rate")
le <- largest.empty(pratee61,pratee11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

plot.multi.dens(list(prateo61,prateo71,prateo81,prateo91,prateo01,prateo11))
title(main = "K-density of the Log of the Profit Rate(o)")
le <- largest.empty(prateo61,prateo11,.1,.1)
legend(le,legend=c("1961","1971","1981","1991","2001","2011"), lwd=(1:6), lty =(1:6))

# GLOBAL ANALYSIS

# This aggregates the data globally by adding up 'rdgpe,' 'rkna,' and 'wage'.  
# The spec "na.rm=TRUE" is to ignore missing values.
sum(is.na(pwtx0))
pwtx0a <-aggregate(pwtx0, by=list(pwtx0$year), na.rm=TRUE, "sum")

# This computes global 'profit' and runs a simple test to ensure global
# 'wage' and global 'profit' add up to global 'rdgpe'/'rdgpo' as they should:
pwtx0a$profite <- (pwtx0a$rgdpe - pwtx0a$wagee)
pwtx0a$profito <- (pwtx0a$rgdpo - pwtx0a$wageo)
pwtx0a$teste <- (pwtx0a$profite + pwtx0a$wagee)
pwtx0a$testo <- (pwtx0a$profito + pwtx0a$wageo)
cor(pwtx0a$teste,pwtx0a$rgdpe)
cor(pwtx0a$testo,pwtx0a$rgdpo)

# This computes global 'Profit share,' global 'Capital productivity'
# global 'Rate of exploitation,' and global 'Profit rate':
pwtx0a$psharee <- (pwtx0a$profite/pwtx0a$rgdpe)
pwtx0a$pshareo <- (pwtx0a$profito/pwtx0a$rgdpo)
pwtx0a$kprode <- (pwtx0a$rgdpe/pwtx0a$rkna)
pwtx0a$kprodo <- (pwtx0a$rgdpo/pwtx0a$rkna)
pwtx0a$exploite <- (pwtx0a$profite/pwtx0a$wagee)
pwtx0a$exploito <- (pwtx0a$profito/pwtx0a$wageo)
pwtx0a$pratee <- (pwtx0a$profite/pwtx0a$rkna)
pwtx0a$prateo <- (pwtx0a$profito/pwtx0a$rkna)

# This computes global wshare (labsh, but after aggregation):
pwtx0a$wsharee <- (pwtx0a$wagee/pwtx0a$rgdpe)
pwtx0a$wshareo <- (pwtx0a$wageo/pwtx0a$rgdpo)

# This section generates the plots:
library(ggplot2)
ggplot(pwtx0a, aes(Group.1, rgdpe)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global real GDP (mil. 2005 USD) [PWT 8.1]", x = "Year", y = "Real GDP^e"))

ggplot(pwtx0a, aes(Group.1, rgdpo)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global real GDP(o) (mil. 2005 USD) [PWT 8.1]", x = "Year", y = "Real GDP^o"))

ggplot(pwtx0a, aes(Group.1, rkna)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global real capital stock (mil. 2005 USD) [PWT 8.1]", x = "Year", y = "Real capital stock"))

ggplot(pwtx0a, aes(Group.1, wsharee)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global wage share^e [PWT 8.1]", x = "Year", y = "Wage share^e"))

ggplot(pwtx0a, aes(Group.1, wshareo)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global wage share^o [PWT 8.1]", x = "Year", y = "Wage share^o"))

ggplot(pwtx0a, aes(Group.1, pratee)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global profit rate^e [PWT 8.1]", x = "Year", y = "Profit rate^e"))

ggplot(pwtx0a, aes(Group.1, prateo)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global profit rate^o [PWT 8.1]", x = "Year", y = "Profit rate^o"))


ggplot(pwtx0a, aes(Group.1, psharee)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global profit share^e [PWT 8.1]", x = "Year", y = "Profit share^e"))

ggplot(pwtx0a, aes(Group.1, pshareo)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global profit share^o [PWT 8.1]", x = "Year", y = "Profit share^o"))

ggplot(pwtx0a, aes(Group.1, kprode)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global 'capital productivity^e' [PWT 8.1]", x = "Year", y = "'Capital productivity^e"))

ggplot(pwtx0a, aes(Group.1, kprodo)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global 'capital productivity^o' [PWT 8.1]", x = "Year", y = "'Capital productivity^o"))

ggplot(pwtx0a, aes(Group.1, exploite)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global rate of exploitation^e [PWT 8.1]", x = "Year", y = "Rate of exploitation^e"))

ggplot(pwtx0a, aes(Group.1, exploito)) +
  theme_bw() +
  geom_path(colour="black") +
  geom_smooth(method=lm, colour="red", linetype="dashed") +
  geom_smooth(colour="red", linetype="dashed") +
  labs(list(title = "Global rate of exploitation^o [PWT 8.1]", x = "Year", y = "Rate of exploitation^o"))

# This creates a txt file I can open on Excel.
# The file can be found on the R work file.
write.table(pwtx0a, "pwtx0aa.txt", sep="\t")

# End