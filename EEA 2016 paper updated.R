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

# Or this way:
pwtx <- read.csv("https://sites.google.com/site/juliohuatopwt/pwt81/pwt81.csv")
str(pwtx)

# This computes 'wage' (as W and as C) and profits ('se'=Y-C):
pwtx$wagee <- ((pwtx$labsh/10) * pwtx$rgdpe)
pwtx$ce <- (pwtx$csh_c * pwtx$rgdpe)
pwtx$csh_s <- (pwtx$csh_i + pwtx$csh_g)
pwtx$se <- (pwtx$csh_s * pwtx$rgdpe)

# This removes unnecessary columns from dataframe.
# The adjustment below removes two outliers (El Salvador & Bermuda 2000).
pwtx0 <- subset(pwtx, select = c(countrycode, year, rgdpe, rkna, labsh, wagee, ce, se, csh_c, csh_i, csh_g))
str(pwtx0)

# This runs correlations.
cor(pwtx0[c(-1,-2)], use = "complete")

# Plot of variables by year.
yrgdpe<-tapply(pwtx0$rgdpe, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrgdpe, type="l", col="blue", main="Global real GDP (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrkna<-tapply(pwtx0$rkna, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrkna, type="l", col="blue", main="Global real capital stock (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

ylabsh<-tapply(pwtx0$labsh, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ylabsh, type="l", col="blue", main="International (average) wage (W) share", xlab="Year", ylab="")

ycsh_c<-tapply(pwtx0$csh_c, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ycsh_c, type="l", col="blue", main="International (average) wage (C) share", xlab="Year", ylab="")

ycsh_s<-tapply(pwtx0$csh_s, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ycsh_s, type="l", col="blue", main="International (average) profit (I+G) share", xlab="Year", ylab="")

# I AM HERE!!!!!!!!!!!!!!!!!

ywagee<-tapply(pwtx0$wagee, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],ywagee, type="l", col="blue", main="Global labor income (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrce<-tapply(pwtx0$ce, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrce, type="l", col="blue", main="Global consumption expenditure (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yrse<-tapply(pwtx0$se, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yrce, type="l", col="blue", main="Global investment and government expenditure (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

# INTERNATIONAL ANALYSIS

# This computes national 'profit' and runs a simple test to ensure national
# 'wage' and national 'profit' add up to national 'rdgpe'/'rdgpo' as they should:
pwtx0$profite <- (pwtx0$rgdpe - pwtx0$wagee)
pwtx0$teste <- (pwtx0$profite + pwtx0$wagee)
cor(pwtx0$teste,pwtx0$rgdpe, use = "complete")

# I don't expect this correlation to be perfect.
pwtx0$se <- (pwtx0$rgdpe - pwtx0$ce)
pwtx0$teste1 <- (pwtx0$se + pwtx0$ce)
cor(pwtx0$teste1,pwtx0$rgdpe, use = "complete")

# This computes national 'Profit share,' 'Wage share', 'Capital productivity'
# 'Rate of exploitation,' and 'Profit rate':
pwtx0$psharee <- (pwtx0$profite/pwtx0$rgdpe)
pwtx0$seshare <- (pwtx0$se/pwtx0$rgdpe)
pwtx0$wsharee <- (pwtx0$wagee/pwtx0$rgdpe)
pwtx0$ceshare <- (pwtx0$ce/pwtx0$rgdpe)
pwtx0$kprode <- (pwtx0$rgdpe/pwtx0$rkna)
pwtx0$exploite <- (pwtx0$profite/pwtx0$wagee)
pwtx0$pratee <- (pwtx0$profite/pwtx0$rkna)
pwtx0$seratee <- (pwtx0$se/pwtx0$rkna)

# Correlations:
cor(pwtx0$profite,pwtx0$se, use = "complete")
cor(log(pwtx0$profite),log(pwtx0$se), use = "complete")
cor(pwtx0$wagee,pwtx0$ce, use = "complete")
cor(log(pwtx0$wagee),log(pwtx0$ce), use = "complete")
grprofite <- diff(log(pwtx0$profite))
grwagee <- diff(log(pwtx0$wagee))
grse <- diff(log(pwtx0$se))
grce <- diff(log(pwtx0$ce))
cor(grprofite,grse, use = "complete")
cor(grwagee,grce, use = "complete")

# This plots the variables created above:

yprofite<-tapply(pwtx0$profite, pwtx0$year, sum, na.rm=TRUE)
str(yprofite)
plot(pwtx0$year[1:62],yprofite, type="l", main="Global profits (Y-W) (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

yse<-tapply(pwtx0$se, pwtx0$year, sum, na.rm=TRUE)
plot(pwtx0$year[1:62],yse, type="l", main="Global profits(I+G) (Millions 2005 $)", xlab="Year", ylab="Millions 2005 $")

ypsharee<-tapply(pwtx0$psharee, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ypsharee, type="l", main="International (average) profit (Y-W) share", xlab="Year", ylab="")

yseshare<-tapply(pwtx0$seshare, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],yseshare, type="l", main="International (average) profit (I+G) share(o)", xlab="Year", ylab="")

ywsharee<-tapply(pwtx0$wsharee, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ywsharee, type="l", main="International (average) wage (W) share", xlab="Year", ylab="")

yceshareo<-tapply(pwtx0$ceshare, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],yceshare, type="l", main="International (average) wage (C) share(o)", xlab="Year", ylab="")

ykprode<-tapply(pwtx0$kprode, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ykprode, type="l", main="International (average) capital productivity", xlab="Year", ylab="Millions 2005 $")

yexploite<-tapply(pwtx0$exploite, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],yexploite, type="l", main="International (average) rate of exploitation", xlab="Year", ylab="")

ypratee<-tapply(pwtx0$pratee, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],ypratee, type="l", main="International (average) profit (Y-W) rate", xlab="Year", ylab="")

yseratee<-tapply(pwtx0$seratee, pwtx0$year, mean, na.rm=TRUE)
plot(pwtx0$year[1:62],yserate, type="l", main="International (average) profit (I+G) rate", xlab="Year", ylab="")

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

plot.multi.dens(list(wageo71,wageo91,wageo11))
title(main = "K-density of log of Wages(o) (Variable Capital)")
le <- largest.empty(wageo61,wageo11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

plot.multi.dens(list(profite71,profite91,profite11))
title(main = "K-density of log of Profits (Surplus Value)")
le <- largest.empty(profite61,profite11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

plot.multi.dens(list(profito71,profito91,profito11))
title(main = "K-density of log of Wages(o) (Surplus Value)")
le <- largest.empty(profito61,profito11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

plot.multi.dens(list(psharee71,psharee91,psharee11))
title(main = "K-density of Profit Share")
le <- largest.empty(psharee61,psharee11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

plot.multi.dens(list(pshareo71,pshareo91,pshareo11))
title(main = "K-density of Profit Share(o)")
le <- largest.empty(pshareo61,pshareo11,.1,.1)
legend(le,legend=c("1971","1991","2011"), lwd=(1:3), lty =(1:3))

# [I'M HERE!!!!!]

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
