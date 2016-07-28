# The Deep History of Postwar Global Capitalism 
# by Julio Huato (3/18/2016)
# Loads the PWT 8.1 data directly or by RStudio/Import Dataset/From Web URL/Import
# https://sites.google.com/site/juliohuatopwt/pwt81/pwt81.csv
# pwt81 <- read.csv("https://sites.google.com/site/juliohuatopwt/pwt81/pwt81.csv")
pwt0 <- subset(pwt81,currency_unit!="Zimbabwe Dollar") # Removes 3 redundant observations.
vars <- c("countrycode", "year", "rgdpe", "rgdpo", "pop", "emp", "avh", "hc", "rkna", "rtfpna","rwtfpna", "labsh", "csh_c", "i_outlier") 
pwt1 <- pwt0[vars] # Keeps the needed variables
summ_pwt1 <- summary(pwt1)
pwt1$y <- (pwt1$rgdpe + pwt1$rgdpo)/2 # Defines y as the average of rgdpe & rgdpo
vars2 <- c("countrycode", "year", "y", "rgdpo", "pop", "emp", "avh", "hc", "rkna", "rtfpna","rwtfpna", "labsh", "csh_c", "i_outlier")
pwt2 <- pwt1[vars2] # Keeps the needed variables
summ_pwt2 <- summary(pwt2)
# summ_pwt2
# Computes L [l], H [h], Y/N [ypc], Y/E [ypw], Y/L [tilde_y], 
# Y/H-adj L [bar_y], Y/H [check_y], K/N [kpc], K/E [kpw], K/L [tilde_k], 
# K/H-adj L [bar_k], K/H [check_k]:
pwt2$l <- pwt2$avh*pwt2$emp
pwt2$bar_l <- pwt2$hc*pwt2$l
pwt2$h <- pwt2$hc*pwt2$pop
pwt2$ypc <- pwt2$y/pwt2$pop
pwt2$ypw <- pwt2$y/pwt2$emp
pwt2$tilde_y <- pwt2$y/pwt2$l
pwt2$bar_y <- pwt2$y/pwt2$bar_l
pwt2$check_y <- pwt2$y/pwt2$h
pwt2$kpc <- pwt2$rkna/pwt2$pop
pwt2$kpw <- pwt2$rkna/pwt2$emp
pwt2$tilde_k <- pwt2$rkna/pwt2$l
pwt2$bar_k <- pwt2$rkna/pwt2$bar_l
pwt2$check_k <- pwt2$rkna/pwt2$h
# Computes (Y-W)/Y [pi], (Y-W)/W [e], E/N [epsilon], L/N [tilde_epsilon],
# 1-c [iota], (1-c)/c [beta], Y/K [rho], (Y-W)/K [r], (Y-C)/K [kappa]:
pwt2$pi <- 1-pwt2$labsh
pwt2$e <- pwt2$pi/pwt2$labsh
pwt2$epsilon <- pwt2$emp/pwt2$pop
pwt2$tilde_epsilon <- pwt2$l/pwt2$pop
pwt2$iota <- 1-pwt2$csh_c
pwt2$beta <- pwt2$iota/pwt2$csh_c
pwt2$rho <- pwt2$y/pwt2$rkna
pwt2$r <- pwt2$pi*pwt2$rho
pwt2$kappa <- pwt2$iota*pwt2$rho
# Computes omega*Y [wages], (1-omega)*Y [profits], c*Y [cons], 
# (1-c)*Y [noncons]:
pwt2$wages <- pwt2$labsh*pwt2$y
pwt2$profits <- (1-pwt2$labsh)*pwt2$y
pwt2$cons <- pwt2$csh_c*pwt2$rgdpo
pwt2$noncons <- (1-pwt2$csh_c)*pwt2$rgdpo
# summary(pwt2)
# Takes 2010 as base year for country classification:
pwt2010 <- subset(pwt2,year==2010)
summ_pwt2010 <- summary(pwt2010)
# Loads the country classification directly or by RStudio/Import Dataset/From Web URL/Import:
# https://sites.google.com/site/juliohuatopwt/pwt81/dh_country_class.csv
# dh_country <- read.csv("https://sites.google.com/site/juliohuatopwt/pwt81/dh_country_class.csv")
# Merges pop_2010 and dh_country:
dh_country <- dh_country_class
pwt10 <- merge(pwt2010, dh_country, by="countrycode")
summ_pwt10 <- summary(pwt10)
# summ_pwt10
# The population threshold for the 30 n-largest countries:
n_pwt10 <- sort(-pwt10$pop)
# n_pwt10 # n<40.5
pwt10$n_larg[pwt10$pop < 40.5] <- 0
pwt10$n_larg[pwt10$pop >= 40.5] <- 1
summ_pwt10_n_larg <- summary(pwt10$n_larg)
# summ_pwt10_n_larg
# The Y threshold for the 30 y-largest countries:
y_pwt10 <- sort(-pwt10$y)
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
# MISSING VALUES PER VARIABLE/YEAR
valid_count<-function(x) {
  length(x[!is.na(x)])
}
valid_pwt_vars <- data.frame(matrix(0, ncol = 57, nrow = 62))
names(valid_pwt_vars) <- names(pwt)
for(i in 1:57){
  valid_pwt_vars[[i]] <- tapply(pwt[[i]], pwt$year,  valid_count)
  }
valid_pwt_vars
# MISSING VALUES PER VARIABLE/COUNTRY
valid_pwt_countries <- data.frame(matrix(0, ncol = 57, nrow = 167))
names(valid_pwt_countries) <- names(pwt)
for(i in 1:57){
  valid_pwt_countries[[i]] <- tapply(pwt[[i]], pwt$country_i,  valid_count)
}
valid_pwt_countries
# GLOBAL ANALYSIS
# Subsets the 
pwtg1<-subset(pwt, select = c(year, y, rgdpo, rkna, pop, emp, rtfpna, rwtfpna, l, bar_l, h, wages, cons))
str(pwtg1)
# Aggregates the data globally:
sum(is.na(pwtg1))
pwtg <-aggregate(pwtg1, by=list(pwtg1$year), na.rm=TRUE, "sum")
str(pwtg)
summary(pwg)
# Computes global lambda [avh] and h-coefficient [hc]:
pwtg$avh <- pwtg$l/pwtg$emp
pwtg$hc <- pwtg$h/pwtg$pop
# Computes (Y-W)/Y [pi], (Y-W)/W [e], E/N [epsilon], L/N [tilde_epsilon],
# 1-c [iota], (1-c)/c [beta], Y/K [rho], (Y-W)/K [r], (Y-C)/K [kappa]:
pwtg$labsh <- pwtg$wages/pwtg$y
pwtg$csh_c <- pwtg$cons/pwtg$rgdpo
pwtg$ypc <- pwtg$y/pwtg$pop
pwtg$ypw <- pwtg$y/pwtg$emp
pwtg$tilde_y <- pwtg$y/pwtg$l
pwtg$bar_y <- pwtg$y/pwtg$bar_l
pwtg$check_y <- pwtg$y/pwtg$h
pwtg$kpc <- pwtg$rkna/pwtg$pop
pwtg$kpw <- pwtg$rkna/pwtg$emp
pwtg$tilde_k <- pwtg$rkna/pwtg$l
pwtg$bar_k <- pwtg$rkna/pwtg$bar_l
pwtg$check_k <- pwtg$rkna/pwtg$h
pwtg$pi <- 1-pwtg$labsh
pwtg$e <- pwtg$pi/pwtg$labsh
pwtg$epsilon <- pwtg$emp/pwtg$pop
pwtg$tilde_epsilon <- pwtg$l/pwtg$pop
pwtg$iota <- 1-pwtg$csh_c
pwtg$beta <- pwtg$iota/pwtg$csh_c
pwtg$rho <- pwtg$y/pwtg$rkna
pwtg$r <- pwtg$pi*pwtg$rho
pwtg$kappa <- pwtg$iota*pwtg$rho
# Computes omega*Y [wages], (1-omega)*Y [profits], c*Y [cons], 
# (1-c)*Y [noncons]:
pwtg$wages <- pwtg$labsh*pwtg$y
pwtg$profits <- (1-pwtg$labsh)*pwtg$y
pwtg$cons <- pwtg$csh_c*pwtg$rgdpo
pwtg$noncons <- (1-pwtg$csh_c)*pwtg$rgdpo
pwtg2 <-pwtg[-2]
pwtg2$year <- pwtg2$Group.1
pwtg <- pwtg2[-1]
summ_pwtg <- summary(pwtg)
summ_pwtg
names(pwtg) <-c("Gross domestic product", "Gross domestic product[o]", "Capital stock", "Population", "Engaged persons", "Total factor productivity", "Welfare-relevant TFP", "Annual labor", "Annual labor in unskilled units", "Labor power in unskilled units", "Wages", "Consumption", "Average hours of annual labor per engaged person", "Skilled-labor coefficient", "Wage share", "Consumption share", "Value added per capita", "Value added per engaged person", "Value added per hour of labor","Value added per unskilled labor unit","Value added per unskilled person","Capital per person","Capital per engaged person", "Capital per hour of labor", "Capital per unit of unskilled labor", "Capital per unskilled person", "Profit share", "Rate of exploitation", "Engaged persons per person", "Hours of engaged labor per person", "Non-consumption share", "Ratio of non-consumption to consumption", "Capital productivity", "Profit rate", "Non-consumption per unit of capital","Profits", "Non-consumption", "Year")
names_pwtg<-names(pwtg)
units_pwtg<-c("Millions 2005 PPP $", "Millions 2005 PPP $", "Millions 2005 PPP $", "Millions", "Millions", "", "", "Hours", "", "", "Millions 2005 PPP $", "Millions 2005 PPP $", "Hours of labor/engaged person", " ", " ", " ", "2005 PPP $/person", "2005 PPP $/person", "2005 PPP $/hour of labor","2005 PPP $/unskilled labor unit","2005 PPP $/person","2005 PPP $/person","2005 PPP $/person", "2005 PPP $/hour of labor", "2005 PPP $/unskilled labor unit", "2005 PPP $/person", " ", " ", "Engaged persons/person", "Hours of engaged labor/person", " ", " ", " ", " ", " ","Millions 2005 PPP $", "Millions 2005 PPP $", "Year")
plotg<-function(x,y,z){
    plot(c(1950:2011),x, type="l", col="black", lwd=2, main=y, sub="Source: PWT 8.1", xlim=c(1965,2010), xlab="Year", ylab=z)
}
for (i in 1:37) {
  plotg(pwtg[[i]],names_pwtg[i],units_pwtg[i])    
}

plot(c(1950:2011),pwtg$`Gross domestic product`, type="l", col="black", lwd=2, main="Global gross domestic product: Distribution", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="Millions 2005 PPP $")
lines(pwtg$Year,pwtg$Wages, col="black", lwd=2)
polygon(c(pwtg$Year, rev(pwtg$Year)), c(pwtg$`Gross domestic product`, rev(pwtg$Wages)), col = "grey60", border = NA)
polygon(c(pwtg$Year, rev(pwtg$Year)), c(pwtg$Wages, rev(rep(0,62))), col = "grey40", border = NA)
legend("topleft", c("Profits","Wages"), fill = c("gray60", "gray40"))

plot(c(1950:2011),pwtg$`Gross domestic product`, type="l", col="black", lwd=2, main="Global gross domestic product: Distribution", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="Millions 2005 PPP $")
lines(pwtg$Year,pwtg$Consumption, col="black", lwd=2)
polygon(c(pwtg$Year, rev(pwtg$Year)), c(pwtg$`Gross domestic product`, rev(pwtg$Consumption)), col = "grey60", border = NA)
polygon(c(pwtg$Year, rev(pwtg$Year)), c(pwtg$Consumption, rev(rep(0,62))), col = "grey40", border = NA)
legend("topleft", c("Non-consumption","Consumption"), fill = c("gray40", "gray60"))

plot(c(1950:2011),pwtg$Population, type="l", col="black", lwd=2, main="Global population & engaged persons", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="Millions")
lines(pwtg$Year,pwtg$`Engaged persons`, col="black", lwd=2)
polygon(c(pwtg$Year, rev(pwtg$Year)), c(pwtg$Population, rev(pwtg$`Engaged persons`)), col = "grey60", border = NA)
polygon(c(pwtg$Year, rev(pwtg$Year)), c(pwtg$`Engaged persons`, rev(rep(0,62))), col = "grey40", border = NA)
legend("topleft", c("Dependents","Engaged"), fill = c("gray40", "gray60"))

plot(c(1950:2011),pwtg$`Total factor productivity`, type="l", col="black", lwd=1, main="Global TFP & welfare-relevant TFP", sub="Source: PWT 8.1", xlim=c(1965,2011), xlab="Year", ylab="", ylim=c(50,120))
lines(c(1950:2011),pwtg$`Welfare-relevant TFP`, type="l", col="black", lwd=3, xlab="", ylab="")
legend("topleft", c("TFP","W-R TFP"), lty=c(1,1), lwd=c(1,3),col=c("black","black"))

plot(c(1950:2011),pwtg$`Annual labor in unskilled units`, type="l", col="black", lwd=3, main="Global annual labor", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(700000,4000000), xlab="Year", ylab="Hours")
lines(c(1950:2011),pwtg$`Annual labor`, type="l", col="black", lwd=1, xlab="", ylab="")
legend("topleft", c("Equivalent unskilled hours", "Total hours"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

plot(c(1950:2011),pwtg$`Annual labor`, type="l", col="black", lwd=3, main="Global annual labor", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="Hours")

plot(c(1950:2011),pwtg$`Labor power in unskilled units`, type="l", col="black", lwd=3, main="Labor power in unskilled equivalent units", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="Hours")

plot(c(1950:2011),pwtg$`Average hours of annual labor per engaged person`, type="l", col="black", lwd=3, main="Annual labor per engaged person", sub="Source: PWT 8.1", xlim=c(1955,2011), xlab="Year", ylab="Hours (average)")

plot(c(1950:2011),pwtg$`Skilled-labor coefficient`, type="l", col="black", lwd=3, main="Skilled-labor coefficient", sub="Source: PWT 8.1", xlim=c(1955,2011), xlab="Year", ylab="")

plot(c(1950:2011),pwtg$`Consumption share`, type="l", col="black", lwd=3, main="Global consumption & wage shares", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(0.5,0.7), xlab="Year", ylab="")
lines(c(1950:2011),pwtg$`Wage share`, type="l", col="black", lwd=1, xlab="", ylab="")
legend("topright", c("C/Y", "W/Y"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

plot(c(1950:2011),pwtg$`Value added per capita`, type="l", col="black", lwd=1, main="Global value added per person & engaged person", sub="Source: PWT 8.1", xlim=c(1965,2011), ylim=c(2000,26000), xlab="Year", ylab="")
lines(c(1950:2011),pwtg$`Value added per engaged person`, type="l", col="black", lwd=3, xlab="", ylab="")
legend("bottomright", c("2005 PPP $/person", "2005 PPP $/engaged person"), lty=c(1,1), lwd=c(1,3),col=c("black","black"))

plot(c(1950:2011),pwtg$`Value added per hour of labor`, type="l", col="black", lwd=3, main="Value added per hour of labor", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(0,60), xlab="Year", ylab="2005 PPP $/hour")

plot(c(1950:2011),pwtg$`Value added per unskilled labor unit`, type="l", col="black", lwd=3, main="Value added per unskilled labor unit", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="2005 PPP $/unit")

plot(c(1950:2011),pwtg$`Value added per unskilled person`, type="l", col="black", lwd=3, main="Value added per unskilled person", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="2005 PPP $/hour")

plot(c(1950:2011),pwtg$`Capital per person`, type="l", col="black", lwd=1, main="Capital per person", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(0,80000), xlab="Year", ylab="2005 PPP $/hour")
lines(c(1950:2011),pwtg$`Capital per engaged person`, type="l", col="black", lwd=3, xlab="", ylab="")
legend("topleft", c("2005 PPP $/hour of labor", "2005 PPP $/hour of equivalent unskilled labor"), lty=c(1,1), lwd=c(1,3),col=c("black","black"))

plot(c(1950:2011),pwtg$`Capital per hour of labor`, type="l", col="black", lwd=3, main="Capital per hour of labor", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(20,180), xlab="Year", ylab="2005 PPP $/hour")

plot(c(1950:2011),pwtg$`Capital per unit of unskilled labor`, type="l", col="black", lwd=3, main="Capital per hour of labor", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="2005 PPP $/hour")

plot(c(1950:2011),pwtg$`Capital per unskilled person`, type="l", col="black", lwd=3, main="Capital per unskilled person", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab="2005 PPP $/hour")

plot(c(1950:2011),pwtg$`Profit share`, type="l", col="black", lwd=3, main="Global profit share", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Rate of exploitation`, type="l", col="black", lwd=3, main="Global rate of exploitation", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Engaged persons per person`, type="l", col="black", lwd=3, main="Engaged persons per person", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Hours of engaged labor per person`, type="l", col="black", lwd=3, main="Hours of engaged labor per person", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Non-consumption share`, type="l", col="black", lwd=3, main="Non-consumption share", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Profit share`, type="l", col="black", lwd=3, main="Global profit & non-consumption share", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(.3,.5), xlab="Year", ylab=" ")
lines(c(1950:2011),pwtg$`Non-consumption share`, type="l", col="black", lwd=1,ylab=" ")
legend("topleft", c("(Y-W)/Y", "(Y-C)/Y"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

plot(c(1950:2011),pwtg$`Rate of exploitation`, type="l", col="black", lwd=3, main="Global rate of exploitation & non-consumption to consumption ratio", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(0.2,1), xlab="Year", ylab=" ")
lines(c(1950:2011),pwtg$`Ratio of non-consumption to consumption`, type="l", col="black", lwd=1,ylab=" ")
legend("topleft", c("(Y-W)/W", "(Y-C)/C"), lty=c(1,1), lwd=c(3,1),col=c("black","black"))

plot(c(1950:2011),pwtg$`Capital productivity`, type="l", col="black", lwd=3, main="Global capital productivity", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Profit rate`, type="l", col="black", lwd=3, main="Global profit rate", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Non-consumption per unit of capital`, type="l", col="black", lwd=3, main="Global non-consumption per unit of capital", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$, type="l", col="black", lwd=3, main="Global capital productivity", sub="Source: PWT 8.1", xlim=c(1950,2011), xlab="Year", ylab=" ")

plot(c(1950:2011),pwtg$`Capital productivity`, type="l", col="black", lwd=3, main="Global Y/K, (Y-W)/K, (Y-C)/K", sub="Source: PWT 8.1", xlim=c(1950,2011), ylim=c(0.1,.4), xlab="Year", ylab=" ")
lines(c(1950:2011),pwtg$`Profit rate`, type="l", col="black", lwd=2, xlim=c(1950,2011), xlab=" ", ylab=" ")
lines(c(1950:2011),pwtg$`Non-consumption per unit of capital`, type="l", col="black", lwd=1, xlim=c(1950,2011), xlab=" ", ylab=" ")
legend("topright", c("Y/K", "(Y-W)/K", "(Y-C)/K"), lty=c(1,1,1), lwd=c(3,2,1),col=c("black","black", "black"))