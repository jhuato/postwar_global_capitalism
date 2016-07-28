# Deep History of Post-War Global Capitalism
# Julio Huato
# 3/12/2016
library(Hmisc)

# pwt81 <- read.csv("https://sites.google.com/site/juliohuatopwt/pwt81/pwt81.csv")
# Use Import Dataset facility on RStudio/Environment
View(pwt81)
str(pwt81)
# 10357 obs. of  47 variables
# 167 countries, 1950-2011
summary(pwt81)
# This computes country-year rgdp (Y):
pwt81$rgdps<-pwt81$rgdpo + pwt81$rgdpe
pwt81$rgdp<-(1/2)*pwt81$rgdps
# Subsetting to drop unnecessary variables:
pwt81a<-subset(pwt81, select = c(countrycode, year, rgdp, pop, emp, rkna, labsh, csh_c))
str(pwt81a)
summary(pwt81a)
# This computes country-year wage (W), consumption (C), 
# gross capital accumulation (Y-C), and profit (Y-W).
pwt81$wage <- (pwt81$labsh * pwt81$rgdp)
pwt81$c <- (pwt81$csh_c * pwt81$rgdp)
pwt81$csh_s <- (1 - pwt81$csh_c)
pwt81$s <- (pwt81$csh_s * pwt81$rgdp)
pwt81$profit <- (pwt81$rgdp - pwt81$wage)
pwt81$rgdp_test <- (pwt81$profit + pwt81$wage)
pwt81$s1 <- (pwt81$rgdp - pwt81$c)
pwt81$rgdp_testw <- (pwt81$s + pwt81$c)
pwt81$rgdp_testc <- (pwt81$s1 + pwt81$c)
pwt81$part <- (pwt81$emp/pwt81$pop)
pwt81$prodv <- (pwt81$rgdp/pwt81$emp)
pwt81$stliv <- (pwt81$rgdp/pwt81$pop)
pwt81$pwwage <- (pwt81$wage/pwt81$emp)
pwt81$pwprofit <- (pwt81$rgdp/pwt81$emp)
pwt81$pcwage <- (pwt81$wage/pwt81$pop)
pwt81$pcprofit <- (pwt81$rgdp/pwt81$pop)
str(pwt81a)
summary(pwt81a)


