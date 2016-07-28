# Load the PWT 8.1 data using Import Dataset/From Web URL/Import
# https://sites.google.com/site/juliohuatopwt/pwt81/pwt81.csv
pwt0 <- subset(pwt81,currency_unit!="Zimbabwe Dollar")
vars <- c("countrycode", "year", "rgdpe", "rgdpo", "pop", "emp", "rkna", "labsh", "csh_c")
pwt1 <- pwt0[vars]
summary(pwt1)
pwt1$rgdp <- (pwt1$rgdpe + pwt1$rgdpo)/2
vars2 <- c("countrycode", "year", "rgdp", "pop", "emp", "rkna", "labsh", "csh_c")
pwt2 <- pwt1[vars2]
summary(pwt2)
length(pwt2$country)

# Load the country classification using Import Dataset/From Web URL/Import
# https://sites.google.com/site/juliohuatopwt/pwt81/dh_country_class.csv
pwt <- merge(pwt2, dh_country_class, by="countrycode")
summary(pwt)

# Take 2010 as base year for country classification:
pwt2010 <- subset(pwt,year==2010)
summary(pwt2010)
# The pop threshold for the 30 n-largest countries:
n_pwt2010 <- sort(-pwt2010$pop)
n_pwt2010
pwt$n_largest[pwt$pop < 40.5] <- 0
pwt$n_largest[pwt$pop >= 40.5] <- 1
summary(pwt$n_largest)
# The rgdp threshold for the 30 y-largest countries:
y_pwt2010 <- sort(-pwt2010$rgdp)
y_pwt2010
pwt$y_largest[pwt$rgdp < 3.4e+05 ] <- 0
pwt$y_largest[pwt$rgdp >= 3.4e+05 ] <- 1
summary(pwt$y_largest)


# x: dataframe, y=countrycode, z:economic variable
f_plot <- function(x,y) {
  for(j in length()){
  x1 <- subset(x, y==y[j], select=c(x))
  ts_x <- ts(x1, start=1950, end=2011, frequency=1)
  plot(ts_x, main="",xlab="Year",ylab="GDP")}
}
f_plot(mydata$rgdp,mydata$countrycode)