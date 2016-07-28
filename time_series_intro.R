options(digits=4)
# Julio Huato
# Intro to time series

# Data web site: http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/#Data

data(AirPassengers)
AP <- AirPassengers
AP
class(AP)
start(AP); end(AP); frequency(AP)
plot(AP, ylab="Passengers (1000s)")
str(AP)
agAP <- aggregate(AP)
agAP
str(agAP)
# "Aggregate" a time series adds up all years over the same month.
layout(1:2)
plot(agAP)
boxplot(AP~cycle(AP))

www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Maine.dat"
Maine.month <- read.table(www, header=T)
attach(Maine.month)
class(Maine.month)
Maine.month.ts <- ts(unemploy, start = c(1996,1), freq=12)
Maine.annual.ts <- aggregate(Maine.month.ts)/12 # This aggregates all months in a year into that year.

layout(1:2)
plot(Maine.month.ts, ylab="unemployed (%)")
plot(Maine.annual.ts, ylab="unemployed (%)")

Maine.Feb <- window(Maine.month.ts, start=c(1996,2), freq=T)
Maine.Aug <- window(Maine.month.ts, start=c(1996,8), freq=T)
Feb.ratio <- mean(Maine.Feb)/mean(Maine.month.ts)
Feb.ratio
Aug.ratio <- mean(Maine.Aug)/mean(Maine.month.ts)
Aug.ratio

www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/USunemp.dat"
US.month <- read.table(www, header=T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq=12)
plot(US.month.ts, ylab="unemployed (%)")

www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat"
CBE <- read.table(www,header=T)
str(CBE)
summary(CBE)
CBE[1:4,]
class(CBE)
Elec.ts <- ts(CBE[,3], start=1958, freq=12)
Beer.ts <- ts(CBE[,2], start=1958, freq=12)
Choc.ts <- ts(CBE[,1], start=1958, freq=12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))
str(AP)
str(Elec.ts)
AP.elec <- ts.intersect(AP, Elec.ts)
str(AP.elec)
AP.elec
AP
Elec.ts
start(Elec.ts)
end(Elec.ts)

AP <- AP.elec[,1]; Elec <- AP.elec[,2]
layout(1:2)
plot(AP, main="", ylab="Air Passengers (1000s)")
plot(Elec, main="", ylab="Electricity production (MkWh)")

layout(1:1)
plot(as.vector(AP), as.vector(Elec), xlab="Air Passengers (1000s)", ylab="Electricity production (MkWh)")
abline(reg=lm(Elec~AP))

www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/pounds_nz.dat"
Z <- read.table(www, header=T)
Z
class(Z)
Z.ts <- ts(Z, st=1991, fr=4)
Z.ts
plot(Z.ts, xlab="time/years", ylab="Quarterly exchange rate in $NZ/pound")

Z.92.96 <- window(Z.ts, start=c(1992,1), end=c(1996,1))
Z.92.96
Z.96.98 <- window(Z.ts, start=c(1996,1), end=c(1998,1))
Z.96.98
layout(1:2)
plot(Z.92.96, ylab="Exchange rate in $NZ/pound", xlab="Time (years)")
plot(Z.96.98, ylab="Exchange rate in $NZ/pound", xlab="Time (years)") # Note the different year range of the plots

www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/global.dat"
Global <- scan(www)
Global
Global.ts <- ts(Global, st=c(1856,1), end = c(2005,12), fr=12)
plot(Global) # scattergram
plot(Global.ts) # Line time series plot
Global.annual <- aggregate(Global.ts, FUN=mean)
layout(1:2)
plot(Global.ts)
plot(Global.annual)
New.series <- window(Global.ts, st=c(1970,1))
layout(1:1)
New.time <- time(New.series)
plot(New.series); abline(reg=lm(New.series~New.time))
summary(lm(New.series~New.time))
