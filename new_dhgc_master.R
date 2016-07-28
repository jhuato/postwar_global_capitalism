# DHGC Julio Huato
# PWT
rm() # Removes all objects from workspace
cat("\014") # Clears the consol

# Packages
library(fBasics)
library(nlme)

pwt9 <- read.csv("~/Downloads/pwt9.csv") # Loads data
str(pwt9) # Data frame structure
summary(pwt9) # Summary: i = 1, \ldots, 182 (countries), 
# t = 1950, ..., 2014 (65 years), n = i \times t = 11830 obs.

# PRIMARY VARIABLES: CALCULATION ------------------------------------------
pwt9$N <- pwt9$pop 
pwt9$E <- pwt9$emp
pwt9$L <- pwt9$E*pwt9$avh
pwt9$Lh <- pwt9$L*pwt9$hc
pwt9$w <- pwt9$labsh
pwt9$Yo <- pwt9$rgdpo
pwt9$Y <- (pwt9$rgdpe + pwt9$Yo)/2 # average of expenditure and output side of RGDP
pwt9$W <- (pwt9$w * pwt9$Y) # Wage
pwt9$c <- pwt9$csh_c
pwt9$C <- (pwt9$c * pwt9$Yo)
pwt9$K <- pwt9$rkna
pwt9no <- pwt9
pwt9no$c[pwt9no$c<0] <- NA # Out of range c as NA
pwt9no$c[pwt9no$c>1] <- NA # Out of range c as NA
pwt9$cno <- pwt9no$c
pwt9$Cno <- (pwt9$cno * pwt9$Yo) # Consumption with no outliers
rm(pwt9no)
pwt9a <- pwt9
pwt9a$c[pwt9a$c<0] <- 0 # Adjusts c by forcing c<0 to 0
pwt9a$c[pwt9a$c>1] <- 1 # Adjusts c by forcing c>1 to 1
pwt9$ca <- pwt9a$c # Adjusted c
pwt9$Ca <- (pwt9$ca * pwt9$Yo) # Consumption
rm(pwt9a)

# SECONDARY VARIABLE CALCULATION ------------------------------------------
pwt9$yn <- pwt9$Y/pwt9$N # Per capita productivity
pwt9$yw <- pwt9$Y/pwt9$E # Per worker productivity
pwt9$yl <- pwt9$Y/pwt9$L # Per hour productivity
pwt9$yh <- pwt9$Y/pwt9$Lh # Skilled-adjusted hourly productivity index
pwt9$epr <- pwt9$E/pwt9$N # Employment-population ratio
pwt9$kn <- pwt9$K/pwt9$N # Per capita capital
pwt9$kw <- pwt9$K/pwt9$E # Per worker capital
pwt9$kl <- pwt9$K/pwt9$L # Per hour capital
pwt9$kh <- pwt9$K/pwt9$Lh # Capital per skilled-adjusted hour
pwt9$e <- (pwt9$Y-pwt9$W)/pwt9$W # Rate of exploitation
pwt9$eps <- (pwt9$Yo-pwt9$C)/pwt9$C # Nonconsumption-consumption ratio
pwt9$epsno <- (pwt9$Yo-pwt9$Cno)/pwt9$Cno # Nonconsumption-consumption ratio (NAs)
pwt9$epsa <- (pwt9$Yo-pwt9$Ca)/pwt9$Ca # Nonconsumption-consumption ratio (adj)
pwt9$rho <- pwt9$Y/pwt9$K # Capital productivity
pwt9$r <- (pwt9$Y-pwt9$W)/pwt9$K # Profit rate
pwt9$kap <- (pwt9$Yo-pwt9$C)/pwt9$C # Nonconsumption-capital ratio
pwt9$kapno <- (pwt9$Yo-pwt9$Cno)/pwt9$Cno # Nonconsumption-capital ratio
pwt9$kapa <- (pwt9$Yo-pwt9$Ca)/pwt9$Ca # Nonconsumption-capital ratio

pwt9 <- pwt9[c("year", "N", "E", "L", "Lh", "w", "Yo", "Y", "W", "c", "cno", "ca", "C", "Cno", "Ca", "K", "yn", "yw", "yl", "yh", "epr", "kn", "kw", "kl", "kh", "e", "eps", "epsno", "epsa", "rho", "r", "kap", "kapno", "kapa")]
summary(pwt9)
str(pwt9)
basicStats(pwt9[c("year", "N", "E", "L", "Lh", "w", "Yo", "Y", "W", "c", "cno", "ca", "C", "Cno", "Ca", "K", "yn", "yw", "yl", "yh", "epr", "kn", "kw", "kl", "kh", "e", "eps", "epsno", "epsa", "rho", "r", "kap", "kapno", "kapa")])[c("nobs", "NAs", "Sum", "Mean", "Median", "Minimum", "Maximum","Stdev", "SE Mean", "Skewness", "Kurtosis"),]

des <- as.data.frame(matrix(c(1950:2014),nrow = 65, ncol=1))

yrNstats <- tapply(pwt9$N, pwt9$year, basicStats)
yrNstats <- as.data.frame(yrNstats)
yrNstats <- t(yrNstats)
Ncolnames <- c( "Nnobs", "NNAs", "NMinimum", "NMaximum", "N1. Quartile", "N3. Quartile", "NMean", "NMedian", "NSum", "NSE Mean", "NLCL Mean", "NUCL Mean", "NVariance", "NStdev", "NSkewness", "NKurtosis")
colnames(yrNstats) <-Ncolnames

yrEstats <- tapply(pwt9$E, pwt9$year, basicStats)
yrEstats <- as.data.frame(yrEstats)
yrEstats <- t(yrEstats)
Ecolnames <- c( "Enobs", "ENAs", "EMinimum", "EMaximum", "E1. Quartile", "E3. Quartile", "EMean", "EMedian", "ESum", "ESE Mean", "ELCL Mean", "EUCL Mean", "EVariance", "EStdev", "ESkewness", "EKurtosis")
colnames(yrEstats) <-Ecolnames

yrLstats <- tapply(pwt9$L, pwt9$year, basicStats)
yrLstats <- as.data.frame(yrLstats)
yrLstats <- t(yrLstats)
Lcolnames <- c( "Lnobs", "LNAs", "LMinimum", "LMaximum", "L1. Quartile", "L3. Quartile", "LMean", "LMedian", "LSum", "LSE Mean", "LLCL Mean", "LUCL Mean", "LVariance", "LStdev", "LSkewness", "LKurtosis")
colnames(yrLstats) <-Lcolnames

yrLhstats <- tapply(pwt9$Lh, pwt9$year, basicStats)
yrLhstats <- as.data.frame(yrLhstats)
yrLhstats <- t(yrLhstats)
Lhcolnames <- c( "Lhnobs", "LhNAs", "LhMinimum", "LhMaximum", "Lh1. Quartile", "Lh3. Quartile", "LhMean", "LhMedian", "LhSum", "LhSE Mean", "LhLCL Mean", "LhUCL Mean", "LhVariance", "LhStdev", "LhSkewness", "LhKurtosis")
colnames(yrLhstats) <-Lhcolnames

yrwstats <- tapply(pwt9$w, pwt9$year, basicStats)
yrwstats <- as.data.frame(yrwstats)
yrwstats <- t(yrwstats)
wcolnames <- c( "wnobs", "wNAs", "wMinimum", "wMaximum", "w1. Quartile", "w3. Quartile", "wMean", "wMedian", "wSum", "wSE Mean", "wLCL Mean", "wUCL Mean", "wVariance", "wStdev", "wSkewness", "wKurtosis")
colnames(yrwstats) <-wcolnames

yrYostats <- tapply(pwt9$Yo, pwt9$year, basicStats)
yrYostats <- as.data.frame(yrYostats)
yrYostats <- t(yrYostats)
Yocolnames <- c( "Yonobs", "YoNAs", "YoMinimum", "YoMaximum", "Yo1. Quartile", "Yo3. Quartile", "YoMean", "YoMedian", "YoSum", "YoSE Mean", "YoLCL Mean", "YoUCL Mean", "YoVariance", "YoStdev", "YoSkewness", "YoKurtosis")
colnames(yrYostats) <-Yocolnames

yrYstats <- tapply(pwt9$Y, pwt9$year, basicStats)
yrYstats <- as.data.frame(yrYstats)
yrYstats <- t(yrYstats)
Ycolnames <- c( "Ynobs", "YNAs", "YMinimum", "YMaximum", "Y1. Quartile", "Y3. Quartile", "YMean", "YMedian", "YSum", "YSE Mean", "YLCL Mean", "YUCL Mean", "YVariance", "YStdev", "YSkewness", "YKurtosis")
colnames(yrYstats) <-Ycolnames

yrWstats <- tapply(pwt9$W, pwt9$year, basicStats)
yrWstats <- as.data.frame(yrWstats)
yrWstats <- t(yrWstats)
Wcolnames <- c( "Wnobs", "WNAs", "WMinimum", "WMaximum", "W1. Quartile", "W3. Quartile", "WMean", "WMedian", "WSum", "WSE Mean", "WLCL Mean", "WUCL Mean", "WVariance", "WStdev", "WSkewness", "WKurtosis")
colnames(yrWstats) <-Wcolnames

yrcstats <- tapply(pwt9$c, pwt9$year, basicStats)
yrcstats <- as.data.frame(yrcstats)
yrcstats <- t(yrcstats)
ccolnames <- c( "cnobs", "cNAs", "cMinimum", "cMaximum", "c1. Quartile", "c3. Quartile", "cMean", "cMedian", "cSum", "cSE Mean", "cLCL Mean", "cUCL Mean", "cVariance", "cStdev", "cSkewness", "cKurtosis")
colnames(yrcstats) <-ccolnames

yrcnostats <- tapply(pwt9$cno, pwt9$year, basicStats)
yrcnostats <- as.data.frame(yrcnostats)
yrcnostats <- t(yrcnostats)
cnocolnames <- c( "cnonobs", "cnoNAs", "cnoMinimum", "cnoMaximum", "cno1. Quartile", "cno3. Quartile", "cnoMean", "cnoMedian", "cnoSum", "cnoSE Mean", "cnoLCL Mean", "cnoUCL Mean", "cnoVariance", "cnoStdev", "cnoSkewness", "cnoKurtosis")
colnames(yrcnostats) <-cnocolnames

yrcastats <- tapply(pwt9$ca, pwt9$year, basicStats)
yrcastats <- as.data.frame(yrcastats)
yrcastats <- t(yrcastats)
cacolnames <- c( "canobs", "caNAs", "caMinimum", "caMaximum", "ca1. Quartile", "ca3. Quartile", "caMean", "caMedian", "caSum", "caSE Mean", "caLCL Mean", "caUCL Mean", "caVariance", "caStdev", "caSkewness", "caKurtosis")
colnames(yrcastats) <-cacolnames

yrCstats <- tapply(pwt9$C, pwt9$year, basicStats)
yrCstats <- as.data.frame(yrCstats)
yrCstats <- t(yrCstats)
Ccolnames <- c( "Cnobs", "CNAs", "CMinimum", "CMaximum", "C1. Quartile", "C3. Quartile", "CMean", "CMedian", "CSum", "CSE Mean", "CLCL Mean", "CUCL Mean", "CVariance", "CStdev", "CSkewness", "CKurtosis")
colnames(yrCstats) <-Ccolnames

yrCnostats <- tapply(pwt9$Cno, pwt9$year, basicStats)
yrCnostats <- as.data.frame(yrCnostats)
yrCnostats <- t(yrCnostats)
Cnocolnames <- c( "Cnonobs", "CnoNAs", "CnoMinimum", "CnoMaximum", "Cno1. Quartile", "Cno3. Quartile", "CnoMean", "CnoMedian", "CnoSum", "CnoSE Mean", "CnoLCL Mean", "CnoUCL Mean", "CnoVariance", "CnoStdev", "CnoSkewness", "CnoKurtosis")
colnames(yrCnostats) <-Cnocolnames

yrCastats <- tapply(pwt9$Ca, pwt9$year, basicStats)
yrCastats <- as.data.frame(yrCastats)
yrCastats <- t(yrCastats)
Cacolnames <- c( "Canobs", "CaNAs", "CaMinimum", "CaMaximum", "Ca1. Quartile", "Ca3. Quartile", "CaMean", "CaMedian", "CaSum", "CaSE Mean", "CaLCL Mean", "CaUCL Mean", "CaVariance", "CaStdev", "CaSkewness", "CaKurtosis")
colnames(yrCastats) <-Cacolnames

yrKstats <- tapply(pwt9$K, pwt9$year, basicStats)
yrKstats <- as.data.frame(yrKstats)
yrKstats <- t(yrKstats)
Kcolnames <- c( "Knobs", "KNAs", "KMinimum", "KMaximum", "K1. Quartile", "K3. Quartile", "KMean", "KMedian", "KSum", "KSE Mean", "KLCL Mean", "KUCL Mean", "KVariance", "KStdev", "KSkewness", "KKurtosis")
colnames(yrKstats) <-Kcolnames

yrynstats <- tapply(pwt9$yn, pwt9$year, basicStats)
yrynstats <- as.data.frame(yrynstats)
yrynstats <- t(yrynstats)
yncolnames <- c( "ynnobs", "ynNAs", "ynMinimum", "ynMaximum", "yn1. Quartile", "yn3. Quartile", "ynMean", "ynMedian", "ynSum", "ynSE Mean", "ynLCL Mean", "ynUCL Mean", "ynVariance", "ynStdev", "ynSkewness", "ynKurtosis")
colnames(yrynstats) <-yncolnames

yrywstats <- tapply(pwt9$yw, pwt9$year, basicStats)
yrywstats <- as.data.frame(yrywstats)
yrywstats <- t(yrywstats)
ywcolnames <- c( "ywnobs", "ywNAs", "ywMinimum", "ywMaximum", "yw1. Quartile", "yw3. Quartile", "ywMean", "ywMedian", "ywSum", "ywSE Mean", "ywLCL Mean", "ywUCL Mean", "ywVariance", "ywStdev", "ywSkewness", "ywKurtosis")
colnames(yrywstats) <-ywcolnames

yrylstats <- tapply(pwt9$yl, pwt9$year, basicStats)
yrylstats <- as.data.frame(yrylstats)
yrylstats <- t(yrylstats)
ylcolnames <- c( "ylnobs", "ylNAs", "ylMinimum", "ylMaximum", "yl1. Quartile", "yl3. Quartile", "ylMean", "ylMedian", "ylSum", "ylSE Mean", "ylLCL Mean", "ylUCL Mean", "ylVariance", "ylStdev", "ylSkewness", "ylKurtosis")
colnames(yrylstats) <-ylcolnames

yryhstats <- tapply(pwt9$yh, pwt9$year, basicStats)
yryhstats <- as.data.frame(yryhstats)
yryhstats <- t(yryhstats)
yhcolnames <- c( "yhnobs", "yhNAs", "yhMinimum", "yhMaximum", "yh1. Quartile", "yh3. Quartile", "yhMean", "yhMedian", "yhSum", "yhSE Mean", "yhLCL Mean", "yhUCL Mean", "yhVariance", "yhStdev", "yhSkewness", "yhKurtosis")
colnames(yryhstats) <-yhcolnames

yreprstats <- tapply(pwt9$epr, pwt9$year, basicStats)
yreprstats <- as.data.frame(yreprstats)
yreprstats <- t(yreprstats)
eprcolnames <- c( "eprnobs", "eprNAs", "eprMinimum", "eprMaximum", "epr1. Quartile", "epr3. Quartile", "eprMean", "eprMedian", "eprSum", "eprSE Mean", "eprLCL Mean", "eprUCL Mean", "eprVariance", "eprStdev", "eprSkewness", "eprKurtosis")
colnames(yreprstats) <-eprcolnames

yrknstats <- tapply(pwt9$kn, pwt9$year, basicStats)
yrknstats <- as.data.frame(yrknstats)
yrknstats <- t(yrknstats)
kncolnames <- c( "knnobs", "knNAs", "knMinimum", "knMaximum", "kn1. Quartile", "kn3. Quartile", "knMean", "knMedian", "knSum", "knSE Mean", "knLCL Mean", "knUCL Mean", "knVariance", "knStdev", "knSkewness", "knKurtosis")
colnames(yrknstats) <-kncolnames

yrkwstats <- tapply(pwt9$kw, pwt9$year, basicStats)
yrkwstats <- as.data.frame(yrkwstats)
yrkwstats <- t(yrkwstats)
kwcolnames <- c( "kwnobs", "kwNAs", "kwMinimum", "kwMaximum", "kw1. Quartile", "kw3. Quartile", "kwMean", "kwMedian", "kwSum", "kwSE Mean", "kwLCL Mean", "kwUCL Mean", "kwVariance", "kwStdev", "kwSkewness", "kwKurtosis")
colnames(yrkwstats) <-kwcolnames

yrklstats <- tapply(pwt9$kl, pwt9$year, basicStats)
yrklstats <- as.data.frame(yrklstats)
yrklstats <- t(yrklstats)
klcolnames <- c( "klnobs", "klNAs", "klMinimum", "klMaximum", "kl1. Quartile", "kl3. Quartile", "klMean", "klMedian", "klSum", "klSE Mean", "klLCL Mean", "klUCL Mean", "klVariance", "klStdev", "klSkewness", "klKurtosis")
colnames(yrklstats) <-klcolnames

yrkhstats <- tapply(pwt9$kh, pwt9$year, basicStats)
yrkhstats <- as.data.frame(yrkhstats)
yrkhstats <- t(yrkhstats)
khcolnames <- c( "khnobs", "khNAs", "khMinimum", "khMaximum", "kh1. Quartile", "kh3. Quartile", "khMean", "khMedian", "khSum", "khSE Mean", "khLCL Mean", "khUCL Mean", "khVariance", "khStdev", "khSkewness", "khKurtosis")
colnames(yrkhstats) <-khcolnames

yrestats <- tapply(pwt9$e, pwt9$year, basicStats)
yrestats <- as.data.frame(yrestats)
yrestats <- t(yrestats)
ecolnames <- c( "enobs", "eNAs", "eMinimum", "eMaximum", "e1. Quartile", "e3. Quartile", "eMean", "eMedian", "eSum", "eSE Mean", "eLCL Mean", "eUCL Mean", "eVariance", "eStdev", "eSkewness", "eKurtosis")
colnames(yrestats) <-ecolnames

yrepsstats <- tapply(pwt9$eps, pwt9$year, basicStats)
yrepsstats <- as.data.frame(yrepsstats)
yrepsstats <- t(yrepsstats)
epscolnames <- c( "epsnobs", "epsNAs", "epsMinimum", "epsMaximum", "eps1. Quartile", "eps3. Quartile", "epsMean", "epsMedian", "epsSum", "epsSE Mean", "epsLCL Mean", "epsUCL Mean", "epsVariance", "epsStdev", "epsSkewness", "epsKurtosis")
colnames(yrepsstats) <-epscolnames

yrepsnostats <- tapply(pwt9$epsno, pwt9$year, basicStats)
yrepsnostats <- as.data.frame(yrepsnostats)
yrepsnostats <- t(yrepsnostats)
epsnocolnames <- c( "epsnonobs", "epsnoNAs", "epsnoMinimum", "epsnoMaximum", "epsno1. Quartile", "epsno3. Quartile", "epsnoMean", "epsnoMedian", "epsnoSum", "epsnoSE Mean", "epsnoLCL Mean", "epsnoUCL Mean", "epsnoVariance", "epsnoStdev", "epsnoSkewness", "epsnoKurtosis")
colnames(yrepsnostats) <-epsnocolnames

yrepsastats <- tapply(pwt9$epsa, pwt9$year, basicStats)
yrepsastats <- as.data.frame(yrepsastats)
yrepsastats <- t(yrepsastats)
epsacolnames <- c( "epsanobs", "epsaNAs", "epsaMinimum", "epsaMaximum", "epsa1. Quartile", "epsa3. Quartile", "epsaMean", "epsaMedian", "epsaSum", "epsaSE Mean", "epsaLCL Mean", "epsaUCL Mean", "epsaVariance", "epsaStdev", "epsaSkewness", "epsaKurtosis")
colnames(yrepsastats) <-epsacolnames

yrrhostats <- tapply(pwt9$rho, pwt9$year, basicStats)
yrrhostats <- as.data.frame(yrrhostats)
yrrhostats <- t(yrrhostats)
rhocolnames <- c( "rhonobs", "rhoNAs", "rhoMinimum", "rhoMaximum", "rho1. Quartile", "rho3. Quartile", "rhoMean", "rhoMedian", "rhoSum", "rhoSE Mean", "rhoLCL Mean", "rhoUCL Mean", "rhoVariance", "rhoStdev", "rhoSkewness", "rhoKurtosis")
colnames(yrrhostats) <-rhocolnames

yrrstats <- tapply(pwt9$r, pwt9$year, basicStats)
yrrstats <- as.data.frame(yrrstats)
yrrstats <- t(yrrstats)
rcolnames <- c( "rnobs", "rNAs", "rMinimum", "rMaximum", "r1. Quartile", "r3. Quartile", "rMean", "rMedian", "rSum", "rSE Mean", "rLCL Mean", "rUCL Mean", "rVariance", "rStdev", "rSkewness", "rKurtosis")
colnames(yrrstats) <-rcolnames

yrkapstats <- tapply(pwt9$kap, pwt9$year, basicStats)
yrkapstats <- as.data.frame(yrkapstats)
yrkapstats <- t(yrkapstats)
kapcolnames <- c( "kapnobs", "kapNAs", "kapMinimum", "kapMaximum", "kap1. Quartile", "kap3. Quartile", "kapMean", "kapMedian", "kapSum", "kapSE Mean", "kapLCL Mean", "kapUCL Mean", "kapVariance", "kapStdev", "kapSkewness", "kapKurtosis")
colnames(yrkapstats) <-kapcolnames

yrkapnostats <- tapply(pwt9$kapno, pwt9$year, basicStats)
yrkapnostats <- as.data.frame(yrkapnostats)
yrkapnostats <- t(yrkapnostats)
kapnocolnames <- c( "kapnonobs", "kapnoNAs", "kapnoMinimum", "kapnoMaximum", "kapno1. Quartile", "kapno3. Quartile", "kapnoMean", "kapnoMedian", "kapnoSum", "kapnoSE Mean", "kapnoLCL Mean", "kapnoUCL Mean", "kapnoVariance", "kapnoStdev", "kapnoSkewness", "kapnoKurtosis")
colnames(yrkapnostats) <-kapnocolnames

yrkapastats <- tapply(pwt9$kapa, pwt9$year, basicStats)
yrkapastats <- as.data.frame(yrkapastats)
yrkapastats <- t(yrkapastats)
kapacolnames <- c( "kapanobs", "kapaNAs", "kapaMinimum", "kapaMaximum", "kapa1. Quartile", "kapa3. Quartile", "kapaMean", "kapaMedian", "kapaSum", "kapaSE Mean", "kapaLCL Mean", "kapaUCL Mean", "kapaVariance", "kapaStdev", "kapaSkewness", "kapaKurtosis")
colnames(yrkapastats) <-kapacolnames

descr <- cbind(des, yrNstats, yrEstats, yrLstats, yrLhstats, yrwstats, yrYostats, yrYstats, yrWstats, yrcstats, yrcnostats, yrcastats, yrCstats, yrCnostats, yrCastats, yrKstats, yrynstats, yrywstats, yrylstats, yryhstats, yreprstats, yrknstats, yrkwstats, yrklstats, yrkhstats, yrestats, yrepsstats, yrepsnostats, yrepsastats, yrrhostats, yrrstats, yrkapstats, yrkapnostats, yrkapastats)
colnames(descr)[1] <- "year"
write.table(descr, "descr.csv", sep=",")

plot(descr$year, descr$NMedian, ylim=c(0,70), lwd=1, type="line", main="Population: mean & median", sub="Source: PWT 9.0", xlab="Year", ylab="Millions")
polygon(x=c(descr$year, rev(descr$year)), y=c(descr$`NLCL Mean`, rev(descr$`NUCL Mean`)), col="lightgray", border=NA)
lines(descr$year, descr$NMean, col="black", lwd=3)
lines(descr$year, descr$`NLCL Mean`, col="black", lty=2)
lines(descr$year, descr$`NUCL Mean`, col="black", lty=2)
legend("topleft", c("Mean","Median", "95% CI"), lwd=c(3,1,10), lty=c(1,1,1), col=c("black","black", "lightgray"))

plot(descr$year, descr$EMedian, ylim=c(0,30), lwd=1, type="line", main="Employment: mean, median, and 95% CI", ylab="Millions", xlab="Year")
polygon(x=c(descr$year, rev(descr$year)), y=c(descr$`ELCL Mean`, rev(descr$`EUCL Mean`)), col="lightgray", border=NA)
lines(descr$year, descr$EMean, col="black", lwd=3)
lines(descr$year, descr$`ELCL Mean`, col="black", lty=2)
lines(descr$year, descr$`EUCL Mean`, col="black", lty=2)
legend("topleft", c("Mean","Median", "95% CI"), lwd=c(3,1,10), lty=c(1,1,1), col=c("black","black", "lightgray"))

plot(descr$year, descr$LMedian, ylim=c(0,90000), lwd=1, type="line", main="Labor time: Mean, median, and 95% CI", ylab="Million hours", xlab="Year")
polygon(x=c(descr$year, rev(descr$year)), y=c(descr$`LLCL Mean`, rev(descr$`LUCL Mean`)), col="lightgray", border=NA)
lines(descr$year, descr$LMean, lwd=3)
lines(descr$year, descr$`LLCL Mean`, lty=2)
lines(descr$year, descr$`LUCL Mean`, lty=2)
legend("topleft", c("Mean","Median", "95% CI"), lwd=c(3,1,10), lty=c(1,1,1), col=c("black","black", "lightgray"))

plot(descr$year, descr$LhMedian, ylim=c(0,225000), lwd=1, type="line", main="Skilled-adjusted labor time: Mean, median, and 95% CI", ylab="Index", xlab="Year")
polygon(x=c(descr$year, rev(descr$year)), y=c(descr$`LhLCL Mean`, rev(descr$`LhUCL Mean`)), col="lightgray", border=NA)
lines(descr$year, descr$LhMean, lwd=3)
lines(descr$year, descr$`LhLCL Mean`, lty=2)
lines(descr$year, descr$`LhUCL Mean`, lty=2)
legend("topleft", c("Mean","Median", "95% CI"), lwd=c(3,1,10), lty=c(1,1,1), col=c("black","black", "lightgray"))

plot(descr$year, descr$wMedian, ylim=c(.45,.65), lwd=1, type="line", main="Wage share: Mean, median, and 95% CI", ylab=" ", xlab="Year")
polygon(x=c(descr$year, rev(descr$year)), y=c(descr$`wLCL Mean`, rev(descr$`wUCL Mean`)), col="lightgray", border=NA)
lines(descr$year, descr$wMedian, lwd=1)
lines(descr$year, descr$wMean, lwd=3)
lines(descr$year, descr$`wLCL Mean`, lty=2)
lines(descr$year, descr$`wUCL Mean`, lty=2)
legend("topright", c("Mean","Median", "95% CI"), lwd=c(3,1,10), lty=c(1,1,1), col=c("black","black", "lightgray"))

plot(descr$year, descr$YoMedian, ylim=c(0,850000), lwd=2, type="line", col="blue", main="Mean & median of Yo for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$YoMean, lwd=2, col="red")
lines(descr$year, descr$`YoLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`YoUCL Mean`, col="red", lty=2)

plot(descr$year, descr$YMedian, ylim=c(0,850000), lwd=2, type="line", col="blue", main="Mean & median of Y for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$YMean, lwd=2, col="red")
lines(descr$year, descr$`YLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`YUCL Mean`, col="red", lty=2)

plot(descr$year, descr$WMedian, ylim=c(0,650000), lwd=2, type="line", col="blue", main="Mean & median of W for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$WMean, lwd=2, col="red")
lines(descr$year, descr$`WLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`WUCL Mean`, col="red", lty=2)

plot(descr$year, descr$cMedian, ylim=c(0.4,0.9), lwd=2, type="line", col="blue", main="Mean & median of c for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$cMean, lwd=2, col="red")
lines(descr$year, descr$`cLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`cUCL Mean`, col="red", lty=2)

plot(descr$year, descr$cnoMedian, ylim=c(0.55,0.75), lwd=2, type="line", col="blue", main="Mean & median of c (no outliers) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$cnoMean, lwd=2, col="red")
lines(descr$year, descr$`cnoLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`cnoUCL Mean`, col="red", lty=2)

plot(descr$year, descr$caMedian, ylim=c(0.55,0.75), lwd=2, type="line", col="blue", main="Mean & median of c (adjusted) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$caMean, lwd=2, col="red")
lines(descr$year, descr$`caLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`caUCL Mean`, col="red", lty=2)

plot(descr$year, descr$CMedian, ylim=c(0,500000), lwd=2, type="line", col="blue", main="Mean & median of C for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$CMean, lwd=2, col="red")
lines(descr$year, descr$`CLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`CUCL Mean`, col="red", lty=2)

plot(descr$year, descr$CnoMedian, ylim=c(0,500000), lwd=2, type="line", col="blue", main="Mean & median of C (no outliers) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$CnoMean, lwd=2, col="red")
lines(descr$year, descr$`CnoLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`CnoUCL Mean`, col="red", lty=2)

plot(descr$year, descr$CaMedian, ylim=c(0,500000), lwd=2, type="line", col="blue", main="Mean & median of C (adjusted) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$CaMean, lwd=2, col="red")
lines(descr$year, descr$`CaLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`CaUCL Mean`, col="red", lty=2)

plot(descr$year, descr$KMedian, ylim=c(0,31e+5), lwd=2, type="line", col="blue", main="Mean & median of K for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$KMean, lwd=2, col="red")
lines(descr$year, descr$`KLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`KUCL Mean`, col="red", lty=2)

plot(descr$year, descr$ynMedian, ylim=c(0,5.2e+4), lwd=2, type="line", col="blue", main="Mean & median of yn for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$ynMean, lwd=2, col="red")
lines(descr$year, descr$`ynLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`ynUCL Mean`, col="red", lty=2)

plot(descr$year, descr$ywMedian, ylim=c(0, 1e+5), lwd=2, type="line", col="blue", main="Mean & median of yw for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$ywMean, lwd=2, col="red")
lines(descr$year, descr$`ywLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`ywUCL Mean`, col="red", lty=2)

plot(descr$year, descr$ylMedian, ylim=c(0,40), lwd=2, type="line", col="blue", main="Mean & median of yl for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$ylMean, lwd=2, col="red")
lines(descr$year, descr$`ylLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`ylUCL Mean`, col="red", lty=2)

plot(descr$year, descr$yhMedian, ylim=c(0,12), lwd=2, type="line", col="blue", main="Mean & median of yh for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$yhMean, lwd=2, col="red")
lines(descr$year, descr$`yhLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`yhUCL Mean`, col="red", lty=2)

plot(descr$year, descr$eprMedian, ylim=c(0.3,0.45), lwd=2, type="line", col="blue", main="Mean & median of E-N ratio for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$eprMean, lwd=2, col="red")
lines(descr$year, descr$`eprLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`eprUCL Mean`, col="red", lty=2)

plot(descr$year, descr$knMedian, ylim=c(0,100000), lwd=2, type="line", col="blue", main="Mean & median of K/N for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$knMean, lwd=2, col="red")
lines(descr$year, descr$`knLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`knUCL Mean`, col="red", lty=2)

plot(descr$year, descr$kwMedian, ylim=c(0,200000), lwd=2, type="line", col="blue", main="Mean & median of K/E for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$kwMean, lwd=2, col="red")
lines(descr$year, descr$`kwLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`kwUCL Mean`, col="red", lty=2)

plot(descr$year, descr$klMedian, ylim=c(0,160), lwd=2, type="line", col="blue", main="Mean & median of K/L for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$klMean, lwd=2, col="red")
lines(descr$year, descr$`klLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`klUCL Mean`, col="red", lty=2)

plot(descr$year, descr$khMedian, ylim=c(0,50), lwd=2, type="line", col="blue", main="Mean & median of kh for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$khMean, lwd=2, col="red")
lines(descr$year, descr$`khLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`khUCL Mean`, col="red", lty=2)

plot(descr$year, descr$eMedian, ylim=c(0.4,1.4), lwd=2, type="line", col="blue", main="Mean & median of e for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$eMean, lwd=2, col="red")
lines(descr$year, descr$`eLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`eUCL Mean`, col="red", lty=2) # Globalization has not reduced spread of e

plot(descr$year, descr$epsMedian, ylim=c(.3,1.5), lwd=2, type="line", col="blue", main="Mean & median of epsilon for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$epsMean, lwd=2, col="red")
lines(descr$year, descr$`epsLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`epsUCL Mean`, col="red", lty=2) # Globalization has not reduced spread of epsilon

plot(descr$year, descr$epsnoMedian, ylim=c(.3,1.5), lwd=2, type="line", col="blue", main="Mean & median of epsilon (no outliers) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$epsnoMean, lwd=2, col="red")
lines(descr$year, descr$`epsnoLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`epsnoUCL Mean`, col="red", lty=2)

plot(descr$year, descr$epsaMedian, ylim=c(.3,1.5), lwd=2, type="line", col="blue", main="Mean & median of epsilon (adjusted) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$epsaMean, lwd=2, col="red")
lines(descr$year, descr$`epsaLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`epsaUCL Mean`, col="red", lty=2)

plot(descr$year, descr$rhoMedian, ylim=c(.2,.9), lwd=2, type="line", col="blue", main="Mean & median of rho for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$rhoMean, lwd=2, col="red")
lines(descr$year, descr$`rhoLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`rhoUCL Mean`, col="red", lty=2) # Globalization has reduced spread

plot(descr$year, descr$rMedian, ylim=c(.05,.4), lwd=2, type="line", col="blue", main="Mean & median of r for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$rMean, lwd=2, col="red")
lines(descr$year, descr$`rLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`rUCL Mean`, col="red", lty=2) # Globalization has reduced spread

plot(descr$year, descr$kapMedian, ylim=c(0.4,1.5), lwd=2, type="line", col="blue", main="Mean & median of kappa for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$kapMean, lwd=2, col="red")
lines(descr$year, descr$`kapLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`kapUCL Mean`, col="red", lty=2)

plot(descr$year, descr$kapnoMedian, ylim=c(0.4,1.5), lwd=2, type="line", col="blue", main="Mean & median of kappa (no outliers) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$kapnoMean, lwd=2, col="red")
lines(descr$year, descr$`kapnoLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`kapnoUCL Mean`, col="red", lty=2)

plot(descr$year, descr$kapaMedian, ylim=c(0.4,1.5), lwd=2, type="line", col="blue", main="Mean & median of kappa (adjusted) for 182 countries", ylab="", xlab="Year")
lines(descr$year, descr$kapaMean, lwd=2, col="red")
lines(descr$year, descr$`kapaLCL Mean`, col="red", lty=2)
lines(descr$year, descr$`kapaUCL Mean`, col="red", lty=2)

# END OF THIS PART


