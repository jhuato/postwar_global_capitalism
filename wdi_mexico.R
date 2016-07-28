# WDI report (Mexico)
# Piketty db
wid_mex <- read.csv("~/Downloads/WID-report_Mexico.csv")
require(ggplot2)
summary(wid_mex)
names(wid_mex)

plots<-list()
for(i in 2:6)
{
  yvar<-wid_mex[, i]
 plots[[i]]<-qplot(wid_mex[,1], yvar, main=cbind("Mexico", names(wid_mex[i])), data=wid_mex, geom ="line",xlim = c(1965,2013), xlab=names(wid_mex[i]), ylab="")
}
print(plots[[2]])
print(plots[[3]])
print(plots[[4]])
print(plots[[5]])
print(plots[[6]])


qplot(wid_mex[,1], wid_mex[,2], main="Mexico", sub=names(wid_mex[2]), data=wid_mex, geom ="line",xlim = c(1985,2013), xlab=names(wid_mex[2]), ylab="")
qplot(wid_mex[,1], wid_mex[,3], main="Mexico", sub=names(wid_mex[3]), data=wid_mex, geom ="line",xlim = c(1985,2013), xlab=names(wid_mex[3]), ylab="")
qplot(wid_mex[,1], wid_mex[,4], main="Mexico", sub=names(wid_mex[4]), data=wid_mex, geom ="line",xlim = c(1985,2013), xlab=names(wid_mex[4]), ylab="")
