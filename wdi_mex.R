require(ggplot2)
par(ask=TRUE)
names(wid_mex)
for(i in 2:113){
  yvar <- wid_mex[,i]
  plot(wid_mex[,1], yvar, main="Mexico", fill=T, data=wid_mex, type ="line", xlab=names(wid_mex[i]), ylab="", xlim=c(1970,2013))
  flush.console()
}
par(ask=FALSE)
