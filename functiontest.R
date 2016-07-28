mydata <- pwt81a
mydata2 <-  subset(mydata, currency_unit!="Zimbabwe Dollar")
summary(mydata)

for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

# x: mydata$rgdp; y: mydata$countrycode
f_plot<-function(x,y){
  for(j in y){
    x1<-subset(x,y==j, select(x))
    ts_x<-ts(x1, start=1950, end =2011, frequency=1)
    plot(ts_x, main="y[1]",xlab="Year",ylab="GDP")
  }
}
x1
f_plot(mydata$rdgp,mydata$countrycode)