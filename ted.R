# Julio Huato
# TED
# Data file stored on Windows One
ted <- read.delim("~/Documents/ted.txt")
str(ted)
# 128 countries
# year from 1950 through 2015
write.table(ted, "ted.csv", sep=",")
table(ted$vname)
emp <- ted[ which(ted$vname=="Employment (in thousands of persons)"), -4]
ygk <- ted[ which(ted$vname=="Total GDP, in millions of 1990 US$ (converted at Geary Khamis PPPs)"), -4]
yeks <- ted[ which(ted$vname=="Total GDP, in millions of 2014 US$ (converted to 2014 price level with updated 2011 EKS PPPs)"), -4]
wsh <- ted[ which(ted$vname=="Labor share"), -4]
pop <- ted[ which(ted$vname=="Midyear population (in thousands of persons)"), -4]
lab <- ted[ which(ted$vname=="Total annual hours worked (in thousands)"), -4]
tfp <- ted[ which(ted$vname=="Total annual hours worked (in thousands)"), -4]

# Wages and profits
wagesgk <- pop
wagesgk$value <- (wsh$value*ygk$value/100)
wageseks <- pop
wageseks$value <- (wsh$value*yeks$value/100)
profitsgk <- pop
profitsgk$value <- ((100-wsh$value)*ygk$value/100)
profitseks <- pop
profitseks$value <- ((100-wsh$value)*yeks$value/100)
plot(wageseks$value,wagesgk$value, log="xy", pch=19, col="red")
plot(profitseks$value,profitsgk$value, log="xy", pch=19, col="blue")  

wagesgk <- data.frame(wsh$value*ygk$value)
data.fram
wageseks <- (wsh$value*yeks$value)


library(ggplot2)

latamwsh <- read.csv("~/Downloads/latamwsh.csv")
p <- ggplot(latamwsh,aes(x=year,y=labsh), ) + geom_line()
p + facet_wrap(~ country, ncol=2)




par(ask=TRUE)
for(i in 2:128){
  
  for(j in ){ 
p <-  ggplot(tedw,aes(x=Year,y=), ) + geom_line()
print(qplot(c(1950,2015), pop$value, main=paste(countries[i],"/ Source:
                                                 TED"), geom=c("line", "point"), sub="Source: Total Economy Database",
              xlab="Year", ylab="Wage share (% of GDP)"))
  }
}
par(ask=FALSE)

plot(tedlsh[,1], tedlsh$Brazil, main="Wage share (% of GDP)
1950-2015", sub="Source: CB's Total Economy Database", col=2,
     type="line", xlab="Year", ylab="Wage share (% of GDP)", ylim=c(10,90),
     lwd=2)
lines(tedlsh[,1], tedlsh$Colombia, col=3, lwd=2)
lines(tedlsh[,1], tedlsh$Ecuador, col=4, lwd=2)
lines(tedlsh[,1], tedlsh$Mexico, col=5, lwd=2)
lines(tedlsh[,1], tedlsh$Venezuela, col=6, lwd=2)
legend("topleft", c("Brazil","Colombia", "Ecuador", "Mexico",
                    "Venezuela"), lty=c(1,1,1,1), lwd=c(2,2,2,2), col=c(2,3,4,5,6))
