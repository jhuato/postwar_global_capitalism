# Firearm-related death rate vs. guns per 100 residents
# Julio Huato with data from Seth Ackerman

# Load data sets
fired <- read.csv("~/Downloads/firearmdeath.csv", comment.char="#")
guns <- read.csv("~/Downloads/guns.csv", comment.char="#")

# Merge data sets
firedguns <- merge(fired,guns,by="Country")
summary(firedguns)
plot(firedguns$Guns.per.100.residents..2007.,firedguns$Total.firearm.related.death.rate.2)
cor.test(firedguns$Guns.per.100.residents..2007.,firedguns$Total.firearm.related.death.rate.2)
plot(firedguns$Guns.per.100.residents..2007.,firedguns$Homicides)
cor.test(firedguns$Guns.per.100.residents..2007.,firedguns$Homicides)
