# Julio Huato
# Financialization project
# 4/18/2014
# Has "finance" growth outpaced output growth?

# OECD selected finance indicators 1950-2013
# Largest economies (WB PPP 2012 GDP) with France & Germany as EU18
# This loads the data file and checks it.
oecdm <- read.csv("~/Downloads/oecdm.csv")
View(oecdm)
str(oecdm)

# This loads the package ggplot2.
library(ggplot2)

# Broad Money (M3) Index 2010=100, SA
# This requires converting to USD
ggplot(oecdm, aes(x = year, y = m3)) +
  theme_bw() +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Broad Money (M3) Index 2010=100, SA") +
  ylab("") +
  facet_wrap(~ country, ncol=3)


bra <- subset(oecdm, country=="Brazil", select = c(year, m3) ) 
bra <- na.omit(bra)
bra.hat <- predict( lm( log(m3) ~ year, data=bra))
qplot(year, log(m3), data=bra, geom="line", main="Brazil Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=bra.hat), col=2)
bra.g <-coefficients( lm( log(m3) ~ year, data=bra) )[2]
bra.g

chi <- subset(oecdm, country=="China", select = c(year, m3) ) 
chi <- na.omit(chi)
chi.hat <- predict( lm( log(m3) ~ year, data=chi))
qplot(year, log(m3), data=chi, geom="line", main="China Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=chi.hat), col=2)
chi.g <-coefficients( lm( log(m3) ~ year, data=chi) )[2]
chi.g

eu <- subset(oecdm, country=="Euro area (18 countries)", select = c(year, m3) ) 
eu <- na.omit(eu)
eu.hat <- predict( lm( log(m3) ~ year, data=eu))
qplot(year, log(m3), data=eu, geom="line", main="EU18 Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=eu.hat), col=2)
eu.g <-coefficients( lm( log(m3) ~ year, data=eu) )[2]
eu.g

ind <- subset(oecdm, country=="India", select = c(year, m3) ) 
ind <- na.omit(ind)
ind.hat <- predict( lm( log(m3) ~ year, data=ind))
qplot(year, log(m3), data=ind, geom="line", main="India Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=ind.hat), col=2)
ind.g <-coefficients( lm( log(m3) ~ year, data=ind) )[2]
ind.g

jap <- subset(oecdm, country=="Japan", select = c(year, m3) ) 
jap <- na.omit(jap)
jap.hat <- predict( lm( log(m3) ~ year, data=jap))
qplot(year, log(m3), data=jap, geom="line", main="Japan Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=jap.hat), col=2)
jap.g <-coefficients( lm( log(m3) ~ year, data=jap) )[2]
jap.g

mex <- subset(oecdm, country=="Mexico", select = c(year, m3) ) 
mex <- na.omit(mex)
mex.hat <- predict( lm( log(m3) ~ year, data=mex))
qplot(year, log(m3), data=mex, geom="line", main="Mexico Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=mex.hat), col=2)
mex.g <-coefficients( lm( log(m3) ~ year, data=mex) )[2]
mex.g

rus <- subset(oecdm, country=="Russian Federation", select = c(year, m3) ) 
rus <- na.omit(jap)
rus.hat <- predict( lm( log(m3) ~ year, data=rus))
qplot(year, log(m3), data=rus, geom="line", main="Russian Federation Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=rus.hat), col=2)
rus.g <-coefficients( lm( log(m3) ~ year, data=rus) )[2]
rus.g

uk <- subset(oecdm, country=="United Kingdom", select = c(year, m3) ) 
uk <- na.omit(uk)
uk.hat <- predict( lm( log(m3) ~ year, data=uk))
qplot(year, log(m3), data=uk, geom="line", main="United Kingdom Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=uk.hat), col=2)
uk.g <-coefficients( lm( log(m3) ~ year, data=uk) )[2]
uk.g

us <- subset(oecdm, country=="United States", select = c(year, m3) ) 
us <- na.omit(us)
us.hat <- predict( lm( log(m3) ~ year, data=us))
qplot(year, log(m3), data=us, geom="line", main="United States Broad Money (M3) Index 2010=100")
last_plot() + geom_line(aes(x=year, y=us.hat), col=2)
us.g <-coefficients( lm( log(m3) ~ year, data=us) )[2]
us.g
