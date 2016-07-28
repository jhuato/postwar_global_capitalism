# Julio Huato
# Tech paper for URPE Reader
# 10/25/2015

# PWT8.1 all available indicators 1950-2011
# http://www.rug.nl/research/ggdc/data/penn-world-table
# This reads the data file and checks its structure.
pwt0 <- read.csv("~/Downloads/pwt81.csv")
str(pwt0)

agpwt0 <-aggregate(pwt0, by=list(pwt0$year),
                     FUN=mean, na.rm=TRUE)

getwd()

write.table(agpwt0, "agptw0.txt", sep="\t")


# This creates the subset of the ten largest economies
# in the world (World Bank PPP 2012 GDP):
# United States, China, India, Japan, Germany, Russia, 
# France, Brazil, United Kingdom, Mexico
pwt010 <- subset(pwt, (countrycode=="USA" | countrycode=="CHN" | countrycode=="IND" | countrycode=="JPN" | countrycode=="DEU" | countrycode=="RUS" | countrycode=="FRA" | countrycode=="BRA" | countrycode=="GBR" | countrycode=="MEX"))

# This loads the package ggplot2 to create the plots.
library(ggplot2)

# Log of expenditure-side real GDP, chained PPPs 
# (in millions of 2005 USD)
ggplot(pwt010, aes(x = year, y = log(rgdpe))) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of expenditure-side real GDP, chained PPPs (millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ countrycode, ncol=2)

# Log of real GDP at constant 2005 national prices
# (in millions of 2005 USD)
ggplot(pwt010, aes(x = year, y = log(rgdpna))) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of real GDP at constant 2005 national prices (millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ countrycode, ncol=2)

# Log of real consumption at constant 2005 national prices 
# (in millions of 2005 USD)
ggplot(pwt010, aes(x = year, y = log(rconna))) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of real consumption at constant 2005 national prices (millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ countrycode, ncol=2)

sapply(pwt010, class)
print(pwt010$)

agpwt010 <-aggregate(pwt010, by=list(pwt010$year),
                    FUN=mean, na.rm=TRUE)
warnings()
