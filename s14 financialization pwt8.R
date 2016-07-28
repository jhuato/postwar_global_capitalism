# Julio Huato
# Financialization project
# 4/18/2014
# Has "finance" growth outpaced output growth?

# PWT8 all available indicators 1950-2011
# http://www.rug.nl/research/ggdc/data/penn-world-table
# This reads the data file and checks its structure.
pwt <- read.csv("~/Downloads/pwt.csv")
str(pwt)

# This creates the subset of the ten largest economies
# in the world (World Bank PPP 2012 GDP):
# United States, China, India, Japan, Germany, Russia, 
# France, Brazil, United Kingdom, Mexico
pwt10 <- subset(pwt, (countryid==22 | countryid==31 | countryid==54 | countryid==42 | countryid==71 | countryid==80 | countryid==102 | countryid==130 | countryid==159 | countryid==56))

# This loads the package ggplot2 to create the plots.
library(ggplot2)

# Log of output-side real GDP, chained PPPs 
# (millions of 2005 USD)
ggplot(pwt10, aes(x = year, y = log(rgdpo))) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of output-side real GDP, chained PPPs (millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Log of expenditure-side real GDP, chained PPPs 
# (in millions of 2005 USD)
ggplot(pwt10, aes(x = year, y = log(rgdpe))) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of expenditure-side real GDP, chained PPPs (millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Index of human capital per person, based on 
# years of schooling and returns to education
ggplot(pwt10, aes(x = year, y = hc)) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Index of human capital per person, based on years of schooling and returns to education") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Capital stock, current PPPs (millions 2005 USD)
ggplot(pwt10, aes(x = year, y = log(ck) )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of capital stock, current PPPs (millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# TFP level, current PPPs (USA=1)
ggplot(pwt10, aes(x = year, y = ctfp )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "TFP level at current PPPs (USA=1)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Log of real GDP, constant 2005 national prices
# ( millions 2005 USD)
ggplot(pwt10, aes(x = year, y = log(rgdpna)  )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of real GDP, constant 2005 national prices ( millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Capital stock, constant 2005 national prices
# (millions 2005 USD)
ggplot(pwt10, aes(x = year, y = log(rkna)   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Capital stock, constant 2005 national prices (millions 2005 USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# TFP at constant national prices (2005=1)
ggplot(pwt10, aes(x = year, y = rtfpna   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "TFP at constant national prices (2005=1)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Share of labour compensation in GDP at current 
# national prices
ggplot(pwt10, aes(x = year, y = labsh   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Share of labour compensation in GDP at current national prices") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Price level of CGDPe (PPP/XR), price level 
# of USA GDPo in 2005=1
ggplot(pwt10, aes(x = year, y = pl_gdpe   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Price level of CGDPe (PPP/XR), price level of USA GDPo in 2005=1") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP-share of household consumption, current PPPs
ggplot(pwt10, aes(x = year, y = csh_c   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP-share of household consumption, current PPPs") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP-share of gross capital formation, current PPPs
ggplot(pwt10, aes(x = year, y = csh_i   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP-share of gross capital formation, current PPPs") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP-share of government consumption, current PPPs
ggplot(pwt10, aes(x = year, y = csh_g   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP-share of government consumption, current PPPs") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP-share of merchandise exports, current PPPs
ggplot(pwt10, aes(x = year, y = csh_x   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP-share of merchandise exports, current PPPs") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP-share of merchandise imports, current PPPs
ggplot(pwt10, aes(x = year, y = csh_m   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP-share of merchandise imports, current PPPs") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP-share of residual trade and GDP statistical
# discrepancy, current PPPs
ggplot(pwt10, aes(x = year, y = csh_r   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP-share of residual trade and GDP statistical discrepancy, current PPPs") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP-share of residual trade and GDP statistical 
# discrepancy, current PPPs
ggplot(pwt10, aes(x = year, y = csh_r   )) +
  theme_bw() +
  xlim(1950,2012) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP-share of residual trade and GDP statistical discrepancy, current PPPs") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

