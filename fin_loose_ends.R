# Julio Huato
# Financialization project
# 4/18/2014
# Has "finance" growth outpaced output growth?

# WB WDI all available finance indicators 1960-2013
# Ten largest economies (WB PPP 2012 GDP)
# This loads the data file and checks it.
wdifin10 <- read.csv("~/Downloads/wdi10.csv")
View(wdifin10)
str(wdifin10)

# This loads the package ggplot2.
library(ggplot2)

# Automated teller machines (ATMs) (per 100,000 adults)
ggplot(wdifin10, aes(x = year, y = FB.ATM.TOTL.P5)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Automated teller machines (ATMs) (per 100,000 adults)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Bank capital to assets ratio (%)
ggplot(wdifin10, aes(x = year, y = FB.BNK.CAPA.ZS)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Bank capital to assets ratio (%)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Bank liquid reserves to bank assets ratio (%)
ggplot(wdifin10, aes(x = year, y = FD.RES.LIQU.AS.ZS)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Bank liquid reserves to bank assets ratio (%)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Bank nonperforming loans to total gross loans (%)
ggplot(wdifin10, aes(x = year, y = FB.AST.NPER.ZS)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Bank nonperforming loans to total gross loans (%)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Borrowers from commercial banks (per 1,000 adults)
ggplot(wdifin10, aes(x = year, y = FB.CBK.BRWR.P3)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Borrowers from commercial banks (per 1,000 adults)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Broad money (% of GDP)
ggplot(wdifin10, aes(x = year, y = FM.LBL.BMNY.GD.ZS)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Broad money (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Broad money (current LCU)
ggplot(wdifin10, aes(x = year, y = log(FM.LBL.BMNY.CN))) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Broad money (current LCU)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Broad money growth (annual %)
ggplot(wdifin10, aes(x = year, y = FM.LBL.BMNY.ZG)) +
  theme_bw() +
  xlim(1999,2013) +
  ylim(0,100) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Broad money growth (annual %)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Broad money to total reserves ratio
ggplot(wdifin10, aes(x = year, y = FM.LBL.BMNY.IR.ZS)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Broad money to total reserves ratio") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Claims on central government (annual growth as % of 
# broad money)
ggplot(wdifin10, aes(x = year, y = FM.AST.CGOV.ZG.M3)) +
  theme_bw() +
  xlim(1999,2013) +
  ylim(-10,50) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Claims on central government (annual growth as % of broad money)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Claims on central government, etc. (% GDP)
ggplot(wdifin10, aes(x = year, y = FS.AST.CGOV.GD.ZS)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Claims on central government, etc. (% GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Claims on other sectors of the domestic economy 
# (% of GDP)
ggplot(wdifin10, aes(x = year, y = FS.AST.DOMO.GD.ZS)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Claims on other sectors of the domestic economy (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Claims on other sectors of the domestic economy
# (annual growth as % of broad money)
ggplot(wdifin10, aes(x = year, y = FM.AST.DOMO.ZG.M3)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Claims on other sectors of the domestic economy (annual growth as % of broad money)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Claims on private sector (annual growth as %
# of broad money)
ggplot(wdifin10, aes(x = year, y = FM.AST.PRVT.ZG.M3)) +
  theme_bw() +
  xlim(1999,2013) +
  ylim(0,50) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Claims on private sector (annual growth as % of broad money)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Commercial bank branches (per 100,000 adults)
ggplot(wdifin10, aes(x = year, y = FB.CBK.BRCH.P5)) +
  theme_bw() +
  xlim(1999,2013) +
  ylim(0,50) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Commercial bank branches (per 100,000 adults)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Consumer price index (2005 = 100)
ggplot(wdifin10, aes(x = year, y = FP.CPI.TOTL)) +
  theme_bw() +
  xlim(1999,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Consumer price index (2005 = 100)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# DEC alternative conversion factor (LCU per USD)
ggplot(wdifin10, aes(x = year, y = PA.NUS.ATLS)) +
  theme_bw() +
  xlim(1999,2013) +
  ylim(0,130) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "DEC alternative conversion factor (LCU per USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Deposit interest rate (%)
ggplot(wdifin10, aes(x = year, y = FR.INR.DPST)) +
  theme_bw() +
  xlim(1960,2013) +
  ylim(0,95) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Deposit interest rate (%)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Depositors with commercial banks (per 1,000 adults)
ggplot(wdifin10, aes(x = year, y = FB.CBK.DPTR.P3)) +
  theme_bw() +
  xlim(2000,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Depositors with commercial banks (per 1,000 adults)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Domestic credit provided by ﬁnancial sector 
# (% of GDP)
ggplot(wdifin10, aes(x = year, y = FS.AST.DOMS.GD.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Domestic credit provided by ﬁnancial sector (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Domestic credit to private sector (% of GDP)
ggplot(wdifin10, aes(x = year, y = FS.AST.PRVT.GD.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Domestic credit to private sector (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Domestic credit to private sector by banks (% of GDP)
ggplot(wdifin10, aes(x = year, y = FD.AST.PRVT.GD.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Domestic credit to private sector by banks (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# GDP deﬂator (base year varies by country)
ggplot(wdifin10, aes(x = year, y = NY.GDP.DEFL.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "GDP deﬂator (base year varies by country)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Inﬂation, GDP deﬂator (annual %)
ggplot(wdifin10, aes(x = year, y = NY.GDP.DEFL.KD.ZG)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Inﬂation, GDP deﬂator (annual %)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Inﬂation, consumer prices (annual %)
ggplot(wdifin10, aes(x = year, y = FP.CPI.TOTL.ZG)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Inﬂation, consumer prices (annual %)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Interest rate spread (lending rate minus 
# deposit rate, %)
ggplot(wdifin10, aes(x = year, y = FR.INR.LNDP)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Interest rate spread (lending rate minus deposit rate, %)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Lending interest rate (%)
ggplot(wdifin10, aes(x = year, y = FR.INR.LEND)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Lending interest rate (%)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Liquid liabilities (M3) as % of GDP
ggplot(wdifin10, aes(x = year, y = FS.LBL.LIQU.GD.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Liquid liabilities (M3) as % of GDP") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Listed domestic companies, total
ggplot(wdifin10, aes(x = year, y = CM.MKT.LDOM.NO)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Listed domestic companies, total") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Market capitalization of listed companies (% of GDP)
ggplot(wdifin10, aes(x = year, y = CM.MKT.LCAP.GD.ZS)) +
  theme_bw() +
  xlim(1987,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Market capitalization of listed companies (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Log of market capitalization of listed companies (current USD)
ggplot(wdifin10, aes(x = year, y = log(CM.MKT.LCAP.CD))) +
  theme_bw() +
  xlim(1987,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of market capitalization of listed companies (current USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Log of money (current LCU)
ggplot(wdifin10, aes(x = year, y = log(FM.LBL.MONY.CN))) +
  theme_bw() +
  xlim(1987,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of money (current LCU)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Log of money and quasi money (M2) (current LCU)
# [Divide this by the GDP deflator]
ggplot(wdifin10, aes(x = year, y = log(FM.LBL.MQMY.CN))) +
  theme_bw() +
  xlim(1987,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Log of money and quasi money (M2) (current LCU)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Money and quasi money (M2) as % of GDP
ggplot(wdifin10, aes(x = year, y = FM.LBL.MQMY.GD.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Money and quasi money (M2) as % of GDP") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Money and quasi money (M2) to total reserves ratio
ggplot(wdifin10, aes(x = year, y = FM.LBL.MQMY.IR.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Money and quasi money (M2) to total reserves ratio") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Money and quasi money growth (annual %)
ggplot(wdifin10, aes(x = year, y = FM.LBL.MQMY.ZG)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Money and quasi money growth (annual %)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Net domestic credit (current LCU)
# This comparison makes no sense: different LCUs
ggplot(wdifin10, aes(x = year, y = FM.AST.DOMS.CN)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Net domestic credit (current LCU)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Net foreign assets (current LCU)
ggplot(wdifin10, aes(x = year, y = FM.AST.NFRG.CN)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Net foreign assets (current LCU)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Official exchange rate (LCU per USD, period average)
ggplot(wdifin10, aes(x = year, y = PA.NUS.FCRF)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Official exchange rate (LCU per USD, period average)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Point-of-sale terminals (per 100,000 adults)
ggplot(wdifin10, aes(x = year, y = FB.POS.TOTL.P5)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Point-of-sale terminals (per 100,000 adults)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Quasi money (current LCU)
ggplot(wdifin10, aes(x = year, y = FM.LBL.QMNY.CN)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Quasi money (current LCU)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Quasi-liquid liabilities (% of GDP)
ggplot(wdifin10, aes(x = year, y = FS.LBL.QLIQ.GD.ZS)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Quasi-liquid liabilities (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Real effective exchange rate index (2005 = 100)
ggplot(wdifin10, aes(x = year, y = PX.REX.REER)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Real effective exchange rate index (2005 = 100)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Real interest rate (%)
ggplot(wdifin10, aes(x = year, y = FR.INR.RINR)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Real interest rate (%)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Risk premium on lending (lending rate 
# minus treasury bill rate, %)
ggplot(wdifin10, aes(x = year, y = FR.INR.RISK)) +
  theme_bw() +
  xlim(1960,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Risk premium on lending (lending rate minus treasury bill rate, %)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# S&P Global Equity lndices (annual % change)
ggplot(wdifin10, aes(x = year, y = CM.MKT.INDX.ZG)) +
  theme_bw() +
  xlim(1989,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "S&P Global Equity lndices (annual % change)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Stocks traded, total value (% of GDP)
ggplot(wdifin10, aes(x = year, y = CM.MKT.TRAD.GD.ZS)) +
  theme_bw() +
  xlim(1986,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Stocks traded, total value (% of GDP)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Stocks traded, log of total value (current USD)
ggplot(wdifin10, aes(x = year, y = log(CM.MKT.TRAD.CD))) +
  theme_bw() +
  xlim(1986,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Stocks traded, log of total value (current USD)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Stocks traded, turnover ratio (%)
ggplot(wdifin10, aes(x = year, y = CM.MKT.TRNR)) +
  theme_bw() +
  xlim(1986,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Stocks traded, turnover ratio (%)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# Wholesale price index (2005 = 100)
ggplot(wdifin10, aes(x = year, y = FP.WPI.TOTL)) +
  theme_bw() +
  xlim(1986,2013) +
  opts(axis.line = theme_segment(colour = "black"),
       panel.background = theme_blank()) +
  geom_step() +
  labs(title = "Wholesale price index (2005 = 100)") +
  ylab("") +
  facet_wrap(~ country, ncol=2)

# WB WDI all available finance indicators 1960-2013
# World
# This opens the data file and checks it.
wwdifin <- read.csv("~/Downloads/wdifin.csv")
View(wwdifin)
str(wwdifin)

# Bank capital to assets ratio (%)
plot(wwdifin$year, wwdifin$FB.BNK.CAPA.ZS, xlab="year", ylab="", type="s", main="Bank capital to assets ratio (%)", sub="World, Source: WDI")

# Bank nonperforming loans to total gross loans (%)
plot(wwdifin$year, wwdifin$FB.AST.NPER.ZS, xlab="year", ylab="", type="s", main="Bank nonperforming loans to total gross loans (%)", sub="World, Source: WDI")

# Claims on central government, etc. (% GDP)
plot(wwdifin$year, wwdifin$FS.AST.CGOV.GD.ZS, xlab="year", ylab="", type="s", main="Claims on central government, etc. (% GDP)", sub="World, Source: WDI")

# Claims on other sectors of the domestic economy (% of GDP)
plot(wwdifin$year, wwdifin$FS.AST.DOMO.GD.ZS, xlab="year", ylab="", type="s", main="Claims on other sectors of the domestic economy (% of GDP)", sub="World, Source: WDI")

# Domestic credit provided by financial sector (% of GDP)
plot(wwdifin$year, wwdifin$FS.AST.DOMS.GD.ZS, xlab="year", ylab="", type="s", main="Domestic credit provided by financial sector (% of GDP)", sub="World, Source: WDI")

# Domestic credit to private sector (% of GDP)
plot(wwdifin$year, wwdifin$FS.AST.PRVT.GD.ZS, xlab="year", ylab="", type="s", main="Domestic credit to private sector (% of GDP)", sub="World, Source: WDI")

# Domestic credit to private sector by banks (% of GDP)
plot(wwdifin$year, wwdifin$FD.AST.PRVT.GD.ZS, xlab="year", ylab="", type="s", main="Domestic credit to private sector by banks (% of GDP)", sub="World, Source: WDI")

# End


