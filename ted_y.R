# Julio Huato
# TED 
# XLS file (9/15 release)

# Variables of interest:
# GDP (Y) (GK, EKS, XR)
# Population (N)
# Employment (E)
# Labor hours (L)
# Labor share (omega)

# Derived variables:
# Wages (W) = omega*Y
# Profits (Pi) = (1-omega)*Y
# Profit share (pi) = Pi/Y

ted_omega <- read.csv("~/Downloads/ted_omega.csv", header=T)
ted_omega <- ted_omega[-c(1, 24, 27, 30, 62, 81, 99, 112), ]
ted_L <- read.csv("~/Downloads/ted_L.csv", header=T)
ted_L <- ted_L[-c(1, 24, 27, 30, 62, 81, 99, 112), ]
ted_N <- read.csv("~/Downloads/ted_N.csv", header=T)
ted_N <- ted_N[-c(1, 24, 27, 30, 62, 81, 99, 112), ]
ted_E <- read.csv("~/Downloads/ted_E.csv", header=T)
ted_E <- ted_E[-c(1, 24, 27, 30, 62, 81, 99, 112), ]
ted_YXR <- read.csv("~/Downloads/ted_YXR.csv", header=T)
ted_YXR <- ted_YXR[-c(1, 24, 27, 30, 62, 81, 99, 112), ]
ted_YEKS <- read.csv("~/Downloads/ted_YEKS.csv", header=T)
ted_YEKS <- ted_YEKS[-c(1, 24, 27, 30, 62, 81, 99, 112), ]
ted_YGK <- read.csv("~/Downloads/ted_YGK.csv", header=T)
ted_YGK <- ted_YGK[-c(1, 24, 27, 30, 62, 81, 99, 112), ]

require(reshape2)
# Reshape the data sets.
ted_omegaw <- melt(ted_omega, id.vars = "Country")
ted_Lw <- melt(ted_L, id.vars = "Country")
ted_Nw <- melt(ted_N, id.vars = "Country")
ted_Ew <- melt(ted_E, id.vars = "Country")
ted_YXRw <- melt(ted_YXR, id.vars = "Country")
ted_YEKSw <- melt(ted_YEKS, id.vars = "Country")
ted_YGKw <- melt(ted_YGK, id.vars = "Country")

require(car) # Recode the year variable
ted_omegaw$Year <- recode(ted_omegaw$variable, "'X1950'=1950; 'X1951'=1951; 'X1952'=1952; 'X1953'=1953; 'X1954'=1954; 'X1955'=1955; 'X1956'=1956; 'X1957'=1957; 'X1958'=1958; 'X1959'=1959; 'X1960'=1960; 'X1961'=1961; 'X1962'=1962; 'X1963'=1963; 'X1964'=1964; 'X1965'=1965; 'X1966'=1966; 'X1967'=1967; 'X1968'=1968; 'X1969'=1969; 'X1970'=1970; 'X1971'=1971; 'X1972'=1972; 'X1973'=1973; 'X1974'=1974; 'X1975'=1975; 'X1976'=1976; 'X1977'=1977; 'X1978'=1978; 'X1979'=1979; 'X1980'=1980; 'X1981'=1981; 'X1982'=1982; 'X1983'=1983; 'X1984'=1984; 'X1985'=1985; 'X1986'=1986; 'X1987'=1987; 'X1988'=1988; 'X1989'=1989; 'X1990'=1990; 'X1991'=1991; 'X1992'=1992; 'X1993'=1993; 'X1994'=1994; 'X1995'=1995; 'X1996'=1996; 'X1997'=1997; 'X1998'=1998; 'X1999'=1999; 'X2000'=2000; 'X2001'=2001; 'X2002'=2002; 'X2003'=2003; 'X2004'=2004; 'X2005'=2005; 'X2006'=2006; 'X2007'=2007; 'X2008'=2008; 'X2009'=2009; 'X2010'=2010; 'X2011'=2011; 'X2012'=2012; 'X2013'=2013; 'X2014'=2014; 'X2015'=2015; ", as.numeric.result=TRUE, as.factor.result = FALSE)
names(ted_omegaw)[names(ted_omegaw)=="value"] <- "omega"
ted_omega <- ted_omegaw[,-2]
ted_Lw$Year <- recode(ted_Lw$variable, "'X1950'=1950; 'X1951'=1951; 'X1952'=1952; 'X1953'=1953; 'X1954'=1954; 'X1955'=1955; 'X1956'=1956; 'X1957'=1957; 'X1958'=1958; 'X1959'=1959; 'X1960'=1960; 'X1961'=1961; 'X1962'=1962; 'X1963'=1963; 'X1964'=1964; 'X1965'=1965; 'X1966'=1966; 'X1967'=1967; 'X1968'=1968; 'X1969'=1969; 'X1970'=1970; 'X1971'=1971; 'X1972'=1972; 'X1973'=1973; 'X1974'=1974; 'X1975'=1975; 'X1976'=1976; 'X1977'=1977; 'X1978'=1978; 'X1979'=1979; 'X1980'=1980; 'X1981'=1981; 'X1982'=1982; 'X1983'=1983; 'X1984'=1984; 'X1985'=1985; 'X1986'=1986; 'X1987'=1987; 'X1988'=1988; 'X1989'=1989; 'X1990'=1990; 'X1991'=1991; 'X1992'=1992; 'X1993'=1993; 'X1994'=1994; 'X1995'=1995; 'X1996'=1996; 'X1997'=1997; 'X1998'=1998; 'X1999'=1999; 'X2000'=2000; 'X2001'=2001; 'X2002'=2002; 'X2003'=2003; 'X2004'=2004; 'X2005'=2005; 'X2006'=2006; 'X2007'=2007; 'X2008'=2008; 'X2009'=2009; 'X2010'=2010; 'X2011'=2011; 'X2012'=2012; 'X2013'=2013; 'X2014'=2014; 'X2015'=2015; ", as.numeric.result=TRUE, as.factor.result = FALSE)
names(ted_Lw)[names(ted_Lw)=="value"] <- "L"
ted_L <- ted_Lw[,-2]
ted_Nw$Year <- recode(ted_Nw$variable, "'X1950'=1950; 'X1951'=1951; 'X1952'=1952; 'X1953'=1953; 'X1954'=1954; 'X1955'=1955; 'X1956'=1956; 'X1957'=1957; 'X1958'=1958; 'X1959'=1959; 'X1960'=1960; 'X1961'=1961; 'X1962'=1962; 'X1963'=1963; 'X1964'=1964; 'X1965'=1965; 'X1966'=1966; 'X1967'=1967; 'X1968'=1968; 'X1969'=1969; 'X1970'=1970; 'X1971'=1971; 'X1972'=1972; 'X1973'=1973; 'X1974'=1974; 'X1975'=1975; 'X1976'=1976; 'X1977'=1977; 'X1978'=1978; 'X1979'=1979; 'X1980'=1980; 'X1981'=1981; 'X1982'=1982; 'X1983'=1983; 'X1984'=1984; 'X1985'=1985; 'X1986'=1986; 'X1987'=1987; 'X1988'=1988; 'X1989'=1989; 'X1990'=1990; 'X1991'=1991; 'X1992'=1992; 'X1993'=1993; 'X1994'=1994; 'X1995'=1995; 'X1996'=1996; 'X1997'=1997; 'X1998'=1998; 'X1999'=1999; 'X2000'=2000; 'X2001'=2001; 'X2002'=2002; 'X2003'=2003; 'X2004'=2004; 'X2005'=2005; 'X2006'=2006; 'X2007'=2007; 'X2008'=2008; 'X2009'=2009; 'X2010'=2010; 'X2011'=2011; 'X2012'=2012; 'X2013'=2013; 'X2014'=2014; 'X2015'=2015; ", as.numeric.result=TRUE, as.factor.result = FALSE)
names(ted_Nw)[names(ted_Nw)=="value"] <- "N"
ted_N <- ted_Nw[,-2]
ted_Ew$Year <- recode(ted_Ew$variable, "'X1950'=1950; 'X1951'=1951; 'X1952'=1952; 'X1953'=1953; 'X1954'=1954; 'X1955'=1955; 'X1956'=1956; 'X1957'=1957; 'X1958'=1958; 'X1959'=1959; 'X1960'=1960; 'X1961'=1961; 'X1962'=1962; 'X1963'=1963; 'X1964'=1964; 'X1965'=1965; 'X1966'=1966; 'X1967'=1967; 'X1968'=1968; 'X1969'=1969; 'X1970'=1970; 'X1971'=1971; 'X1972'=1972; 'X1973'=1973; 'X1974'=1974; 'X1975'=1975; 'X1976'=1976; 'X1977'=1977; 'X1978'=1978; 'X1979'=1979; 'X1980'=1980; 'X1981'=1981; 'X1982'=1982; 'X1983'=1983; 'X1984'=1984; 'X1985'=1985; 'X1986'=1986; 'X1987'=1987; 'X1988'=1988; 'X1989'=1989; 'X1990'=1990; 'X1991'=1991; 'X1992'=1992; 'X1993'=1993; 'X1994'=1994; 'X1995'=1995; 'X1996'=1996; 'X1997'=1997; 'X1998'=1998; 'X1999'=1999; 'X2000'=2000; 'X2001'=2001; 'X2002'=2002; 'X2003'=2003; 'X2004'=2004; 'X2005'=2005; 'X2006'=2006; 'X2007'=2007; 'X2008'=2008; 'X2009'=2009; 'X2010'=2010; 'X2011'=2011; 'X2012'=2012; 'X2013'=2013; 'X2014'=2014; 'X2015'=2015; ", as.numeric.result=TRUE, as.factor.result = FALSE)
names(ted_Ew)[names(ted_Ew)=="value"] <- "E"
ted_E <- ted_Ew[,-2]
ted_YXRw$Year <- recode(ted_YXRw$variable, "'X1950'=1950; 'X1951'=1951; 'X1952'=1952; 'X1953'=1953; 'X1954'=1954; 'X1955'=1955; 'X1956'=1956; 'X1957'=1957; 'X1958'=1958; 'X1959'=1959; 'X1960'=1960; 'X1961'=1961; 'X1962'=1962; 'X1963'=1963; 'X1964'=1964; 'X1965'=1965; 'X1966'=1966; 'X1967'=1967; 'X1968'=1968; 'X1969'=1969; 'X1970'=1970; 'X1971'=1971; 'X1972'=1972; 'X1973'=1973; 'X1974'=1974; 'X1975'=1975; 'X1976'=1976; 'X1977'=1977; 'X1978'=1978; 'X1979'=1979; 'X1980'=1980; 'X1981'=1981; 'X1982'=1982; 'X1983'=1983; 'X1984'=1984; 'X1985'=1985; 'X1986'=1986; 'X1987'=1987; 'X1988'=1988; 'X1989'=1989; 'X1990'=1990; 'X1991'=1991; 'X1992'=1992; 'X1993'=1993; 'X1994'=1994; 'X1995'=1995; 'X1996'=1996; 'X1997'=1997; 'X1998'=1998; 'X1999'=1999; 'X2000'=2000; 'X2001'=2001; 'X2002'=2002; 'X2003'=2003; 'X2004'=2004; 'X2005'=2005; 'X2006'=2006; 'X2007'=2007; 'X2008'=2008; 'X2009'=2009; 'X2010'=2010; 'X2011'=2011; 'X2012'=2012; 'X2013'=2013; 'X2014'=2014; 'X2015'=2015; ", as.numeric.result=TRUE, as.factor.result = FALSE)
names(ted_YXRw)[names(ted_YXRw)=="value"] <- "YXR"
ted_YXR <- ted_YXRw[,-2]
ted_YEKSw$Year <- recode(ted_YEKSw$variable, "'X1950'=1950; 'X1951'=1951; 'X1952'=1952; 'X1953'=1953; 'X1954'=1954; 'X1955'=1955; 'X1956'=1956; 'X1957'=1957; 'X1958'=1958; 'X1959'=1959; 'X1960'=1960; 'X1961'=1961; 'X1962'=1962; 'X1963'=1963; 'X1964'=1964; 'X1965'=1965; 'X1966'=1966; 'X1967'=1967; 'X1968'=1968; 'X1969'=1969; 'X1970'=1970; 'X1971'=1971; 'X1972'=1972; 'X1973'=1973; 'X1974'=1974; 'X1975'=1975; 'X1976'=1976; 'X1977'=1977; 'X1978'=1978; 'X1979'=1979; 'X1980'=1980; 'X1981'=1981; 'X1982'=1982; 'X1983'=1983; 'X1984'=1984; 'X1985'=1985; 'X1986'=1986; 'X1987'=1987; 'X1988'=1988; 'X1989'=1989; 'X1990'=1990; 'X1991'=1991; 'X1992'=1992; 'X1993'=1993; 'X1994'=1994; 'X1995'=1995; 'X1996'=1996; 'X1997'=1997; 'X1998'=1998; 'X1999'=1999; 'X2000'=2000; 'X2001'=2001; 'X2002'=2002; 'X2003'=2003; 'X2004'=2004; 'X2005'=2005; 'X2006'=2006; 'X2007'=2007; 'X2008'=2008; 'X2009'=2009; 'X2010'=2010; 'X2011'=2011; 'X2012'=2012; 'X2013'=2013; 'X2014'=2014; 'X2015'=2015; ", as.numeric.result=TRUE, as.factor.result = FALSE)
names(ted_YEKSw)[names(ted_YEKSw)=="value"] <- "YEKS"
ted_YEKS <- ted_YEKSw[,-2]
ted_YGKw$Year <- recode(ted_YGKw$variable, "'X1950'=1950; 'X1951'=1951; 'X1952'=1952; 'X1953'=1953; 'X1954'=1954; 'X1955'=1955; 'X1956'=1956; 'X1957'=1957; 'X1958'=1958; 'X1959'=1959; 'X1960'=1960; 'X1961'=1961; 'X1962'=1962; 'X1963'=1963; 'X1964'=1964; 'X1965'=1965; 'X1966'=1966; 'X1967'=1967; 'X1968'=1968; 'X1969'=1969; 'X1970'=1970; 'X1971'=1971; 'X1972'=1972; 'X1973'=1973; 'X1974'=1974; 'X1975'=1975; 'X1976'=1976; 'X1977'=1977; 'X1978'=1978; 'X1979'=1979; 'X1980'=1980; 'X1981'=1981; 'X1982'=1982; 'X1983'=1983; 'X1984'=1984; 'X1985'=1985; 'X1986'=1986; 'X1987'=1987; 'X1988'=1988; 'X1989'=1989; 'X1990'=1990; 'X1991'=1991; 'X1992'=1992; 'X1993'=1993; 'X1994'=1994; 'X1995'=1995; 'X1996'=1996; 'X1997'=1997; 'X1998'=1998; 'X1999'=1999; 'X2000'=2000; 'X2001'=2001; 'X2002'=2002; 'X2003'=2003; 'X2004'=2004; 'X2005'=2005; 'X2006'=2006; 'X2007'=2007; 'X2008'=2008; 'X2009'=2009; 'X2010'=2010; 'X2011'=2011; 'X2012'=2012; 'X2013'=2013; 'X2014'=2014; 'X2015'=2015; ", as.numeric.result=TRUE, as.factor.result = FALSE)
names(ted_YGKw)[names(ted_YGKw)=="value"] <- "YGK"
ted_YGK <- ted_YGKw[,-2]

# Merge the data
tedw <- merge(ted_L,ted_N,by=c("Country", "Year"))
tedw <- merge(tedw,ted_E,by=c("Country", "Year"))
tedw <- merge(tedw,ted_YXR,by=c("Country", "Year"))
tedw <- merge(tedw,ted_YEKS,by=c("Country", "Year"))
tedw <- merge(tedw,ted_YGK,by=c("Country", "Year"))
tedw <- merge(tedw, ted_omega,by=c("Country", "Year"), all.x=T)
str(tedw)
summary(tedw)
tedvars <- names(tedw)
tedvars
summary(tedvars)

# National analysis
require(ggplot2)
table(tedw$Country)

require(plyr)
countries <- count(tedw, "Country")
str(countries)
countries[,1]

ted1to6 <- tedw[ which(tedw$Country=="Albania" | tedw$Country=="Algeria" | tedw$Country=="Angola" | tedw$Country=="Argentina" | tedw$Country=="Armenia" | tedw$Country=="Australia"), ]
ted7to12 <- tedw[ which(tedw$Country=="Austria" | tedw$Country=="Azerbaijan" | tedw$Country=="Bahrain" | tedw$Country=="Bangladesh" | tedw$Country=="Barbados" | tedw$Country=="Belarus"), ]
ted13to18 <- tedw[ which(tedw$Country=="Belgium" | tedw$Country=="Bolivia" | tedw$Country=="Bosnia & Herzegovina" | tedw$Country=="Brazil" | tedw$Country=="Bulgaria" | tedw$Country=="Burkina Faso"), ]
ted19to24 <- tedw[ which(tedw$Country=="C\x99te d'Ivoire" | tedw$Country=="Cambodia" | tedw$Country=="Cameroon" | tedw$Country=="Canada" | tedw$Country=="Chile" | tedw$Country=="China"), ]
ted25to30 <- tedw[ which(tedw$Country=="China - old" | tedw$Country=="Colombia" | tedw$Country=="Costa Rica" | tedw$Country=="Croatia" | tedw$Country=="Cyprus" | tedw$Country=="Czech Republic"), ]
ted31to36 <- tedw[ which(tedw$Country=="Czechoslovakia" | tedw$Country=="Denmark" | tedw$Country=="Dominican Republic" | tedw$Country=="DR Congo" | tedw$Country=="East Germany" | tedw$Country=="Ecuador"), ]
ted37to42 <- tedw[ which(tedw$Country=="Egypt" | tedw$Country=="Estonia" | tedw$Country=="Ethiopia" | tedw$Country=="Finland" | tedw$Country=="France" | tedw$Country=="Georgia"), ]
ted43to48 <- tedw[ which(tedw$Country=="Germany" | tedw$Country=="Ghana" | tedw$Country=="Greece" | tedw$Country=="Guatemala" | tedw$Country=="Hong Kong" | tedw$Country=="Hungary"), ]
ted49to54 <- tedw[ which(tedw$Country=="Iceland" | tedw$Country=="India" | tedw$Country=="Indonesia" | tedw$Country=="Iran" | tedw$Country=="Iraq" | tedw$Country=="Ireland"), ]
ted55to60 <- tedw[ which(tedw$Country=="Israel" | tedw$Country=="Italy" | tedw$Country=="Jamaica" | tedw$Country=="Japan" | tedw$Country=="Jordan" | tedw$Country=="Kazakhstan"), ]
ted61to66 <- tedw[ which(tedw$Country=="Kenya" | tedw$Country=="Kuwait" | tedw$Country=="Kyrgyz Republic" | tedw$Country=="Latvia" | tedw$Country=="Lithuania" | tedw$Country=="Luxembourg"), ]
ted67to72 <- tedw[ which(tedw$Country=="Macedonia" | tedw$Country=="Madagascar" | tedw$Country=="Malawi" | tedw$Country=="Malaysia" | tedw$Country=="Mali" | tedw$Country=="Malta"), ]
ted73to78 <- tedw[ which(tedw$Country=="Mexico" | tedw$Country=="Moldova" | tedw$Country=="Morocco" | tedw$Country=="Mozambique" | tedw$Country=="Myanmar" | tedw$Country=="Netherlands"), ]
ted79to84 <- tedw[ which(tedw$Country=="New Zealand" | tedw$Country=="Niger" | tedw$Country=="Nigeria" | tedw$Country=="Norway" | tedw$Country=="Oman" | tedw$Country=="Pakistan"), ]
ted85to90 <- tedw[ which(tedw$Country=="Peru" | tedw$Country=="Philippines" | tedw$Country=="Poland" | tedw$Country=="Portugal" | tedw$Country=="Qatar" | tedw$Country=="Romania"), ]
ted91to96 <- tedw[ which(tedw$Country=="Russian Federation" | tedw$Country=="Saudi Arabia" | tedw$Country=="Senegal" | tedw$Country=="Serbia & Montenegro" | tedw$Country=="Singapore" | tedw$Country=="Slovak Republic"), ]
ted97to102 <- tedw[ which(tedw$Country=="Slovenia" | tedw$Country=="South Africa" | tedw$Country=="South Korea" | tedw$Country=="Spain" | tedw$Country=="Sri Lanka" | tedw$Country=="St. Lucia"), ]
ted103to108 <- tedw[ which(tedw$Country=="Sudan" | tedw$Country=="Sweden" | tedw$Country=="Switzerland" | tedw$Country=="Syria" | tedw$Country=="Taiwan" | tedw$Country=="Tajikistan"), ]
ted109to114 <- tedw[ which(tedw$Country=="Tanzania" | tedw$Country=="Thailand" | tedw$Country=="Trinidad & Tobago" | tedw$Country=="Tunisia" | tedw$Country=="Turkey" | tedw$Country=="Turkmenistan"), ]
ted115to120 <- tedw[ which(tedw$Country=="Uganda" | tedw$Country=="Ukraine" | tedw$Country=="United Arab Emirates" | tedw$Country=="United Kingdom" | tedw$Country=="United States" | tedw$Country=="Uruguay"), ]
ted121to126 <- tedw[ which(tedw$Country=="USSR" | tedw$Country=="Uzbekistan" | tedw$Country=="Venezuela" | tedw$Country=="Vietnam" | tedw$Country=="West Germany" | tedw$Country=="Yemen"), ]
ted127to129 <- tedw[ which(tedw$Country=="Yugoslavia" | tedw$Country=="Zambia" | tedw$Country=="Zimbabwe"), ]

ggplot(ted1to6, aes(x = Year, y =ted1to6$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted7to12, aes(x = Year, y =ted7to12$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted13to18, aes(x = Year, y =ted13to18$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted19to24, aes(x = Year, y =ted19to24$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted25to30, aes(x = Year, y =ted25to30$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted31to36, aes(x = Year, y =ted31to36$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted37to42, aes(x = Year, y =ted37to42$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted43to48, aes(x = Year, y =ted43to48$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted49to54, aes(x = Year, y =ted49to54$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted55to60, aes(x = Year, y =ted55to60$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted61to66, aes(x = Year, y =ted61to66$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted67to72, aes(x = Year, y =ted67to72$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted73to78, aes(x = Year, y =ted73to78$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted79to84, aes(x = Year, y =ted79to84$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted85to90, aes(x = Year, y =ted85to90$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted91to96, aes(x = Year, y =ted91to96$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted97to102, aes(x = Year, y =ted97to102$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted103to108, aes(x = Year, y =ted103to108$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted109to114, aes(x = Year, y =ted109to114$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted115to120, aes(x = Year, y =ted115to120$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted121to126, aes(x = Year, y =ted121to126$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ggplot(ted127to129, aes(x = Year, y =ted127to129$L)) + theme_bw() + xlim(1950,2015) + theme_classic() + geom_line() + labs(title = "Annual hours of labor") + ylab("L (annual hours of labor)") + facet_wrap(~ Country, ncol=2)
ted127to129$L # NAs

# National plots
plot(tedw$Year[tedw$Country=="United States"], tedw$L[tedw$Country=="United States"], main = "United States: Annual hours of labor", type="l", xlab = "Year", ylab="Annual hours of labor", lwd=1)
points(tedw$Year[tedw$Country=="United States"], tedw$L[tedw$Country=="United States"], pch=16)

plot(tedw$Year[tedw$Country=="China"], tedw$L[tedw$Country=="China"], main = "China: Annual hours of labor", type="l", xlab = "Year", ylab="Annual hours of labor", lwd=1)
points(tedw$Year[tedw$Country=="China"], tedw$L[tedw$Country=="China"], pch=16)

plot(tedw$Year[tedw$Country=="Mexico"], tedw$L[tedw$Country=="Mexico"], main = "Mexico: Annual hours of labor", type="l", xlab = "Year", ylab="Annual hours of labor", lwd=1)
points(tedw$Year[tedw$Country=="Mexico"], tedw$L[tedw$Country=="Mexico"], pch=16)

plot(tedw$Year[tedw$Country=="United States"], tedw$YXR[tedw$Country=="United States"]/1000, main = "Real GDP (XR method)", type="l", xlab = "Year", ylab="2014 USD billions", lwd=2, ylim = c(0,18000))
lines(tedw$Year[tedw$Country=="China"], tedw$YXR[tedw$Country=="China"]/1000, lwd=2, col="blue")
lines(tedw$Year[tedw$Country=="Mexico"], tedw$YXR[tedw$Country=="Mexico"]/1000, lwd=2, col="red")
lines(tedw$Year[tedw$Country=="Brazil"], tedw$YXR[tedw$Country=="Brazil"]/1000, lwd=2, col="pink")
lines(tedw$Year[tedw$Country=="India"], tedw$YXR[tedw$Country=="India"]/1000, lwd=2, col="orange")
lines(tedw$Year[tedw$Country=="Russian Federation"], tedw$YXR[tedw$Country=="Russian Federation"]/1000, lwd=2, col="green")
lines(tedw$Year[tedw$Country=="Japan"], tedw$YXR[tedw$Country=="Japan"]/1000, lwd=2, col="purple")
lines(tedw$Year[tedw$Country=="Germany"], tedw$YXR[tedw$Country=="Germany"]/1000, lwd=2, col="yellow")
# Legend needed

plot(tedw$Year[tedw$Country=="United States"], tedw$YXR[tedw$Country=="United States"]/1000, main = "Real GDP (XR method)", type="l", xlab = "Year", ylab="2014 USD billions", lwd=2, ylim = c(0,18000))
lines(tedw$Year[tedw$Country=="United States"], tedw$YEKS[tedw$Country=="United States"]/1000, lwd=1, lty=2)
lines(tedw$Year[tedw$Country=="United States"], tedw$YGK[tedw$Country=="United States"]/1000, lwd=1, lty=3)

cor(tedw$YXR[tedw$Country=="United States"], tedw$YEKS[tedw$Country=="United States"])

plot(tedw$Year[tedw$Country=="Mexico"], tedw$YXR[tedw$Country=="Mexico"]/1000, main = "Real GDP (XR method)", type="l", xlab = "Year", ylab="2014 USD billions", lwd=2, ylim = c(0,2500))
lines(tedw$Year[tedw$Country=="Mexico"], tedw$YEKS[tedw$Country=="Mexico"]/1000, lwd=1, lty=2)
points(tedw$Year[tedw$Country=="Mexico"], tedw$YGK[tedw$Country=="Mexico"]/1000, pch=16)

plot(tedw$Year[tedw$Country=="Brazil"], tedw$YXR[tedw$Country=="Brazil"]/1000, main = "Real GDP (XR method)", type="l", xlab = "Year", ylab="2014 USD billions", lwd=2, ylim = c(0,3000))
lines(tedw$Year[tedw$Country=="Brazil"], tedw$YEKS[tedw$Country=="Brazil"]/1000, lwd=1, lty=2)
points(tedw$Year[tedw$Country=="Brazil"], tedw$YGK[tedw$Country=="Brazil"]/1000, pch=16)

# As an example of how to carry out certain time series procedures
# Largest countries by Y
tedw.mex <- tedw[ which(tedw$Country=="Mexico"), ]
str(tedw.mex)
tedw.mex <- ts(tedw.mex, start = 1950, end=2015)
plot(tedw.mex[,3:9], main="Mexico")

tedw.usa <- tedw[ which(tedw$Country=="United States"), ]
tedw.usa <- ts(tedw.usa, start = 1950, end=2015)
plot(tedw.usa[,3:9], main="United States")

tedw.chn <- tedw[ which(tedw$Country=="China"), ]
tedw.chn <- ts(tedw.chn, start = 1950, end=2015)
plot(tedw.chn[,3:9], main="China")

tedw.jpn <- tedw[ which(tedw$Country=="Japan"), ]
tedw.jpn <- ts(tedw.jpn, start = 1950, end=2015)
plot(tedw.jpn[,3:9], main="Japan")

tedw.ger <- tedw[ which(tedw$Country=="Germany"), ]
tedw.ger <- ts(tedw.ger, start = 1950, end=2015)
plot(tedw.ger[,3:9], main="Germany")

tedw.rus <- tedw[ which(tedw$Country=="Russian Federation"), ]
tedw.rus <- ts(tedw.rus, start = 1950, end=2015)
plot(tedw.rus[,3:9], main="Russian Federation")

tedw.bra <- tedw[ which(tedw$Country=="Brazil"), ]
tedw.bra <- ts(tedw.bra, start = 1950, end=2015)
plot(tedw.bra[,3:9], main="Brazil")

tedw.ven <- tedw[ which(tedw$Country=="Venezuela"), ]
tedw.ven <- ts(tedw.ven, start = 1950, end=2015)
plot(tedw.ven[,3:9], main="Venezuela")

tedw.ind <- tedw[ which(tedw$Country=="India"), ]
tedw.ind <- ts(tedw.ind, start = 1950, end=2015)
plot(tedw.ind[,3:9], main="India")

tedw.fra <- tedw[ which(tedw$Country=="France"), ]
tedw.fra <- ts(tedw.fra, start = 1950, end=2015)
plot(tedw.fra[,3:9], main="France")

tedw.uk <- tedw[ which(tedw$Country== "United Kingdom"), ]
tedw.uk <- ts(tedw.uk, start = 1950, end=2015)
plot(tedw.uk[,3:9], main="United Kingdom")

tedw.indo <- tedw[ which(tedw$Country== "Indonesia"), ]
tedw.indo <- ts(tedw.indo, start = 1950, end=2015)
plot(tedw.indo[,3:9], main="Indonesia")

tedw.sk <- tedw[ which(tedw$Country== "South Korea"), ]
tedw.sk <- ts(tedw.sk, start = 1950, end=2015)
plot(tedw.sk[,3:9], main="South Korea")

tedw.ita <- tedw[ which(tedw$Country== "Italy"), ]
tedw.ita <- ts(tedw.ita, start = 1950, end=2015)
plot(tedw.ita[,3:9], main="Italy")

# YEKS 8 largest countries by Y
require(xts)

dfL <- data.frame(tedw.usa[,1], data.frame(tedw.usa[,3], tedw.chn[,3], tedw.ind[,3], tedw.jpn[,3], tedw.ger[,3], tedw.rus[,3]))
colnames(dfL) <- c("Year", "United States", "China", "India", "Japan", "Germany", "Russian Federation")
dfL.ts <- ts(dfL[-1], start = 1950, frequency = 1)
plot(dfL.ts, xlab="Year", ylab="", main=names(tedw.usa)[3])
plot(dfL.ts, plot.type="single", col = 1:ncol(dfL.ts), lwd=2, main="L")
legend("topleft", colnames(dfL.ts), col=1:ncol(dfL), lty=1, lwd=2, cex=.95)

dfN <- data.frame(tedw.usa[,1], data.frame(tedw.usa[,4], tedw.chn[,4], tedw.ind[,4], tedw.jpn[,4], tedw.ger[,4], tedw.rus[,4]))
colnames(dfN) <- c("Year", "United States", "China", "India", "Japan", "Germany", "Russian Federation")
dfN.ts <- ts(dfN[-1], start = 1950, frequency = 1)
plot(dfN.ts, xlab="Year", ylab="", main=names(tedw.usa)[4])
plot(dfN.ts, plot.type="single", col = 1:ncol(dfN.ts), lwd=2, main="N")
legend("topleft", colnames(dfN.ts), col=1:ncol(dfN), lty=1, lwd=2, cex=.95)

dfE <- data.frame(tedw.usa[,1], data.frame(tedw.usa[,5], tedw.chn[,5], tedw.ind[,5], tedw.jpn[,5], tedw.ger[,5], tedw.rus[,5]))
colnames(dfE) <- c("Year", "United States", "China", "India", "Japan", "Germany", "Russian Federation")
dfE.ts <- ts(dfE[-1], start = 1950, frequency = 1)
plot(dfE.ts, xlab="Year", ylab="", main=names(tedw.usa)[3])
plot(dfE.ts, plot.type="single", col = 1:ncol(dfE.ts), lwd=2, main="E")
legend("topleft", colnames(dfE.ts), col=1:ncol(dfE), lty=1, lwd=2, cex=.95)

dfYXR <- data.frame(tedw.usa[,1], data.frame(tedw.usa[,6], tedw.chn[,6], tedw.ind[,6], tedw.jpn[,6], tedw.ger[,6], tedw.rus[,6]))
colnames(dfYXR) <- c("Year", "United States", "China", "India", "Japan", "Germany", "Russian Federation")
dfYXR.ts <- ts(dfYXR[-1], start = 1950, frequency = 1)
plot(dfYXR.ts, xlab="Year", ylab="", main=names(tedw.usa)[3])
plot(dfYXR.ts, plot.type="single", col = 1:ncol(dfYXR.ts), lwd=2, main="Y XR")
legend("topleft", colnames(dfYXR.ts), col=1:ncol(dfYXR), lty=1, lwd=2, cex=.95)

dfYEKS <- data.frame(tedw.usa[,1], data.frame(tedw.usa[,7], tedw.chn[,7], tedw.ind[,7], tedw.jpn[,7], tedw.ger[,7], tedw.rus[,7]))
colnames(dfYEKS) <- c("Year", "United States", "China", "India", "Japan", "Germany", "Russian Federation")
dfYEKS.ts <- ts(dfYEKS[-1], start = 1950, frequency = 1)
plot(dfYEKS.ts, xlab="Year", ylab="", main=names(tedw.usa)[3])
plot(dfYEKS.ts, plot.type="single", col = 1:ncol(dfYEKS.ts), lwd=2, main="Y EKS")
legend("topleft", colnames(dfYEKS.ts), col=1:ncol(dfYEKS), lty=1, lwd=2, cex=.95)

dfYGK <- data.frame(tedw.usa[,1], data.frame(tedw.usa[,8], tedw.chn[,8], tedw.ind[,8], tedw.jpn[,8], tedw.ger[,8], tedw.rus[,8]))
colnames(dfYGK) <- c("Year", "United States", "China", "India", "Japan", "Germany", "Russian Federation")
dfYGK.ts <- ts(dfYGK[-1], start = 1950, frequency = 1)
plot(dfYGK.ts, xlab="Year", ylab="", main=names(tedw.usa)[3])
plot(dfYGK.ts, plot.type="single", col = 1:ncol(dfYGK.ts), lwd=2, main="Y GK")
legend("topleft", colnames(dfYGK.ts), col=1:ncol(dfYGK), lty=1, lwd=2, cex=.95)

dfomega <- data.frame(tedw.usa[,1], data.frame(tedw.usa[,9], tedw.chn[,9], tedw.ind[,9], tedw.jpn[,9], tedw.ger[,9], tedw.rus[,9]))
colnames(dfomega) <- c("Year", "United States", "China", "India", "Japan", "Germany", "Russian Federation")
dfomega.ts <- ts(dfomega[-1], start = 1950, frequency = 1)
plot(dfomega.ts, xlab="Year", ylab="", main=names(tedw.usa)[3])
plot(dfomega.ts, plot.type="single", col = 1:ncol(dfomega.ts), lwd=2, main="Wage share")
legend("topleft", colnames(dfomega.ts), col=1:ncol(dfomega), lty=1, lwd=2, cex=.95)


# Regional analysis


# Aggregation of demo vars
# Aggregation international by country and pop of economic vars
# Aggregation global (by $)
