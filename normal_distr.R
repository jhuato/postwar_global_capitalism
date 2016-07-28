# JULIO HUATO
# SFC

# Study the normal distribution in the slides.

# The density function of a normally distributed random variable X (standard normal):
curve(exp(-(1/2)*(x^2))/sqrt(2*pi), from=-4, to=4, type="l", main="Normal density", ylab="", lwd=2)

# Or, using the formula built in R (called "dnorm", default=standard normal, Z ~ N(0,1)):
curve(dnorm, from=-4, to=4, type="l", main="Normal density", ylab="", col="red", lwd=2)

# Here is the density for X ~ N(50, 100).  Note the scales of X and Y.
curve(dnorm(x, 50, 100), from=-350, to=450, type="l", main="Normal density", ylab="", col="blue", lwd=2)

# Note that the density function (plotted on the vertical axis) for X = x is not
# the probability that X = x.  For example, the normal density when Z = 0 is approx. 0.4:
dnorm(0, mean = 0, sd = 1, log = FALSE)

# The normal density when X = 50 when X ~ N(50, 10,000):
dnorm(50, mean = 50, sd = 100, log = FALSE)
# This is because the area under the density curve must equal to 1 
# (from minus infinity to plus infinity).

# The Pr(X=x) = 0, because this is a continuous r.v. And Pr (- \infty < x < + \infty) = 1.

# Instead, the probability is only defined for intervals of X.  For example, 
# P(Z < 1.5):
pnorm(1.5, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
curve(dnorm, from=-4, to=4, type="l", main="Normal density", ylab="", col="red", lwd=2)
cord.x <- c(-4,seq(-4,1.5,0.01),1.5) 
cord.y <- c(0,dnorm(seq(-4,1.5,0.01)),0) 
polygon(cord.x,cord.y,col="pink", border=NULL)

# Here's P(X < 250) if X ~ N(50, 10,000):
pnorm(200, mean = 50, sd = 100, lower.tail = TRUE, log.p = FALSE)
curve(dnorm(x,50,100), from=-350, to=450, type="l", main="Normal density", ylab="", col="blue", lwd=2)
cord.x <- c(-350,seq(-350,200,0.01),200) 
cord.y <- c(0,dnorm(mean=50, sd=100, seq(-350,200,0.01)),-350) 
polygon(cord.x,cord.y,col="lightblue", border=NULL)

# R allows to determine critical values of X.  For example,
# suppose you need Z = z so that P(X < x) = 0.025 (2.5% on the left tail):
qnorm(0.025, mean=0, sd=1)

# If you need X = x so that P(X < x) = 0.025 (2.5% on the left tail), when X ~ N(50, 10,000):
qnorm(0.025, mean=50, sd=100)

# This defines a sample vector w (n=100) randomly drawn from X ~ N(50, 10,000):
w <- as.data.frame(rnorm(100, mean = 50, sd = 100))
w[,1]
# This plots w against the observation index (1:100):
plot(w, pch=16, col="red")

# These plot the histogram and density of w:
hist(w)
plot(density(w))
