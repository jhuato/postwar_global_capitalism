library(ggplot2)
mydf <- data.frame(id = sample(1:4, 50, replace = TRUE), var1 = runif(50), var2 = rnorm(50))
ggplot(mydf, aes(x = var1, y = var2)) +
  theme_bw() +
  geom_point() + 
  facet_wrap(~ id)
View(mydf)
