library("vars")
data("Canada")
summary(Canada)
str(Canada)
plot(Canada, nc = 2, xlab = "")
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1
Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct <- VAR(Canada, p = 1, type = "both")
p1ct
summary(p1ct, equation = "e")
plot(p1ct, names = "e")
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial
norm1 <- normality.test(p1ct)
norm1$jb.mul
arch1 <- arch.test(p1ct, lags.multi = 5)
arch1$arch.mul
plot(arch1, names = "e")
plot(stability(p1ct), nc = 2)
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3, spec = "transitory"))
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2, spec = "transitory"))
vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE, runs = 100)
summary(svec)
LR[3, 3] <- 0
svec.oi <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.oi$LRover
svec.irf <- irf(svec, response = "U", n.ahead = 48, boot = TRUE)
plot(svec.irf)
fevd.U <- fevd(svec, n.ahead = 48)$U
