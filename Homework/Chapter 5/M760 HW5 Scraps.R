# We will using $\mu_{2-3}$, the mean increase in length from 2 to 3 years, 
# and $\mu_{4-5}$, the mean increase in length from 4 to 5 years, for this 
# confidence region. 

#The formula for sample mean-centered ellipse is:
# $n[\bar{x_i} - \mu_i, \bar{x_k} - \mu_k] \begin{bmatrix} s_{ii} & s_{ik}\\ 
# s_{ik} & s_{kk} \end{bmatrix}^{-1} \begin{bmatrix} \bar{x_i} - \mu_i \\ 
# \bar{x_k} - \mu_k \end{bmatrix} \leq \frac{p(n-1)}{n-p} F_{p,n-p}(\alpha)$

college <- read.table("D:/Coding/R Storage/T5-2.dat", header = FALSE)
# vars
x1 <- college$V1 #social science & history
x2 <- college$V2 #verbal
x3 <- college$V3 #science
par(mfrow = c(1,2))
# x1
qqnorm(x1, main = "Social Science & History")
qqline(x1, col = "orangered2", lwd = 2)
qqPlot(x1, main = "Social Science & History")
# x2
qqnorm(x2, main = "Verbal")
qqline(x2, col = "darkviolet", lwd = 2)
qqPlot(x2, main = "Verbal")
# x3
qqnorm(x3, main = "Science")
qqline(x3, col = "springgreen4", lwd = 2)
qqPlot(x3, main = "Science")

library(MVN)
skull <- read.table("D:/Coding/R Storage/T6-13.dat", header = FALSE)
skullDim <- skull[1:30,1:4]
# vars
x1 <- skullDim$V1 #max breadth
x2 <- skullDim$V2 #basibregmatic height
x3 <- skullDim$V3 #basialveolar length
x4 <- skullDim$V4 #nasal height
mvn(skull, multivariatePlot = "qq")
par(mfrow = c(1,2))
# x1
qqnorm(x1, main = "Max Breath")
qqline(x1, col = "orangered2", lwd = 2)
qqPlot(x1, main = "Max Breath")
# x2
qqnorm(x2, main = "Basib Height")
qqline(x2, col = "darkviolet", lwd = 2)
qqPlot(x2, main = "Basib Height")
# x3
qqnorm(x3, main = "Basial Length")
qqline(x3, col = "springgreen4", lwd = 2)
qqPlot(x3, main = "Basial Length")
# x4
qqnorm(x4, main = "Nasal Height")
qqline(x4, col = "deepskyblue2", lwd = 2)
qqPlot(x4, main = "Nasal Height")


######################
# University of Oslo #
######################
xbar <- c(95.52, 164.38, 55.69, 93.39, 17.98, 31.13)
S <- matrix(c(3266.46, 1343.97, 731.54, 1175.50, 162.68, 238.37,
              1343.97, 721.91, 324.25, 537.35, 80.17, 117.73,
              731.54, 324.25, 179.28, 281.17, 39.15, 56.80,
              1175.50, 537.35, 281.17, 474.98, 63.73, 94.85,
              162.68, 80.17, 39.15, 63.73, 9.95, 13.88,
              238.37, 117.73, 56.80, 94.85, 13.88, 21.26),
              nrow=6, ncol=6, byrow=TRUE)
p <- 6
n <- 61
alpha <- 0.05
# (a) Scheff’s
SH.qlevel <- qf(1-alpha,df1=p,df2=n-p)
for ( i in 1:p ){
  SH.LCI <- xbar[i]-sqrt((n-1)*p/(n-p)*SH.qlevel)*sqrt(S[i,i]/n)
  SH.UCI <- xbar[i]+sqrt((n-1)*p/(n-p)*SH.qlevel)*sqrt(S[i,i]/n)
  print(c(SH.LCI, SH.UCI))
  }
# (a) Large sample
qlevel <- qchisq(1-alpha,df=p)
for ( i in 1:p ){
  LCI <- xbar[i]-sqrt(qlevel)*sqrt(S[i,i]/n)
  UCI <- xbar[i]+sqrt(qlevel)*sqrt(S[i,i]/n)
  print(c(LCI, UCI))
  }
# (b)
center <- xbar[c(1,4)]
Sn2 <- S[c(1,4),c(1,4)]
npoints <- 1000
theta <- seq(0, 2*pi, length = npoints)
# transform for points on ellipse for Scheff’s
SH.r <- sqrt((n-1)*p/(n-p)*SH.qlevel/n)
SH.v <- rbind(SH.r*cos(theta), SH.r*sin(theta))
SH.z <- backsolve(chol(solve(Sn2)),SH.v)+center
# calculate the 95% simultaneous confidence interval
SH.LCIx <- xbar[1]-sqrt((n-1)*p/(n-p)*SH.qlevel)*sqrt(S[1,1]/n)
SH.UCIx <- xbar[1]+sqrt((n-1)*p/(n-p)*SH.qlevel)*sqrt(S[1,1]/n)
SH.LCIy <- xbar[4]-sqrt((n-1)*p/(n-p)*SH.qlevel)*sqrt(S[4,4]/n)
SH.UCIy <- xbar[4]+sqrt((n-1)*p/(n-p)*SH.qlevel)*sqrt(S[4,4]/n)
# transform for points on ellipse for large sample
r <- sqrt(qlevel/n)
v <- rbind(r*cos(theta), r*sin(theta))
z <- backsolve(chol(solve(Sn2)),v)+center
# calculate the 95% simultaneous confidence interval
LCIx <- xbar[1]-sqrt(qlevel)*sqrt(S[1,1]/n)
UCIx <- xbar[1]+sqrt(qlevel)*sqrt(S[1,1]/n)
LCIy <- xbar[4]-sqrt(qlevel)*sqrt(S[4,4]/n)
UCIy <- xbar[4]+sqrt(qlevel)*sqrt(S[4,4]/n)
# plot the ellipse for Scheff’s
plot(t(SH.z), type = "l", xlab= "Weight", ylab= "Girth", lty = 2, col= "blue")
# plot 95% simultaneous confidence interval
abline(v=SH.LCIx, col="blue")
abline(v=SH.UCIx, col="blue")
abline(h=SH.LCIy, col="blue")
abline(h=SH.UCIy, col="blue")
# plot the ellipse for large sample
lines(t(z))
# plot 95% simultaneous confidence interval
abline(v=LCIx)
abline(v=UCIx)
abline(h=LCIy)
abline(h=UCIy)
# plot center of ellipse
points(center[1], center[2], col="red")
# (c)
BF.qlevel <- qt(1-alpha/(2*p),df=n-1)
for ( i in 1:p ){
  BF.LCI <- xbar[i]-BF.qlevel*sqrt(S[i,i]/n)
  BF.UCI <- xbar[i]+BF.qlevel*sqrt(S[i,i]/n)
  print(c(BF.LCI,BF.UCI))
  }
# (d)
# calculate the 95% simultaneous confidence interval for Bonferroni method
BF.LCIx <- xbar[1]-BF.qlevel*sqrt(S[1,1]/n)
BF.UCIx <- xbar[1]+BF.qlevel*sqrt(S[1,1]/n)
BF.LCIy <- xbar[4]-BF.qlevel*sqrt(S[4,4]/n)
BF.UCIy <- xbar[4]+BF.qlevel*sqrt(S[4,4]/n)
plot(t(SH.z), type = "l", xlab= "Weight", ylab= "Girth", lty=2, col= "blue")
lines(t(z))
points(center[1], center[2], col= "red")
# plot 95% simultaneous confidence interval
abline(v=BF.LCIx, lty=3, col="red")
abline(v=BF.UCIx, lty=3, col="red")
abline(h=BF.LCIy, lty=3, col="red")
abline(h=BF.UCIy, lty=3, col="red")




