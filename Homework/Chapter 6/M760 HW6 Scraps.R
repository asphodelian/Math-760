library(car)
library(DescTools)
library(ellipse)
library(GGally)
library(ggplot2)
library(graphics)
library(gridExtra)
library(investr)
library(matlib)
library(MVN)
library(robustbase)
library(SIBER)

########
# 6.25 #
########

crude <- oil[,-6]
n <- dim(crude)[1]
p <- dim(crude)[2]
# xbar
one <- as.matrix(rep(1, n))
xbar <- 1/n * t(crude) %*% one
# S
mean_matrix <- matrix(data = 1, nrow = n) %*% cbind(xbar[[1]], xbar[[2]], xbar[[3]], xbar[[4]], xbar[[5]])
xstar <- crude - mean_matrix
S <- 1/(n-1) * t(xstar) %*% as.matrix(xstar)
# vars
crit <- qf(0.05, df1 = p, df2 = n-p, lower.tail = FALSE)
frac <- ((n-1)*p)/(n-p)
right <- sqrt(frac*crit1)
# X1
bneg1 <- xbar[1] - crit*sqrt((S[1,1]/n))
bpos1 <- xbar[1] + crit*sqrt((S[1,1]/n))
cat("X1: (", bneg1, ",", bpos1, ") \n")
# X2
bneg2 <- xbar[2] - crit*sqrt((S[2,2]/n))
bpos2 <- xbar[2] + crit*sqrt((S[2,2]/n))
cat("X2: (", bneg2, ",", bpos2, ") \n")
# X3
bneg3 <- xbar[3] - crit*sqrt((S[3,3]/n))
bpos3 <- xbar[3] + crit*sqrt((S[3,3]/n))
cat("X3: (", bneg3, ",", bpos3, ") \n")
# X4
bneg4 <- xbar[4] - crit*sqrt((S[4,4]/n))
bpos4 <- xbar[4] + crit*sqrt((S[4,4]/n))
cat("X4: (", bneg4, ",", bpos4, ") \n")
# X5
bneg5 <- xbar[5] - crit*sqrt((S[5,5]/n))
bpos5 <- xbar[5] + crit*sqrt((S[5,5]/n))
cat("X5: (", bneg5, ",", bpos5, ") \n")

################
# Birds (b) V2 #
################

# vars
nm1 <- dim(male1)[1]
nm2 <- dim(male2)[1]
nf <- dim(female)[1]
pm1 <- dim(male1)[2]
pm2 <- dim(male2)[2]
pf <- dim(female)[2]
# crit
crit1 <- qf(0.05, df1 = (pm1+pf)-1, df2 = (nm1+nf)-(pm1+pf)+1, lower.tail = FALSE)
crit2 <- qf(0.05, df1 = (pm2+pf)-1, df2 = (nm2+nf)-(pm1+pf)+1, lower.tail = FALSE)
frac1 <- (((nm1+nf)-1)*((pm1+pf)-1))/((nm1+nf)-(pm1+pf)+1)
frac2 <- (((nm2+nf)-1)*((pm2+pf)-1))/((nm2+nf)-(pm2+pf)+1)
right1 <- frac1*crit1
right2 <- frac2*crit2
# xbar
onem1 <- as.matrix(rep(1, nm1))
onem2 <- as.matrix(rep(1, nm2))
onef <- as.matrix(rep(1, nf))
xbarm1 <- 1/nm1*t(male1)%*%onem1
xbarm2 <- 1/nm2*t(male2)%*%onem2
xbarf <- 1/nf*t(female)%*%onef
xbar1 <- xbarm1 - xbarf
xbar2 <- xbarm2 - xbarf
# S
mean_matrixm1 <- matrix(data = 1, nrow = nm1)%*%cbind(xbarm1[[1]], xbarm1[[2]])
mean_matrixm2 <- matrix(data = 1, nrow = nm2)%*%cbind(xbarm2[[1]], xbarm2[[2]])
mean_matrixf <- matrix(data = 1, nrow = nf)%*%cbind(xbarf[[1]], xbarf[[2]])
xstarm1 <- male1 - mean_matrixm1
xstarm2 <- male2 - mean_matrixm2
xstarf <- female - mean_matrixf
Sm1 <- 1/(nm1-1) * t(xstarm1) %*% as.matrix(xstarm1)
Sm2 <- 1/(nm2-1) * t(xstarm2) %*% as.matrix(xstarm2)
Sf <- 1/(nf-1) * t(xstarf) %*% as.matrix(xstarf)
S1 <- Sm1 - Sf
S2 <- Sm2 - Sf
# C matrix
Cmat <- c(1,-1)
C <- matrix(Cmat, nrow = 1, ncol = 2, byrow = TRUE)
# T2
T21 <- (nm1+nf) %*% t(C %*% xbar1) %*% solve(C %*% S1 %*% t(C)) %*% (C %*% xbar1)
T22 <- (nm2+nf) %*% t(C %*% xbar2) %*% solve(C %*% S2 %*% t(C)) %*% (C %*% xbar2)

################
# Birds (b) V1 #
################

male <- read.table("D:/Coding/R Storage/T6-11.dat", header = FALSE, sep = "")
female <- read.table("D:/Coding/R Storage/T5-12.dat", header = FALSE, sep = "")
male1 <- male
male2 <- male
male1[31,] <- 184
male2 <- male2[-31,]
# cols
m11 <- male1$V1 # tail length
m12 <- male1$V2 # wing length
m21 <- male2$V1 # tail length
m22 <- male2$V2 # wing length
f1 <- female$V1 # tail length
f2 <- female$V2 # wing length
# vars
nm1 <- dim(male1)[1]
nm2 <- dim(male2)[1]
nf <- dim(female)[1]
p <-2
# right
crit1 <- qf(0.05, df1 = p-1, df2 = (nm1+nf)-p+1, lower.tail = FALSE)
crit2 <- qf(0.05, df1 = p-1, df2 = (nm2+nf)-p+1, lower.tail = FALSE)
frac1 <- (((nm1+nf)-1)*(p-1))/((nm1+nf)-q+1)
frac2 <- (((nm2+nf)-1)*(p-1))/((nm2+nf)-p+1)
right1 <- frac1*crit1
right2 <- frac2*crit2
cat("The critical value for male1/female is", right1)
cat("The critical value for male2/female is", right2)

# xbar
onem1 <- as.matrix(rep(1, nm1))
xbarm1 <- 1/nm1*t(male1)%*%onem1
onem2 <- as.matrix(rep(1, nm2))
xbarm2 <- 1/nm2*t(male2)%*%onem2
onef <- as.matrix(rep(1, nf))
xbarf <- 1/nf*t(female)%*%onef
# S
mean_matrixm1 <- matrix(data = 1, nrow = nm1)%*%cbind(xbarm1[[1]], xbarm1[[2]])
xstarm1 <- male1 - mean_matrixm1
Sm1 <- 1/(nm1-1) * t(xstarm1) %*% as.matrix(xstarm1)
mean_matrixm2 <- matrix(data = 1, nrow = nm2)%*%cbind(xbarm2[[1]], xbarm2[[2]])
xstarm2 <- male2 - mean_matrixm2
Sm2 <- 1/(nm2-1) * t(xstarm2) %*% as.matrix(xstarm2)
mean_matrixf <- matrix(data = 1, nrow = nf)%*%cbind(xbarf[[1]], xbarf[[2]])
xstarf <- female - mean_matrixf
Sf <- 1/(nf-1) * t(xstarf) %*% as.matrix(xstarf)

Cmat <- c()
C <- matrix(C, nrow = , ncol = , byrow = TRUE)
Cx <- C %*% xbar
CSC <- C %*% S %*% t(C)
T2 <- n %*% t(Cx) %*% inv(CSC) %*% Cx
T2

#########
# Bones #
#########

bone <- read.table("D:/Coding/R Storage/T1-8.dat", header = FALSE, sep = "")
# vars
x1 <- bone$V1 # dom radius
x2 <- bone$V2 # radius
x3 <- bone$V3 # dom humerus
x4 <- bone$V4 # humerus
x5 <- bone$V5 # dom ulna
x6 <- bone$V6 # ulna
# subset
dom <- bone %>%
  select(V1, V3, V5)
weak <- bone %>%
  select(V2, V4, V6)
dom <- as.matrix(dom)
weak <- as.matrix(weak)
# new data
radius <- x1-x2
humerus <- x3-x4
ulna <- x5-x6
distance <- data.frame(radius, humerus, ulna)
# dim
n1 <- dim(dom)[1]
n2 <- dim(weak)[1]
n <- n1 + n2
p1 <- dim(dom)[2]
p2 <- dim(weak)[2]
p <- p1 + p2
# xbar
one1 <- as.matrix(rep(1, n1))
one2 <- as.matrix(rep(1, n2))
xbar1 <- 1/n1 * t(dom) %*% one1
xbar2 <- 1/n2 * t(weak) %*% one2
xbar <- xbar1 - xbar2
# S
mean_matrix1 <- matrix(data = 1, nrow = n1) %*% cbind(xbar1[[1]], xbar1[[2]], xbar1[[3]])
mean_matrix2 <- matrix(data = 1, nrow = n2) %*% cbind(xbar2[[1]], xbar2[[2]], xbar2[[3]])
xstar1 <- dom - mean_matrix1
xstar2 <- weak - mean_matrix2
S1 <- 1/(n1-1) * t(xstar1) %*% xstar1
S2 <- 1/(n2-1) * t(xstar2) %*% xstar2
S <- S1 - S2

### (a) Test using $\alpha = 0.05$
HotellingsT2Test(x = dom, y = weak)

### (b) Construct 95% simultaneous confidence intervals for the mean differences.
# vars
crit1 <- qf(0.05, df1 = p, df2 = n-p, lower.tail = FALSE)
frac <- ((n-1)*p)/(n-p)
right <- sqrt(frac*crit)
# Radius
bneg1 <- xbar[1] - crit1*sqrt((S[1,1]^2/n))
bpos1 <- xbar[1] + crit1*sqrt((S[1,1]^2/n))
cat("Radius: (", bneg1, ",", bpos1, ") \n")
# Humerus
bneg2 <- xbar[2] - crit1*sqrt((S[2,2]^2/n))
bpos2 <- xbar[2] + crit1*sqrt((S[2,2]^2/n))
cat("Humerus Height: (", bneg2, ",", bpos2, ") \n")
# Ulna
bneg3 <- xbar[3] - crit1*sqrt((S[3,3]^2/n))
bpos3 <- xbar[3] + crit1*sqrt((S[3,3]^2/n))
cat("Ulna: (", bneg3, ",", bpos3, ") \n")

### (c) Contruct the Bonferroni 95% simultaneous intervals, and compare these with the intervals in (b).
crit2 <- -qt(0.05/(2*p), df = n-1)
# Radius
bneg1 <- xbar[1] - crit2*sqrt((S[1,1]/n))
bpos1 <- xbar[1] + crit2*sqrt((S[1,1]/n))
cat("Radius: (", bneg1, ",", bpos1, ") \n")
# Humerus
bneg2 <- xbar[2] - crit2*sqrt((S[2,2]/n))
bpos2 <- xbar[2] + crit2*sqrt((S[2,2]/n))
cat("Humerus Height: (", bneg2, ",", bpos2, ") \n")
# Ulna
bneg3 <- xbar[3] - crit2*sqrt((S[3,3]/n))
bpos3 <- xbar[3] + crit2*sqrt((S[3,3]/n))
cat("Ulna: (", bneg3, ",", bpos3, ") \n")

############
# Birds V2 #
############

n1 <- dim(male1)[1]
n <- n1 + n2
#male1 <- cbind(male1, Gender = 1)
#female <- cbind(female, Gender = 0)
bird <- smartbind(male1,female)
# vars
crit <- qf(0.05 , df1 = p, df2 = n-p-1, lower.tail = FALSE)
frac <- (1/n1+1/n2)*(n-2)*p/(n-p-1)
r <- sqrt(frac*crit)
# xbar
one1 <- as.matrix(rep(1, n1))
one2 <- as.matrix(rep(1, n2))
xbar1 <- 1/n * t(male1) %*% one1
xbar2 <- 1/n * t(female) %*% one2
xbar <- xbar1 - xbar2
# S
mean_matrix1 <- matrix(data = 1, nrow = n1) %*% cbind(xbar[[1]], xbar[[2]])
xstar1 <- male1 - mean_matrix1
S1 <- 1/(n1-1) * t(xstar1) %*% as.matrix(xstar1)
mean_matrix2 <- matrix(data = 1, nrow = n1) %*% cbind(xbar[[1]], xbar[[2]])
xstar2 <- female - mean_matrix2
S2 <- 1/(n2-1) * t(xstar2) %*% as.matrix(xstar2)
Sp <- ((n1-1)*S1+(n2-1)*S2)/(n-2)
# interval
LCIx <- xbar[1] - r*sqrt(Sp[1,1]/n)
UCIx <- xbar[1] + r*sqrt(Sp[1,1]/n)
LCIy <- xbar[2] - r*sqrt(Sp[2,2]/n)
UCIy <- xbar[2] + r*sqrt(Sp[2,2]/n)
# print
cat("The 95% simultaneous confidence interval of mu1: (", LCIx, ",", UCIx, ") \n")
cat("The 95% simultaneous confidence interval of mu2: (", LCIy, ",", UCIy, ")")

# ellipse
npoints <- 1000
pi <- 3.141593
alpha <- 0.05
p <- 2
theta <- seq(0, 2*pi, length = npoints)

v <- rbind(r*cos(theta), r*sin(theta))
z <- backsolve(chol(solve(Sp)),v)+center
plot(t(z),type="l",xlab=expression(mu[11]-mu[21]),ylab=expression(mu[21]-mu[22]))
points(center[1], center[2],pch=19)

############
# Birds V1 #
############

male <- read.table("D:/Coding/R Storage/T6-11.dat", header = FALSE, sep = "")
female <- read.table("D:/Coding/R Storage/T5-12.dat", header = FALSE, sep = "")
# vars
m1 <- male$V1 # tail length
m2 <- male$V2 # wing length
f1 <- female$V1 # tail length
f2 <- female$V2 # wing length
plot(m2,m1)
identify(m2,m1)

# vars
n1 <- dim(male)[1]
n2 <- dim(female)[1]
q1 <- dim(male)[2]
q2 <- dim(female)[2]

# xbar
one1 <- as.matrix(rep(1, n1))
one2 <- as.matrix(rep(1, n2))
xbar1 <- 1/n1 * t(male) %*% one1
xbar2 <- 1/n2 * t(female) %*% one2
xbar <- xbar1 - xbar2
# S
mean_matrix1 <- matrix(data = 1, nrow = n1) %*% cbind(xbar1[[1]], xbar1[[2]])
mean_matrix2 <- matrix(data = 1, nrow = n2) %*% cbind(xbar2[[1]], xbar2[[2]])
xstar1 <- male - mean_matrix1
xstar2 <- female - mean_matrix2
S1 <- 1/(n1-1) * t(xstar1) %*% as.matrix(xstar1)
S2 <- 1/(n2-1) * t(xstar2) %*% as.matrix(xstar2)
S <- S1 - S2
# crit
crit1 <- qf(0.05, df1 = (q1+q2) - 1, df2 = (n1+n2) - (q1+q2) +1, lower.tail = FALSE)
frac <- (((n1+n2)-1)*((q1+q2)-1))/((n1+n2) - (q1+q2)+1)
right <- frac*crit
crit2 <- sqrt(right)
# C
Cmat <- c(1,-1,-1,1)
C <- matrix(Cmat, nrow = 2, ncol = 2, byrow = TRUE)
Cx <- C %*% xbar
CSC <- C %*% S %*% t(C)
#T2 <- (n1+n2) * Cx %*% inv(CSC) %*% t(Cx)
#cat("The T-squared value is ", T2, "\n")
# interval
m12 <- xbar[1] - xbar[2]
S1 <- (CSC/n)
# intervals
neg12 <- m12 - crit1*sqrt(S1[1,1]) 
pos12 <- m12 + crit1*sqrt(S1[1,1])
#neg <- m12 - crit1*sqrt(S1[1,1]) 
#pos <- m12 + crit1*sqrt(S1[1,1])

# print
cat("95% simultaneous confidence interval of mu1 - mu2: (", neg12, ",", pos12, ")")
cat("95% simultaneous confidence interval of mu1 - mu2: (", neg2, ",", pos2, ")")

