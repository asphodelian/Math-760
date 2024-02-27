########################
# CoVar Matrix Testing #
########################

aMat <- c(1,2,3,4,1,
          3,5,6,5,8,
          3,0,7,8,9)
A <- matrix(aMat, nrow = 3, ncol = 5, byrow = TRUE)
bMat <- c(4,9,5,4,5,
          6,3,4,5,8,
          6,3,4,5,6)
B <- matrix(bMat, nrow = 3, ncol = 5, byrow = TRUE)
A
B

AB <- A %*% t(B)
AB
ABvar <- cov(AB)
ABvar

cMat <- c(48,64,56,
          48,63,43,
          45,40,56,
          45,60,45,
          64,56,24,
          28,56,45,
          45,64,44)
C <- matrix(cMat, nrow = 7, ncol = 3, byrow = TRUE)
dMat <- c(45,32,4,
          45,5,44,
          52,3,52,
          47,4,53,
          74,0,24,
          56,7,1,
          48,4,56)
D <- matrix(dMat, nrow = 7, ncol = 3, byrow = TRUE)
C
D

CD <- C %*% t(D)
CD
CDvar <- cov(CD)
CDvar

#######################
# From Stack Exchange #
#######################

r = 8
c = 10
number_of_chunks = 4
data = matrix(seq(r*c), nrow = r, ncol=c)

chunk <- lapply(split(seq_len(nrow(data)), cut(seq_len(nrow(data)), 
                                               pretty(seq_len(nrow(data)), 
                                                      number_of_chunks))), 
                function(x) data[x, ])
chunk1 <- chunk$`(0,2]`
chunk2 <- chunk$`(2,4]`
chunk3 <- chunk$`(4,6]`
chunk4 <- chunk$`(6,8]`

s12 <- chunk1 %*% t(chunk2)
s12
var12 <- cov(s12)
var12

s12 <- chunk1 %*% t(chunk2)
s12
var12 <- cov(s12)
var12

s13 <- chunk1 %*% t(chunk3)
s13
var13 <- cov(s13)
var13

s14 <- chunk1 %*% t(chunk4)
s14
var14 <- cov(s14)
var14

############################
# Really High Order Matrix #
############################

thousand <- matrix(1:1000, nrow = 100, ncol = 100)
thousand 
View(thousand)
