########################
# CoVar Matrix Testing #
########################

aMat <- c(1,2,3,4,1,
          3,5,6,5,8,
          3,0,7,8,9)
A <- matrix(aMat, nrow = 3, ncol = 5, byrow = TRUE)
A
cov(A)

bMat <- c(1,2,3,4)
B <- matrix(bMat, nrow = 2, ncol = 2, byrow = TRUE)
B
cov(B)

cMat <- c(1,23,45,6,3,1)
C <- matrix(cMat, nrow = 2, ncol = 3, byrow = TRUE)
C
cov(C)

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

data
cov(data)
chunk1
cov(chunk1)
chunk2
cov(chunk2)
chunk3
cov(chunk3)
chunk4
cov(chunk4)
