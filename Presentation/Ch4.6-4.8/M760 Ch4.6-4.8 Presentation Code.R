library(ggplot2)
library(ggpubr)
library(mvtnorm)

# contour
x.points <- seq(-3,3, length.out = 100)
y.points <- x.points
z <- matrix(0, nrow = 100, ncol = 100)
mu <- c(1,1)
sigma <- matrix(c(2,1,1,1), nrow = 2)

for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i], y.points[j]), mean = mu, sigma = sigma)
  }
}

contour(x.points, y.points, z)

# dot plot fail

x1 <- c(1,2,5,6,7,8,10,12,
        13,14,14,15,15,15,
        16,16,17,17,17,18,
        20,21,22,22,23,24,
        25,27,28,29,40)

x2 <- c(1,2,5,6,7,8,10,12,
        13,14,14,15,15,15,
        16,16,17,17,17,18,
        20,21,22,22,23,24,
        25,27,28,29,40)

df <- data.frame(x1,x2)

xplot <- ggplot(df, aes(x = x1)) +
  geom_dotplot(dotsize = 0.8)+theme(axis.text.y=element_blank(),
                                    panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank(),
                                    axis.line.x = element_line(colour = "black"),
                                    axis.title=element_blank(),
                                    axis.ticks.y=element_blank())
xplot

# dot plot

x1 <- c(1,2,5,6,7,8,10,12,
        13,14,14,15,15,15,
        16,16,17,17,17,18,
        20,21,22,22,23,24,
        25,27,28,29,40)

x2 <- c(1,2,5,6,7,8,10,12,
        13,14,14,15,15,15,
        16,16,17,17,17,18,
        20,21,22,22,23,24,
        25,27,28,29,40)

df <- data.frame(x1,x2)

sample <- ggplot(df, aes(x1)) +
  geom_dotplot(binwidth = 1)
sample
