setwd("D:/Batch37/CSE7302c/Day04")

#load("PCA-data.Rdata")
#d
#write.csv(d,file = "PCA-data.csv")
d <- read.csv(file = "PCA-data.csv")
d
View(d)
c <- cov(d[,c("x1","x2")])
c
eigen(c)
X1 <- d$x1*(-0.4926140)+d$x2*0.8702479
X1
X2 <- d$x1*(-0.8702479)+d$x2*(-0.4926140)
X2
D <- data.frame(X1,X2)
D
View(D)
plot(d$x1,d$x2)
plot(X1,X2)
