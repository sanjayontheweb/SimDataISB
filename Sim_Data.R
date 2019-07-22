install.packages(c("ggplot2","ClusterR","fpc","dplyr","tidyr"))
library(ggplot2)
library(ClusterR)
library(fpc)
library(dplyr)
library(tidyr)


x1 <- rnorm(50, -1, 0.25)
x2 <- rnorm(50, 1, 0.25)
y1 <- rnorm(50,0,0.25)
y2 <- rnorm(50,2,0.1)




x <- c(x1,x2)
y <- c(y1,y2)


data1 <- data.frame("x" = x1, "y" = y1)
data2 <- data.frame("x" = x2, "y" = y2)

plot(data1,pch = 16,col="red", xlim = c(min(x1),max(x2)), ylim = c(min(y1),max(y2)))
points(data2,pch = 16,col="blue")


x1mean <- mean(x1)
y1mean <- mean(y1)
x2mean <- mean(x2)
y2mean <- mean(y2)
xmean <- mean(x)
ymean <- mean(y)

slope <- -(x2mean-x1mean)/(y2mean-y1mean)

intercept <- xmean - ymean/slope

abline(intercept, slope)



fit <- lm(y~x, data = data1)
points(xmean,ymean,pch=16, col = "green")
