install.packages(c("ggplot2","ClusterR","fpc","dplyr","tidyr"))
library(ggplot2)
library(ClusterR)
library(fpc)
library(dplyr)
library(tidyr)
library(class)


x1 <- rnorm(50, -1, 0.25)
x2 <- rnorm(50, 1, 0.25)
y1 <- rnorm(50,-2,0.25)
y2 <- rnorm(50,2,0.25)
x <- c(x1,x2)
y <- c(y1,y2)

data1 <- data.frame("x" = x1, "y" = y1, "Label" = 1)
data2 <- data.frame("x" = x2, "y" = y2, "Label" = 2)
data <- data.frame("x" = c(x1,x2),"y"= c(y1,y2), "Label" = c(data1[,3],data2[,3]))

plot(data1[,c(1,2)],pch = 16,col="red", xlim = c(min(x),max(x)), ylim = c(min(y),max(y)))

points(data2[,c(1,2)],pch = 16,col="blue")


#Centroid clustering
x1mean <- mean(x1)
y1mean <- mean(y1)
x2mean <- mean(x2)
y2mean <- mean(y2)
xmean <- mean(x)
ymean <- mean(y)

slope <- -(x2mean-x1mean)/(y2mean-y1mean)

intercept <- xmean - (ymean/slope)

abline(intercept, slope)
points(xmean,ymean,pch=16, col = "green")


#K Nearest Neighbors
zX <- rnorm(100,c(-1,1),0.25)
zY <- rnorm(100,0,0.25)
z1 <- data.frame("x" = zX, "y" = zY, "Label" = c(1,2))

points(z1[,c(1,2)],pch=16,col = "green")


cl <- z1[,1]
knn(data,z1,cl,3)



K <-25
trainerror = array(0,c(1,K))
testerror = array(0,c(1,K))
for(k in 1:K) {
  trainlabel <- knn(data,z1,cl,k=k)
  trainerror[k]<-sum(data$Label!=z1$Label)/nrow(data)
  testlabel <- knn(data,z1,cl,k=k)
  testerror[k]<-sum(data$Label!=z1$label)/nrow(z1)
}
matplot(c(1:K),as.vector(trainerror),type='l',main='Train (red) and test (blue) error for Knn classifier', col='red', lwd=2,ylim=c(0,1),xlab = 'Number of neighbors',ylab = 'Classification error')
matplot(c(1:K),as.vector(testerror),type='l', col='blue', lwd=2,add=T)
grid()
