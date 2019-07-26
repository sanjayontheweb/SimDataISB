install.packages(c("ggplot2","ClusterR","fpc","dplyr","tidyr"))
library(ggplot2)
library(ClusterR)
library(fpc)
library(dplyr)
library(tidyr)
library(class)
library(plyr)

variance <- 0.25

x1 <- rnorm(50, -1, variance)
x2 <- rnorm(50, 1, variance)
y1 <- rnorm(50,-2,variance)
y2 <- rnorm(50,2, variance)
x <- c(x1,x2)
y <- c(y1,y2)

data1 <- data.frame("x" = x1, "y" = y1, "Label" = 1)
data2 <- data.frame("x" = x2, "y" = y2, "Label" = 2)
data <- data.frame("x" = c(x1,x2),"y"= c(y1,y2), "Label" = c(data1[,3],data2[,3]))

plot(data1[,c(1,2)],pch = 16,col="red", xlim = c(min(x),max(x)), ylim = c(min(y),max(y)))
points(data2[,c(1,2)],pch = 16,col="blue")

#Metric Computing
distance <- function(x,y){
  xSq <- (x[1]-y[1])^2
  ySq <- (x[2]-y[2])^2
  dist <- sqrt(xSq + ySq)
  if(dist == 0)
    return(NULL)
  return(dist)
}

intra <- function(x,df){
  pairMat <- vector() 
  
  len <- nrow(df)
  
  for(b in 1:len)
  {
    pairDist = distance(x,df[b,])
    if(!is.null(pairDist))
      pairMat[b]=pairDist
  }
  return(pairMat)
}

#RED INTRACLUSTER
redDist <- intra(data1[1,],data1)
f <- paste("Pof",1,sep = "_")
redDist <- ldply(redDist, data.frame)
names(redDist) <- f

for(b in 2:nrow(data1)){
  redList <- intra(data1[b,],data1)
  f <- paste("Pof",b,sep = "_")
  redList <- ldply(redList, data.frame)
  redDist[f] <- redList
}
 

#BLUE INTRACLUSTER
blueDist <- intra(data2[1,],data2)
f <- paste("Pof",1,sep = "_")
blueDist <- ldply(blueDist, data.frame)
names(blueDist) <- f

for(b in 2:nrow(data2)){
  blueList <- intra(data2[b,],data2)
  f <- paste("Pof",b,sep = "_")
  blueList <- ldply(blueList, data.frame)
  blueDist[f] <- blueList
}


#BETWEEN CLUSTER
crossDist <- intra(data1[1,],data2)
f <- paste("Pof",1,sep = "_")
crossDist <- ldply(crossDist, data.frame)
names(crossDist) <- f

for(b in 2:nrow(data2)){
  crossList <- intra(data1[b,],data2)
  f <- paste("Pof",b,sep = "_")
  crossList <- ldply(crossList, data.frame)
  crossDist[f] <- crossList
}

#GRAPHING

#RED
redFullVect <- redDist[,1]
for(b in 2:ncol(redDist)){
  redFullVect <- c(redFullVect,redDist[,2])
}

redFullVect <- as.data.frame(redFullVect)
names(redFullVect) <- "Distance"



#BLUE
blueFullVect <- blueDist[,1]
for(b in 2:ncol(blueDist)){
  blueFullVect <- c(blueFullVect,blueDist[,2])
}

blueFullVect <- as.data.frame(blueFullVect)
names(blueFullVect) <- "Distance"


#CROSS

crossFullVect <- crossDist[,1]
for(b in 2:ncol(crossDist)){
  crossFullVect <- c(crossFullVect,crossDist[,2])
}

crossFullVect <- as.data.frame(crossFullVect)
names(crossFullVect) <- "Distance"


#ALL TOGETHER
redFullVect$type <- "red"
blueFullVect$type <- "blue"
crossFullVect$type <- "cross"

interm <- rbind(redFullVect,blueFullVect)
interm <- rbind(interm,crossFullVect)

#without bars just omit geom_histo portion
ggplot(interm,
       aes(x=Distance,fill=type)) + geom_histogram(binwidth= 0.05,
                                                  color="black",
                                                  fill="white") +  geom_density(
                                                    aes(y=..density..*(7400*0.05))) + ggtitle(
                                                      "Plot of Pair Distances")



#INDIVIDUAL GRAPHS
redGraph <- ggplot(redFullVect,
                   aes(x=Distance)) + geom_histogram(binwidth= 0.05,
                                                     color="black",
                                                     fill="white") + geom_density(
                                                       aes(y=..density..*(2450*0.05))) + ggtitle(
                                                         "Plot of Intracluster Red Pair Distances")
redGraph


blueGraph <- ggplot(blueFullVect,
                    aes(x=Distance)) + geom_histogram(binwidth= 0.05,
                                                      color="black",
                                                      fill="white") + geom_density(
                                                        aes(y=..density..*(2450*0.05))) + ggtitle(
                                                          "Plot of Intracluster Blue Pair Distances")

blueGraph

crossGraph <- ggplot(crossFullVect,
                     aes(x=Distance)) + geom_histogram(binwidth= 0.05,
                                                       color="black",
                                                       fill="white") + geom_density(
                                                         aes(y=..density..*(2500*0.05))) + ggtitle(
                                                           "Plot of Intercluster Pair Distances")
crossGraph

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
zX <- rnorm(100,c(-1,1),variance)
zY <- rnorm(100,0,variance)
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
