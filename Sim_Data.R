install.packages("ggplot2")
library("ggplot2")

x <- rnorm(10, c(-1,1), 0.25)
y <- rnorm(10,0,0.01)
plot(x,y,col = c("red","blue"))
