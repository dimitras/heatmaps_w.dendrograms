options(rgl.printRglwidget = TRUE)

library(rgl)

df <- data.frame(x=runif(10,0,1), y=runif(10,0,1), z=runif(10,0,1), color=round(runif(10,1,3)))
plot3d(df$x, df$y, df$z, col=df$color, size=2, type='s')




data(iris)
iris$Species <- factor(iris$Species, levels = c("versicolor","virginica","setosa"))
round(cor(iris[,1:4]), 2)
pc <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)
summary(pc)
plot(pc,type="lines")

biplot(pc)

plot3d(pc$scores[,1:3])



