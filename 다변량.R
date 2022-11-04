a <- c(0,0)
b <- c(2,2)
d <-sqrt((2-0)^2+(2-0)^2)
d
data("iris")
head(iris)
dist_E <- round(dist(iris[1:5, 1:2], method = "euclidean"),2)
plot(dist_E, xlab = "Sepal.Length", ylab = "Sepal.Width", main="Euclidean Distance")

a <- c(0,0)
b <- c(2,2)
d_M <- abs(0-2)+abs(0-2)
d_M
dist_E <- round(dist(iris[1:5, 1:2], method = "manhattan"),2)
plot(dist_E, xlab = "Datas", ylab = "Distance", main="Manhattan Distance", pch =19)

a <- c(0,0)
b <- c(2,2)
aa<-rbind(a,b)
aa
cov(aa)
i <- iris[1:5, 1:2]
x<-(5.1, 3.5)
y<-(4.9, 3.0)
x-y
t(x-y)
solve(cov(i))
cov(i)
sqrt(t(x-y)%*%solve(cov(i))%*%(x-y))