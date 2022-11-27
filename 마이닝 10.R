install.packages("kknn")
library(kknn)
data("iris")
m<-dim(iris)[1]
m
val<-sample(1:m,size = round(m/3),replace = FALSE)
val
iris_learn<-iris[-val,]
iris_learn
dim(iris_learn)
iris_val<-iris[val,]
iris_val
dim(iris_val)
cv<-train.kknn(Species~.,iris_learn,iris_val,ks=seq(1,50,by=2),scale = F)
cv
k<-cv$best.parameters$k
k
iris_kknn_normal<-kknn(Species~.,train = iris_learn,test = iris_val,distance = 1,kernel = "rectangular")
summary(iris_kknn_normal)
f1<-fitted(iris_kknn_normal)
table(iris_val$Species,f1)
pcol<-as.character(as.numeric(iris_val$Species))
pcol
pairs(iris_val[1:4],pch=pcol,col=c("green3","red")[(iris_val$Species !=f1)+1])

iris_kknn_weighted<-kknn(Species~.,train = iris_learn,test = iris_val,distance = 2,kernel = "rectangular")
summary(iris_kknn_weighted)
f2<-fitted(iris_kknn_weighted)
table(iris_val$Species,f2)
pcol<-as.character(as.numeric(iris_val$Species))
pcol
pairs(iris_val[1:4],pch=pcol,col=c("green3","red")[(iris_val$Species !=f2)+1])
