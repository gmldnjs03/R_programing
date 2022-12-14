library(class)
data("iris")
set.seed(1234)
iris_random<-iris[sample(nrow(iris)),]
iris_random
train_x<-iris_random[1:100,-5]
test_x<-iris_random[101:150,-5]
train_y<-iris_random[1:100,5]
test_y<-iris_random[101:150,5]
m3<-knn(train=train_x,test=test_x,cl=train_y,k=3)
summary(m3)
tt3<-table(m3,test_y,dnn = c("Predicted","Actual"))
1-sum(diag(tt3))/sum(tt3)
sum(diag(tt3))/sum(tt3)
mean(m==test_y)
m10<-knn(train=train_x,test=test_x,cl=train_y,k=10)
summary(m10)
tt10<-table(m10,test_y,dnn = c("Predicted","Actual"))
1-sum(diag(tt10))/sum(tt10)
sum(diag(tt10))/sum(tt10)
mean(m10==test_y)
m60<-knn(train=train_x,test=test_x,cl=train_y,k=60)
summary(m60)
tt60<-table(m60,test_y,dnn = c("Predicted","Actual"))
1-sum(diag(tt60))/sum(tt60)
sum(diag(tt60))/sum(tt60)
mean(m60==test_y)
library(kknn)
set.seed(1234)
iris_random<-iris[sample(nrow(iris)),]
iris_random
train<-iris_random[1:100,]
test<-iris_random[101:150,]
train,kknn(Species~.,train,test,ks=seq(1,50,by=2),scale=F)
m19<-knn(train=train_x,test=test_x,cl=train_y,k=19)
tt19<-table(m19,test_y,dnn = c("Predicted","Actual"))
sum(diag(tt10))/sum(tt10)
mean(m10==test_y)

library(class)
data("iris")
a<-subset(iris,Species=="Setosa"|Species=="Versicolor")
a
b<-glm(Species~Sepal.Length,data = a,family = "binomial")
summary(b)
round(fitted(b)[c(1:5,96:100)],3)
c<-glm(Species~Sepal.Length+Sepal.Width+petal.Length+petal.Width,data = a,family = "binomial")
summary(c)
reduced<-step(c,direction = "backward")