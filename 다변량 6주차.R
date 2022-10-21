library(MASS)
data(iris)
str(iris)
summary(iris)
train<-sample(1:150,100)
train
a<-lda(Species~.,data=iris,subset = train)
a
a$prior
a$counts
a$means
a$scaling
a$lev
a$svd
pa<-predict(a,iris[-train,])
pa
t<-table(pa$class,iris$Species[-train],dnn=c("Actual","Predicted"))
t
p<-sum(diag(t))/sum(t)
p
1-p
library(klaR)
plot(pa$x,main = "LDA",dim=2)
text(pa$x,as.numeric(pa$class),col = as.numeric(iris$Species[-train]),cex=1.5)
plot(a,dimen = 2)
plot(a,dimen = 1)
plot(pa$x,type = "n",main="LDA",col = as.numeric(iris$Species[-train]),cex=1.5)
text(pa$x,as.character(pa$class),col = as.numeric(iris$Species[-train]),cex=1.5)
