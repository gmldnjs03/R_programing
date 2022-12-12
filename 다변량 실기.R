data(Cars)
install.packages("MASS")
data("cars")
head(cars)
model<-lm(dist~speed,data = cars)
model
plot(cars$dist~cars$speed,data = cars,pch=19)
abline(coef(model),col="red",lty=5,lwd=2)
cor.test(cars$speed,cars$dist)
b<-model$coefficients[1]
a<-model$coefficients[2]
x<-20
y=a*x+b
y
library(MASS)
data(Cars93)
head(Cars93)
ca_model<-lm(Price~EngineSize+Horsepower+Width+Length+Weight,data=Cars93)
ca_model
step(ca_model,diretion="backward")
