library(e1071)
data("iris")
head(iris)
m <- navieBayes(Species~., data = iris)
pm <- prdict(m, iris)
pm
t <- table(pm. iris$Species)
t
sum(diag(t))/sum(t)
library(mlbench)
data("HouseVotes84")
summary(HouseVotes84)
sum(!complete.cases(HouseVotes84))
model <- navieBayes(class~.,  data=HouseVotes84)
model
pmodel<- predict(model,HouseVotes84)
pmodel
tt <-table(pmodel, HouseVotes84$class)
tt
sum(diag(tt))/sum(tt)
sum(!complete.cases(HouseVotes84))
data <-na.omit(HouseVotes84)
sum(!complete.cases(HouseVotes84))

Hmodel <- navieBayes(class~.,  data=HouseVotes84)
Hmodel
pHmodel<- predict(Hmodel,HouseVotes84)
pHmodel
tt <-table(pHmodel, HouseVotes84$class)
tt
sum(diag(tt))/sum(tt)