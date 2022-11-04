library(class)
iris3
train <- rbind(iris3[1:25,,1],iris3[1:25,,2], iris3[1:25,,3])
train
test <- rbind(iris3[26:50,,1],iris3[26:50,,2], iris3[26:50,,3])
test
c1 <- factor(c(rep("setosa",25), rep("versicolor", 25), rep("virginica",25)))
iris3_model <-knn(train, test, c1, k=3, prob = TRUE)
iris3_model
m <- table(c1, iris3_model, dnn=c("Actual", "Predicted"))
1-sum(diag(m))/sum(m)
sum(diag(m))/sum(m)

iris3_model_7 <-knn(train, test, c1, k=5, prob = TRUE)
iris3_model_7
m_7 <- table(c1, iris3_model_7, dnn=c("Actual", "Predicted"))
m_7
1-sum(diag(m_7))/sum(m_7)
sum(diag(m_7))/sum(m_7)