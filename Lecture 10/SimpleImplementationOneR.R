library(RWeka)
data(iris)
summary(iris)
set.seed(12345)
iris_rand <- iris[order(runif(150)), ]

iris_train <- iris_rand[1:110,]
iris_test <- iris_rand[111:150,]
m <- OneR(Species ~ ., data = iris_train)
p <- predict(m, iris_test)
m
