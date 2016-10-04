#DS4559: Assignment #3 (Charlotte Searle)

data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)

#creating a randomized training set and test set of approx. 75/25 split
set.seed(12345)
pima_rand <- PimaIndiansDiabetes[order(runif(768)), ]

pima_train <- pima_rand[1:600,]
pima_test <- pima_rand[601:768,]

#making the decision tree (9th attribute is "diabetes", our outcome)
library(C50)
pima_model <- C5.0(pima_train[-9], pima_train$diabetes)

pima_model
summary(pima_model)

#evaluate model performance by seeing how it does with the test set
pima_pred <- predict(pima_model, pima_test)

#create a confusion matrix
library(gmodels)
CrossTable(pima_test$diabetes, pima_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetic', 'predicted diabetic'))

#Feature Selection

#Random Forest
library(mlbench)
library(caret)
library(randomForest)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))
#Random Forest gives top 5 variables: glucose, mass, age, pregnant, pedigree

#Plot variable importance 
set.seed(12345)
library(mlbench)
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
#Graph gives top 5 variables: glucose, mass, age, pregnant, pedigree (all of importance >= 0.6)

#Subset the training data so that it only contains attributes glucose, mass, age, pregnant, pedigree
sub_pima_train <- pima_train[,-3]
sub_pima_train <- sub_pima_train[,-3]
sub_pima_train <- sub_pima_train[,-3]

#train the subset of data similary to before, using a decision tree
sub_pima_model <- C5.0(sub_pima_train[-6], sub_pima_train$diabetes)

sub_pima_model
summary(pima_model)

#evaluate performance of second model
sub_pima_pred <- predict(sub_pima_model, pima_test)

#create a confusion matrix
CrossTable(pima_test$diabetes, sub_pima_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetic', 'predicted diabetic'))

#How do the results of the confusion matrices compare from before feature selection and after?
#the number of predicted negatives increased, while the number of predicted positives decreased
#the number of false positives decreased (from 28 to 17) (from 0.167 to 0.101)
#the number of false negatives increased (from 17 to 24) (from 0.101 to 0.143)