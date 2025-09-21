data("iris")
view(iris)
str(iris)
set.seed(123)
split= sample.split(iris, SplitRatio =0.8)
train= subset(iris, split==T)
test= subset(iris, split==F)

view(split)
##Feature scaling
iris_features =iris[, 1:4]
iris_scaled=scale(iris_features)
summary(iris_features)
summary(iris_scaled)
test_scaled= scale(test[,1:4])
train_scaled=scale(train[,1:4])


##Training KNN classifier and Predicting
test_pred<- knn(
  train=train_scaled,
  test=test_scaled,
  cl=train$Species,
  k=20
)

view(test)
view(train)

#Model evaluation
actual<-test$Species
cm<-table(actual, test_pred)
cm

accuracy<- sum(diag(cm))/length(actual)
sprintf("Accuracy: %.2f%%", accuracy*100)
