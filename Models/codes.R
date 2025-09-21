library(haven) # for loading the stata file
library(ROSE) # for Balancing the data
library(dplyr) # for data manipulation
library(randomForest) #For developing random forest model
library(rsample) #For splitting the data
library(caret) #Contains ML models
library(pROC) # Plot ROc curve 
library(vip) #Variance importance
library(class)

data <- read_dta("E:/Jeremiah/Infant_Mort_Num_only.dta") #Reading the data

colSums(is.na(data)) # there are no missing values
table(data$infant_mortality) #The data(response variable) is imbalanced 20295 : 665

data <- data %>% 
  mutate(across(everything(),as.factor))

#data <- data[!duplicated(data),]

set.seed(2345)
balanced_data <- ROSE(infant_mortality~.,data=data,p=.5)$data #Balanced data:- response variable 10601 : 10359

#Splitting the data into train and test
set.seed(2345)
split <- initial_split(balanced_data,prop = .8,strata = infant_mortality)
train_set <- training(split) #Extracting the training set
test_set <- testing(split) #Extracting the testing set





#Developing Random forest (Default Model)
set.seed(2345)
rf_model <- randomForest(infant_mortality~.,
                         data=train_set)

#Changing class weights, class 0 =2, class1 =1
rf_model <- randomForest(infant_mortality~.,
                         data=train_set,classwt = c(2,1))
# The model is almost perfect in predicting class 1 
# The model has a bit of difficulty in classifying class 0, we can adjust it by increasing the weights for this category though it is good generally.

#Getting the number of trees to use
plot(rf_model) # From 200 the model seems to be stable, you can try that, there should be no change on the performance

default_predictions <- predict(rf_model,test_set)
actual_classes <- test_set$infant_mortality
table(actual_classes,default_predictions)

confusion_matrix <- confusionMatrix(actual_classes,default_predictions)

#sum(diag(table(default_predictions,test_set$infant_mortality)))/sum(table(default_predictions,test_set$infant_mortality))

#The default model has an accuracy of 98.54%
confusionMatrix(default_predictions,test_set$infant_mortality)

set.seed(2345)
rf_weighted_class <- randomForest(infant_mortality~.,
                                  data=train_set,
                                  ntree=300,classwt=c(2,1)) #Trying to change the weights only reduces the models performace so the initial one is better


#Getting the most important variables
vip::vip(rf_model)


rf_probs <- predict(rf_model,test_set,type = 'prob') # Returning probabilities instead of response
rf_roc_values <- roc(test_set$infant_mortality,rf_probs[,'0'])
auc_Value <- auc(rf_roc_values)
plot(rf_roc_values)
auc(rf_roc_values)



# Developing the logistic regression Model
logit_model <- glm(infant_mortality~.,data = train_set,family = 'binomial')
logit_predictions <- predict(logit_model,test_set,type = 'response')


logistic_roc_values <- roc(test_set$infant_mortality,logit_predictions)

plot(logistic_roc_values,col='red')

logit_classes <- ifelse(logit_predictions >=.5,1,0)
actual <- test_set$infant_mortality
logit_matrix <- confusionMatrix(table(actual,logit_classes))


auc(roc_values)

#Developing the KNN Classifier
#KNN is distance based and because of that we need to center the dataset

train_features <- train_set %>%
  select(-infant_mortality) # Removing infant mortality

test_features <- test_set %>% select(-infant_mortality)

dummy_model <- caret::dummyVars(~.,data=train_features,fullRank = T) #Creating the dummy function

train_dummies <- predict(dummy_model,newdata = train_features) #Fit the "function" to the train, - the function has learned how to create dummy variables
test_dummies <- predict(dummy_model,newdata = test_features) 

knn_classes_model <- knn(train = train_dummies,
                         test = test_dummies,
                         cl=train_set$infant_mortality) # Returns classes for confusion matrix

knn_prob_model <- knn(train = train_dummies,
              test = test_dummies,
              cl=train_set$infant_mortality,
              prob = T) # Returns probabilities for AUC and ROC

caret::confusionMatrix(table(knn_classes_model,test_set$infant_mortality))

#Choosing k: Choose odd values
# for (k in seq(1,7,2)) {
#   knn_prob_model <- knn(train = train_dummies,
#                         test = test_dummies,
#                         cl=train_set$infant_mortality,
#                         k=k)
#   table <- table(knn_prob_model,test_set$infant_mortality)
#   accuracy <- sum(diag(table))/sum(table)
#   
#   cat(sprintf("Model 1 : k = %d Accuracy = %.4f\n",k,accuracy))
# }


roc_curve <- roc(test_set$infant_mortality, as.numeric(knn_classes_model))
auc(roc_curve)

plot(roc_curve,col='blue')
lines(rf_roc_values,col='green')
lines(logistic_roc_values,col='red')

legend('bottomright',legend = c('Random forest',"KNN",'Logistic'),
       col = c('green','blue','red'),lty = 1,lwd = 3)

