###MACHINE LEARNING
###Brian Muuo
#-------------------------------------------------------------------
###DATA
library(haven)
Infant_Mort_Num_only <- read_dta("C:/Users/User/Desktop/Stata folder/Infant_Mort_Num_only.dta")
View(Infant_Mort_Num_only)

#--------------------------------------------------------------------
###OBJECTIVE TWO###
###Installing uninstalled packages
install.packages("openintro")
install.packages("assertive")
install.packages("fst")
install.packages("naivebayes")
install.packages("rpart.plot")
install.packages("sigr")
install.packages("WVPlots")
install.packages("vtreat")
install.packages("randomForest")

#---------------------------------------------------------------
###Calling packages
library(tidyverse)
library(class)
library(openintro)
library(assertive)
library(fst)
library(broom)
library(naivebayes)
library(pROC)
library(rpart)
library(rpart.plot)
library(sigr)
library(WVPlots)
library(vtreat)
library(randomForest)
library(caTools)
library(caret)


#-----------------------------------------------------------------
###KNN_INFANT MORTALITY###
###Split the data into 80% training and 20% testing
set.seed(123)
train_index <- createDataPartition(Infant_Mort_Num_only$infant_mortality, p = 0.8, list = F)
train_data <- Infant_Mort_Num_only[train_index, ]
test_data <- Infant_Mort_Num_only[-train_index, ]



##Feature scaling
train_scaled = scale(train_data[-8])
test_scaled = scale(test_data[-8])


##Training KNN classifier and Predicting
library(class) #package used

TRAIN_knn<-knn(train=train_scaled,test=test_scaled,cl=train_data$infant_mortality,k=8)

#Model evaluation
actual<-test$infant_mortality
cm<-table(actual, TRAIN_knn)
cm
accuracy<- sum(diag(cm))/length(actual)
sprintf("Accuracy: %.2f%%", accuracy*100)


# Extract values from the confusion matrix (cm)
TN <- cm[1,1] #True Negatives
FP <- cm[1,2] #False Positives
FN <- cm[2,1] #False Negatives
TP <- cm[2,2] #True Positives

# Compute Sensitivity (Recall)
sensitivity <- TP / (TP + FN)

# Compute Specificity
specificity <- TN / (TN + FP)

# Print results
sprintf("Sensitivity (Recall): %.2f%%", sensitivity * 100)
sprintf("Specificity: %.2f%%", specificity * 100)

# Compute Positive Predictive Value (PPV) or Precision
PPV <- TP / (TP + FP)

# Compute Negative Predictive Value (NPV)
NPV <- TN / (TN + FN)

# Print results
sprintf("Positive Predictive Value (PPV): %.2f%%", PPV * 100)
sprintf("Negative Predictive Value (NPV): %.2f%%", NPV * 100)

# Load necessary library
library(pROC)

# Compute ROC Curve
roc_curve <- roc(actual, as.numeric(TRAIN_knn))  # actual vs predicted
plot(roc_curve, col="blue", main="ROC Curve")

# Print AUC (Area Under Curve)
auc_value <- auc(roc_curve)
sprintf("AUC: %.2f", auc_value*100)



#----------------------------------------------------------------
###RANDOM FOREST###
##Changing response to factor
Infant_Mort_Num_only$infant_mortality=as.factor(Infant_Mort_Num_only$infant_mortality)
summary(Infant_Mort_Num_only)

###Split the data into 80% training and 20% testing
set.seed(123)
Split_RF <- createDataPartition(Infant_Mort_Num_only$infant_mortality, p = 0.8, list = F)
train_RF <- Infant_Mort_Num_only[Split_RF, ]
test_RF <- Infant_Mort_Num_only[-Split_RF, ]


classifier_RF<-randomForest(infant_mortality~.,train_RF)
classifier_RF

classifier_RF<-randomForest(infant_mortality~.,train_RF,ntree=500,mtry=3)
classifier_RF
##Variable Importance Plot
varImpPlot(classifier_RF)

# Predicting the Test set results 
RF_pred = predict(classifier_RF,test_RF[,-8]) 

#Confusion Matrix 
cfm=table(RF_pred,test_RF$infant_mortality)
cfm

#Accuracy
accuracy<- sum(diag(cm_RF))/sum(cm_RF)
sprintf("Accuracy: %.2f%%", accuracy*100)

Predictions<-as.numeric(predict(classifier_RF,test_RF[,-8],type = "response"))
pred<-prediction(Predictions,test_RF$infant_mortality)

# Extract values from the confusion matrix (cm)
TN <- cfm[1,1] #True Negatives
FN<- cfm[1,2] #False Positives
FP <- cfm[2,1] #False Negatives
TP <- cfm[2,2] #True Positives

# Compute Sensitivity (Recall)
sensitivity <- TP / (TP + FN)

# Compute Specificity
specificity <- TN / (TN + FP)

# Print results
sprintf("Sensitivity (Recall): %.2f%%", sensitivity * 100)
sprintf("Specificity: %.2f%%", specificity * 100)

# Compute Positive Predictive Value (PPV) or Precision
PPV <- TP / (TP + FP)

# Compute Negative Predictive Value (NPV)
NPV <- TN / (TN + FN)

# Print results
sprintf("Positive Predictive Value (PPV): %.2f%%", PPV * 100)
sprintf("Negative Predictive Value (NPV): %.2f%%", NPV * 100)


# Load necessary library
library(pROC)

# Compute ROC Curve
roc_curve <- roc(test_RF$infant_mortality,as.numeric(classifier_RF))  # actual vs predicted
plot(roc_curve, col="red", main="ROC Curve")

# Print AUC (Area Under Curve)
auc_value <- auc(roc_curve)
sprintf("AUC: %.2f", auc_value*100)



#----------------------------------------------------------------
###LOGISTIC REGRESSION###
###Split the data into 80% training and 20% testing
set.seed(123)
Split_L<- createDataPartition(Infant_Mort_Num_only$infant_mortality, p = 0.8, list = F)
train_L<- Infant_Mort_Num_only[Split_L, ]
test_L <- Infant_Mort_Num_only[-Split_L, ]

#model training and prediction
log_model <- glm(infant_mortality~ NUMOccupation+NUMTime_to_Water_Source+NUMSource_of_drinking_water+NUMFamily_Size+NUMChildren_Ever_Born+NUMAge_at_First_Birth+NUMReligion+NUMMarital_Status+NUMType_of_Toilet_facility+v013+v024+v102+v106+v190+v238+b4, data = train_L,family = binomial(link="logit"))
summary(log_model)

log_predict <- predict(log_model,newdata = test_L,type = "response")
log_predict <- ifelse(log_predict > 0.5,"0","1")
log_predict

my_cfm<-table(test_L$infant_mortality,log_predict)
my_cfm

library(Metrics)
pr <- prediction(log_predict,test_L$infant_mortality)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")

