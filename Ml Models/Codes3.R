library(haven)
Infant_mortality_project <- read_dta("MY PAPERS/Project Analysis/Infant mortality project.dta")
View(Infant_mortality_project)
library(gtsummary)

Logist_Inf_Mort= glm(infant_mortality~NUMOccupation+NUMTime_to_Water_Source+NUMSource_of_drinking_water+NUMFamily_Size+NUMChildren_Ever_Born+NUMAge_at_First_Birth+NUMReligion+NUMMarital_Status+NUMType_of_Toilet_facility+v013+v024+v102+v106+v190+v238+b4, family=binomial(link=logit), data =Infant_mortality_project)
summary(Logist_Inf_Mort)

equatiomatic::extract_eq(Logist_Inf_Mort)
equatiomatic::extract_eq(Logist_Inf_Mort,use_coefs = T)
tbl_regression(Logist_Inf_Mort,exponentiate=T, intercept=T)


##Changing variables to factors.
library(haven)
Infant_M_data_Final <- read_dta("MY PAPERS/Project Analysis/Infant_M_data_Final.dta")

Infant_M_data_Final$Marital_factor=NA
Infant_M_data_Final$Marital_factor=factor(Infant_M_data_Final$NUMMarital_Status,levels=c(1,2,3,4),labels=c("Dd","Married","Single","Widowed"),ordered=F)

Infant_M_data_Final$Age_group_factor=NA
Infant_M_data_Final$Age_group_factor=factor(Infant_M_data_Final$Age_Group,levels=c(1,2,3,4,5,6,7),labels=c("15-19","20-24","25-29","30-34","35-39","40-44","45-49"),ordered=F)

Infant_M_data_Final$Region_factor=NA
Infant_M_data_Final$Region_factor=factor(Infant_M_data_Final$Geographical_region,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47),labels =c("mombasa","kwale","kilifi","tana river","lamu","taita taveta","garissa","wajir","mandera","marsabit","isiolo","meru","tharaka-nithi","embu","kitui","machakos","makueni","nyandarua","nyeri","kirinyaga","murang’a","kiambu","turkana","west pokot","samburu","trans nzoia","uasin gishu","elgeyo-marakwet","nandi","baringo","laikipia","nakuru","narok","kajiado","kericho","bomet","kakamega","vihiga","bungoma","busia","siaya","kisumu","homa bay","migori","kisii","nyamira","nairobi"),ordered=F)


Infant_M_data_Final$Marital_factor=NA
Infant_M_data_Final$Marital_factor=factor(Infant_M_data_Final$NUMMarital_Status,levels=c(1,2,3,4),labels=c("Divorced","Married","Single","Widowed"),ordered=F)


Infant_M_data_Final$Source_of_drinking_water_factor=NA
Infant_M_data_Final$Source_of_drinking_water_factor=factor(Infant_M_data_Final$NUMSource_of_drinking_water,levels=c(1,2,3,4,5),labels=c("Containerised","Natural Sources","Others","Piped Water","Wells and Boreholes"),ordered=F)


Infant_M_data_Final$Family_Size_factor=NA
Infant_M_data_Final$Family_Size_factor=factor(Infant_M_data_Final$NUMFamily_Size,levels=c(1,2,3),labels=c("1 - 5","6 - 10","More Than 10"),ordered=F)

Infant_M_data_Final$Children_Ever_Born_factor=NA
Infant_M_data_Final$Children_Ever_Born_factor=factor(Infant_M_data_Final$NUMChildren_Ever_Born,levels=c(1,2,3),labels=c("1 - 3","4 - 7","More than 7"),ordered=F)

Infant_M_data_Final$Religion_factor=NA
Infant_M_data_Final$Religion_factor=factor(Infant_M_data_Final$NUMReligion,levels=c(1,2,3,4,5),labels=c("Islam","No religion","Others","Traditionists","christian"),ordered=F)

Infant_M_data_Final$Age_at_First_Birth_factor=NA
Infant_M_data_Final$Age_at_First_Birth_factor=factor(Infant_M_data_Final$NUMAge_at_First_Birth,levels=c(1,2,3),labels=c("20-30 Yrs","<20 Yrs",">30 Yrs"),ordered=F)

Infant_M_data_Final$Occupation_factor=NA
Infant_M_data_Final$Occupation_factor=factor(Infant_M_data_Final$NUMOccupation ,levels=c(1,2,3),labels=c("Employed","Others","Unemployed"),ordered=F)

Infant_M_data_Final$Time_to_Water_Source_factor=NA
Infant_M_data_Final$Time_to_Water_Source_factor=factor(Infant_M_data_Final$NUMTime_to_Water_Source,levels=c(1,2,3,4,5),labels=c("Don't Know","Long Time","Medium Time","On Premises","Short Time"),ordered=F)

Infant_M_data_Final$Type_of_Toilet_facility_factor=NA
Infant_M_data_Final$Type_of_Toilet_facility_factor=factor(Infant_M_data_Final$NUMType_of_Toilet_facility,levels=c(1,2,3,4,5),labels=c("Flush Toilet","No Toilet Facility","Other","Pit with Slab","Pit without Slab"),ordered=F)

Infant_M_data_Final$Sex_of_Child_factor=NA
Infant_M_data_Final$Sex_of_Child_factor=factor(Infant_M_data_Final$Sex_of_Child,levels=c(1,2),labels=c("male","female"),ordered=F)

Infant_M_data_Final$infant_mortality_factor=NA
Infant_M_data_Final$infant_mortality_factor=factor(Infant_M_data_Final$infant_mortality,levels=c(0,1),labels=c("Alive","Dead"),ordered=F)

Infant_M_data_Final$Wealth_Index_factor=NA
Infant_M_data_Final$Wealth_Index_factor=factor(Infant_M_data_Final$Wealth_Index,levels=c(1,2,3,4,5),labels=c("poorest","poorer","middle","richer","richest"),ordered=F)

Infant_M_data_Final$Level_of_Education_factor=NA
Infant_M_data_Final$Level_of_Education_factor=factor(Infant_M_data_Final$Level_of_Education,levels=c(0,1,2,3),labels=c("no education","primary","secondary","higher"),ordered=F)

Infant_M_data_Final$Place_of_Residence_factor=NA
Infant_M_data_Final$Place_of_Residence_factor=factor(Infant_M_data_Final$Place_of_Residence ,levels=c(1,2),labels=c("urban","rural"),ordered=F)

Infant_M_data_Final$Births_In_Last_3_years_factor=NA
Infant_M_data_Final$Births_In_Last_3_years_factor=factor(Infant_M_data_Final$Births_In_Last_3_years,levels=c(0,1,2,3),ordered=F)

View(Infant_M_data_Final)

#logistic regression
library(gtsummary)

Logist_Inf_Mort= glm(infant_mortality_factor~Occupation_factor+Time_to_Water_Source_factor+Source_of_drinking_water_factor+Family_Size_factor+Children_Ever_Born_factor+Age_at_First_Birth_factor+Religion_factor+Marital_factor+Type_of_Toilet_facility_factor+Age_group_factor+Region_factor+Place_of_Residence_factor+Level_of_Education_factor+Wealth_Index_factor+Births_In_Last_3_years_factor+Sex_of_Child_factor, family=binomial(link=logit), data =Infant_M_data_Final)
summary(Logist_Inf_Mort)

equatiomatic::extract_eq(Logist_Inf_Mort)
equatiomatic::extract_eq(Logist_Inf_Mort,use_coefs = T)
tbl_regression(Logist_Inf_Mort,exponentiate=T, intercept=T)


##OBJECTIVE TWO

library(haven)
Infant_Mort_Num_only <- read_dta("MY PAPERS/Project Analysis/Infant_Mort_Num_only.dta")
View(Infant_Mort_Num_only)

###KNN_INFANT MORTALITY###

# Split the data into 80% training and 20% testing
set.seed(123)
train_index <- createDataPartition(Infant_Mort_Num_only$infant_mortality, p = 0.8, list = F)
train_data <- Infant_Mort_Num_only[train_index, ]
test_data <- Infant_Mort_Num_only[-train_index, ]

View(train_data)
View(test_data)

##Feature scaling
train_scaled = scale(train_data[-8])
test_scaled = scale(test_data[-8])
View(train_scaled)

##Training KNN classifier and Predicting

library(class)

TRAIN_knn<-knn(train=train_scaled, test=test_scaled,cl=train_data$infant_mortality,k=8)

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
sprintf("AUC: %.2f", auc_value *100) 
  

      
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
actual<- test_RF$infant_mortality
cm<-table(actual, RF_pred)
cm


cfm=table(RF_pred,test_RF$infant_mortality)
cfm

#Accuracy
accuracy<- sum(diag(cfm))/sum(cfm)
sprintf("Accuracy: %.2f%%", accuracy*100)

Predictions<-as.numeric(predict(classifier_RF,test_RF[,-8],type = "response"))
pred<-prediction(Predictions,test_RF$infant_mortality)
perf<-performance(pred, measure = "tpr", x.measure ="fpr")
plot(perf,colour = "blue")

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




