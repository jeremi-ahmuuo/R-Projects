library(haven)
Infant_M_data_Final <- read_dta("MY PAPERS/Project Analysis/Infant_M_data_Final.dta")
View(Infant_M_data_Final)

###CHANGING VARIABLES TO FACTOR###
##NUMMarital_Status
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

Infant_M_data_Final$Age_group_factor=NA
Infant_M_data_Final$Age_group_factor=factor(Infant_M_data_Final$Age_Group,levels=c(1,2,3,4,5,6,7),labels=c("15-19","20-24","25-29","30-34","35-39","40-44","45-49"),ordered=F)

Infant_M_data_Final$Region_factor=NA
Infant_M_data_Final$Region_factor=factor(Infant_M_data_Final$Geographical_region,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47),labels =c("mombasa","kwale","kilifi","tana river","lamu","taita taveta","garissa","wajir","mandera","marsabit","isiolo","meru","tharaka-nithi","embu","kitui","machakos","makueni","nyandarua","nyeri","kirinyaga","murang’a","kiambu","turkana","west pokot","samburu","trans nzoia","uasin gishu","elgeyo-marakwet","nandi","baringo","laikipia","nakuru","narok","kajiado","kericho","bomet","kakamega","vihiga","bungoma","busia","siaya","kisumu","homa bay","migori","kisii","nyamira","nairobi"),ordered=F)


View(Infant_M_data_Final)

library(gtsummary)
library(equatiomatic)

Logist_Inf_Mort= glm(infant_mortality_factor~Marital_factor+Region_factor+Age_group_factor+Births_In_Last_3_years_factor+Place_of_Residence_factor+Level_of_Education_factor+Wealth_Index_factor+Sex_of_Child_factor+Type_of_Toilet_facility_factor+Time_to_Water_Source_factor+Occupation_factor+Age_at_First_Birth_factor+Religion_factor+Children_Ever_Born_factor+Family_Size_factor+Source_of_drinking_water_factor,family=binomial(link=logit), data =Infant_M_data_Final)
equatiomatic::extract_eq(Logist_Inf_Mort)
equatiomatic::extract_eq(Logist_Inf_Mort,useofile_coefs = T)
tbl_regression(Logist_Inf_Mort,exponentiate=T, intercept=T)
