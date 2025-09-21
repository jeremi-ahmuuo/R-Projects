

#############################################

#	DAY 5

#	Morning: Advanced data analysis
#		Mixed models with R
#		Models involving time-to-event dataset using R
#		Joint models with R (JoinPoint)
#	Afternoon: Emerging analytical techniquestechnics
#		Bayesian statistical analysis using R
#		Disease mapping and risk assessment with R

###############################################

#survminer ggsurvfit

setwd("C:/Lazarus/LZD/Bondo/R JOOUST/Day5")
getwd()

xfun::pkg_attach(c('boot','car','caret','coin','correlation','corrplot','dplyr',
'epitools','factoextra','FactoMineR','finalfit','gapminder','ggfortify',	
'ggplot2','ggpubr','gplots','ggsurvfit','gridExtra','gtsummary','haven','HH','InformationValue',
'irr','ISLR','ISwR','labelled','learnr',	
'lessR','likert','magrittr','MASS','multcomp','nlme','nnet','officer',
'PerformanceAnalytics','psych','questionr','R2wd','RColorBrewer',	
'rcompanion','readr','likert','lsmeans','pls','pscl','pwr','readxl','reshape2',
'rmarkdown','robustbase','rstatix',
'RVAideMemoire','scales','smd','survminer','tidyverse','tinytex','vcd','xfun','yaml'
)) 


myDat<-data.frame(cbind(outcome1=rnorm(1000,20,5),
outcome2=rpois(1000,5),
grp=factor(sample(c("a","b","c"), 1000, replace=T))))
str(myDat)

View(myDat)

hist(myDat$outcome2)

table(myDat$grp)

fivenum(myDat$outcome1)
stem(myDat$outcome1)


boxplot(myDat)
boxplot(outcome1~grp, data=myDat)

myDat2<-cbind(rnorm(1000,20,5), rpois(1000,5))
boxplot(myDat2)
qqnorm(myDat$outcome1)
qqline(myDat$outcome1)
t.test(myDat$outcome1, myDat$outcome2)
wilcox.test(myDat$outcome1, myDat$outcome2)
wilcox.test(myDat$outcome1, myDat$outcome2, paired=T)


?Duncan

View(Duncan)

duncan.model<-lm(Duncan$prestige ~ Duncan$income + Duncan$education)
duncan.model
summary(duncan.model)
confint(duncan.model)
duncan.model2<-lm(prestige ~ income, data=Duncan)
plot(Duncan$prestige, Duncan$income)
abline(duncan.model2)
newIncome<-data.frame(income=c(9,19,10))
predict(duncan.model2, newIncome, interval = "confidence")


#dig<-read.csv("http://www.columbia.edu/~sjm2186/EPIC_R/dig.csv",stringsAsFactors=F) #digitalis data
#names(dig)

load(file="dig.rda") 

View(dig)

#save(dig, file="dig.rda")


table(dig$TRTMT,dig$DEATH)

tab.1<-xtabs(~TRTMT + DEATH, data=dig)
epitab(tab.1)


epitab(dig$TRTMT,dig$DEATH)

epitab(tab.1, rev="rows")

tab.1<-table(dig$TRTMT[dig$AGE<50],dig$DEATH[dig$AGE<50])
tab.2<-table(dig$TRTMT[dig$AGE>=50 & dig$AGE<65],dig$DEATH[dig$AGE>=50 & dig$AGE<65])
tab.3<-table(dig$TRTMT[dig$AGE>=65],dig$DEATH[dig$AGE>=65])
or.1<-epitab(tab.1)
or.2<-epitab(tab.2)
or.3<-epitab(tab.3)


str(or.1)

View(or.1)


young<-or.1$tab[2,5:7]
middle<-or.2$tab[2,5:7]
old<-or.3$tab[2,5:7]
my.table<-data.frame(rbind(young, middle, old))
my.table


fisher.test(tab.1)
chisq.test(tab.1)
my.model<-glm(DEATH ~ TRTMT + SEX, data=dig, family=binomial)
summary(my.model)
exp(my.model$coef)
summary(my.model)$coef
sum.coef<-summary(my.model)$coef
est<-exp(sum.coef[,1])
upper.ci<-exp(sum.coef[,1]+1.96*sum.coef[,2])
lower.ci<-exp(sum.coef[,1]-1.96*sum.coef[,2])
cbind(est,upper.ci,lower.ci)
cbind(coef(my.model),confint(my.model))


#births<-read.csv("http://www.columbia.edu/~sjm2186/EPIC_R/births.csv",stringsAsFactors=F)

#save(births, file="births.rda")

load(file="births.rda")

View(births)
names(births)

births$low=factor(births$low, levels = c(0, 1), labels=c("_no", "_yes"))
births$socio=factor(births$socio, levels = c(1, 2, 3), labels=c("_low", "_mid", "_high"))
births$alcohol=factor(births$alcohol, levels = c(0, 1), labels=c("_no", "_yes"))
births$prev_pretrm=factor(births$prev_pretrm, levels = c(0, 1), labels=c("_no", "_yes"))
births$hist_hyp=factor(births$hist_hyp, levels = c(0, 1), labels=c("_no", "_yes"))
births$uterine_irr=factor(births$uterine_irr, levels = c(0, 1), labels=c("_no", "_yes"))
births$phy_visit=factor(births$phy_visit, levels = c(0, 1), labels=c("_no", "_yes"))

var_label(births$low)="Low birth"
var_label(births$socio)="SES"
var_label(births$alcohol)="Alcohol"
var_label(births$prev_pretrm)="Preterm birth"
var_label(births$hist_hyp)="Hypertension"
var_label(births$uterine_irr)="Urine irritability"
var_label(births$low)="Visited a physician"


str(births)


my.model3<-glm(low~mother_age+mother_wt+socio+alcohol+prev_pretrm+hist_hyp+uterine_irr+
phy_visit, data=births, family=binomial)
summary(my.model3)

exp(my.model3$coef)
summary(my.model3)$coef
sum.coef<-summary(my.model3)$coef
est<-exp(sum.coef[,1])
upper.ci<-exp(sum.coef[,1]+1.96*sum.coef[,2])
lower.ci<-exp(sum.coef[,1]-1.96*sum.coef[,2])
p_value<-round(sum.coef[,4],4)
cbind(est,upper.ci,lower.ci,p_value)
cbind(exp(coef(my.model3)),exp(confint(my.model3)),round(sum.coef[,4],4))

str(sum.coef)

#Survival example

# Load required packages
library(survival)
#install.packages("survminer")
library(survminer)
library(dplyr)


rm(ovarian) #remove from memory
data(ovarian)

View(ovarian)


glimpse(ovarian)

#The futime column holds the survival times. 
#fustat, tells you if an individual patients’ survival time is censored.
#two therapy regimens (rx) and 
#the attending physician assessed the regression of tumors (resid.ds) and 
#patients’ performance (according to the standardized ECOG criteria; ecog.ps) at some point.


# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx,levels = c("1", "2"),labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds,levels = c("1", "2"),labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps,levels = c("1", "2"),labels = c("good", "bad"))

# Data seems to be bimodal
hist(ovarian$age)  


ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object 
table(ovarian$fustat)


fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)

ggsurvplot(fit1, data = ovarian, pval = TRUE)


fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
summary(fit2)
ggsurvplot(fit2, data = ovarian, pval = TRUE)

names(ovarian)

#Cox proportional hazards models allow you to include covariates. 

fit3 <- survfit(surv_object ~ resid.ds + ecog.ps, data = ovarian)
summary(fit3)
ggsurvplot(fit3, data = ovarian, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps,data = ovarian)
ggforest(fit.coxph, data = ovarian, main="Hazard ration among blablas!")


#Kaplan-Meier plots

rm(colon)
data(colon)

View(colon)

library(survival)
fit <- survfit(Surv(time,status)~rx, data=colon)
fit

fit <- survfit(Surv(time,status)~rx+sex, data=colon)
fit

out_surv <- survfit(Surv(time, status) ~ rx, data = colon)
out_tidy <- tidy(out_surv)
View(out_tidy)

#install.packages("ggsurvfit")
library(ggsurvfit)


survfit2(Surv(time, status) ~ 1, data = colon) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval() +
  add_risktable()

#Estimating x-year survival
summary(survfit(Surv(time, status) ~ 1, data = colon), times = 365.25) # time is in days

#We find that the 1-year probability of survival in this study is 84%.

survfit(Surv(time, status) ~ 1, data = colon) %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )


#Estimating median survival time

survfit(Surv(time, status) ~ 1, data = colon)

#Comparing survival times between groups

survdiff(Surv(time, status) ~ sex, data = colon)

#There was no significant difference in overall survival according to sex in the colon data,
# with a p-value of p = 0.6

# The median survival time is 2351 days

#The Cox regression model

#Remember to fit a Cox proportional hazards model

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = colon$time, event = colon$status)
#surv_object 
table(colon$status)

fit.coxph <- coxph(surv_object ~ rx + sex + obstruct+perfor+adhere+nodes, data = colon)
ggforest(fit.coxph, data = colon, main="Hazard ration among blablas!")

colon$sex <- factor(colon$sex ,levels = c("0", "1"),labels = c("male", "female"))

fit.coxph <- coxph(surv_object ~ rx + sex + obstruct+perfor+adhere+nodes, data = colon)
ggforest(fit.coxph, data = colon, main="Hazard ration among blablas!")

surv_object <- Surv(time = colon$time, event = colon$status)

fit1 <- survfit(surv_object ~ sex, data = colon)
#summary(fit1)

ggsurvplot(fit1, data = colon, pval = TRUE)

#But

coxph(Surv(time, status) ~ sex, data = colon)

coxph(Surv(time, status) ~ sex, data = colon) %>% 
  tbl_regression(exp = TRUE) 

###########

#Landmark Analysis and Time Dependent Covariates

#Anderson et al (JCO, 1983) described why traditional methods such as log-rank tests 
#or Cox regression are biased in favor of responders in this scenario, 
#and proposed the landmark approach. 
#The null hypothesis in the landmark approach is that survival from landmark 
#does not depend on response status at landmark.



#Exercise
cen=read_excel("censure.xlsx",sheet="Sheet1")
View(cen)

surv_object <- Surv(time = cen$time, event = cen$censure)
surv_object 
table(cen$censure)

fit1 <- survfit(surv_object ~ censure, data = cen)
summary(fit1)

ggsurvplot(fit1, data = cen, pval = TRUE)

#Estimating median survival time

survfit(Surv(time, censure) ~ 1, data = cen)

#Comparing survival times between groups

#Duncan
