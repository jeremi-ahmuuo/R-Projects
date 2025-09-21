

#############################################

#	DAY 4

#	Binary logistic regression
#	Nominal logistic regression
#	Ordinal logistic regression
#	Poisson regression
#	Over dispersed/Zero inflated Poisson regression
#	Truncated Poisson regression

###############################################


setwd("C:/Lazarus/LZD/Bondo/R JOOUST/Day4")
getwd()

xfun::pkg_attach(c('boot','car','caret','coin','correlation','corrplot','dplyr',
'epitools','factoextra','FactoMineR','finalfit','gapminder','ggfortify',	
'ggplot2','ggpubr','gplots','gridExtra','gtsummary','haven','HH','InformationValue',
'irr','ISLR','ISwR','labelled','learnr',	
'lessR','likert','magrittr','MASS','multcomp','nlme','nnet','officer',
'PerformanceAnalytics','psych','questionr','R2wd','RColorBrewer',	
'rcompanion','readr','likert','lsmeans','pls','pscl','pwr','readxl','reshape2',
'rmarkdown','robustbase','rstatix',
'RVAideMemoire','scales','smd','tidyverse','tinytex','vcd','xfun','yaml'
)) 


#install.packages("'packagename'")

#boot

#	Binary logistic regression

#load dataset
#install.packages("ISLR")
#library(ISLR)
#data <- ISLR::Default
#save(Default, file="Default.rda")

load(file="Default.rda")

View(data)

#This dataset contains the following information about 10,000 individuals:
#default: Indicates whether or not an individual defaulted.
#student: Indicates whether or not an individual is a student.
#balance: Average balance carried by an individual.
#income: Income of the individual.

#Create Training and Test Samples
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]
dim(train)
dim(test)

#fit logistic regression model
model <- glm(default~student+balance+income, family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model)

#A a one unit increase in balance is associated with an average increase of 0.005988 in the 
#log odds of defaulting.

#Assessing Model Fit:
#We use R2 as a way to assess how well a model fits the data. 
#This number ranges from 0 to 1, with higher values indicating better model fit.
#We can compute a metric known as McFadden’s R2,
#Values close to 0 indicate that the model has no predictive power. 
#In practice, values over 0.40 indicate that a model fits the data very well.

library(pscl)
#install.packages("pscl")
pR2(model)["McFadden"]

#A value of 0.4728807 is quite high for McFadden’s R2, 
#which indicates that our model fits the data very well and has high predictive power.

#We can also compute the importance of each predictor variable in the model

library(caret)
#install.packages("caret")

varImp(model)

#Balance is by far the most important predictor variable, followed by student status and then income.

#calculate VIF values for each predictor variable in our model
#We can also calculate the VIF values of each variable in the 
#model to see if multicollinearity is a problem:
library(car)

vif(model)

#VIF values above 5 indicate severe multicollinearity. Multicollinearity is not an issue in our model.

#Use the Model to Make Predictions
#define two individuals
new <- data.frame(balance = 1400, income = 2000, student = c("Yes", "No"))

#predict probability of defaulting
predict(model, new, type="response")

#define two individuals
new <- data.frame(balance = 1400, income = 2000, student = c("Yes", "No"))

#calculate probability of default for each individual in test dataset
predicted <- predict(model, test, type="response")


#install.packages("devtools")
#devtools::install_github("selva86/InformationValue")
library(InformationValue)

#convert defaults from "Yes" and "No" to 1's and 0's
test$default <- ifelse(test$default=="Yes", 1, 0)


#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$default, predicted)[1]
optimal

confusionMatrix(test$default, predicted)


#calculate sensitivity
sensitivity(test$default, predicted)


#calculate specificity
specificity(test$default, predicted)


#calculate total misclassification error rate
misClassError(test$default, predicted, threshold=optimal)


#plot the ROC curve !!ERROR
plotROC(test$default, predicted)


#	Nominal logistic regression

#Alexander Anderson’s data of passing grades by sex within counties. we use a log-linear model
#instead of the Cochran–Mantel–Haenszel test

anders=read_excel("anders.xls", sheet="anders")
View(anders)

Table = xtabs(Count ~ Sex + Result + County, data=anders)
Table

mantelhaen.test(Table)

groupwiseCMH(Table, group   = 3,
                       fisher  = TRUE,
                       gtest   = FALSE,
                       chisq   = FALSE,
                       method  = "fdr",
                       correct = "none",
                       digits  = 3)

### Order factors otherwise R will alphabetize them

anders$County = factor(anders$County,levels=unique(anders$County))
anders$Sex    = factor(anders$Sex,levels=unique(anders$Sex))
anders$Result = factor(anders$Result,levels=unique(anders$Result))

str(anders)

Table = xtabs(Count ~ Sex + Result + County,data=anders)
ftable(Table) 

#Log-linear model: The model here tests for mutual independence of the three variables.

library(MASS)

loglm( ~ Sex + Result + County,Table)

#Post-hoc analysis: you could slice up a multi-dimensional table in any way that makes sense for the hypotheses you want to test.

Bloom = Table[,,1]
loglm( ~ Sex + Result,Bloom)

Cobblestone = Table[,,2]
loglm( ~ Sex + Result,Cobblestone)

Dougal = Table[,,3]
loglm( ~ Sex + Result,Dougal)

Heimlich = Table[,,4]
loglm( ~ Sex + Result,Heimlich)

#Summary of analysis from Cochran–Mantel–Haenszel tests and log-linear models

$County        C-H-M     log-linear
#              p-value   p-value (lr)
#Bloom         0.0468    0.03381
#Cobblestone   0.0102    0.00529
#Dougal        0.5230    0.4433
#Heimlich      0.1750    0.14175


#Default ends


# Another example of of multinomial logistic regression

#Entering high school students make program choices among general program, 
#vocational program and academic program. 
#Their choice might be modeled using their writing score and their social economic status.

#Read in the dataset

#ml=read_dta("hsbdemo.dta") 
ml=read_excel("hsbdemo.xlsx",sheet="hsbdemo") 
View(ml)

ml$prog= factor(ml$prog)
ml$ses= factor(ml$ses,levels=unique(ml$ses))
ml$female= factor(ml$female,levels=unique(ml$female))
ml$schtyp= factor(ml$schtyp,levels=unique(ml$schtyp))
ml$honors= factor(ml$honors,levels=unique(ml$honors))

str(ml)

 .libPaths()

#library(packagename)
#install.packages("packagename")
#200 students. The outcome variable is prog, program type. 
#The predictor variables are social economic status, ses, 
#a three-level categorical variable and writing score, write, a continuous variable.

#Some descriptive statistics of the variables of interest.

with(ml, table(ses, prog))

with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))


library(nnet)

test <- multinom(prog ~ ses + write, data = ml)
summary(test)


z <- summary(test)$coefficients/summary(test)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


#ln\left(\frac{P(prog=general)}{P(prog=academic)}\right) 
#= b_{10} + b_{11}(ses=2) + b_{12}(ses=3) + b_{13}write

#ln\left(\frac{P(prog=vocation)}{P(prog=academic)}\right) 
#= b_{20} + b_{21}(ses=2) + b_{22}(ses=3) + b_{23}write

#b13 A one-unit increase in the variable write is associated with the decrease in 
#	the log odds of being in general program vs. academic program in the amount of .058 .
#b23 A one-unit increase in the variable write is associated with the decrease in 
#	the log odds of being in vocation program vs. academic program. in the amount of .1136 .
#b12 The log odds of being in general program vs. in 
#	academic program will decrease by 1.163 if moving from ses="low" to ses="high".
#b11 The log odds of being in general program vs. in academic program will decrease by 0.533 
#	if moving from ses="low"to ses="middle", although this coefficient is not significant.
#b22 The log odds of being in vocation program vs. in academic program will decrease by 0.983 
#	if moving from ses="low" to ses="high".
#b21 The log odds of being in vocation program vs. in academic program will increase by 0.291 
#	if moving from ses="low" to ses="middle", although this coefficient is not significant.

## extract the coefficients from the model and exponentiate
exp(coef(test))

#The relative risk ratio for a one-unit increase in the variable write is .9437 for being in 
#	general program vs. academic program.
#The relative risk ratio switching from ses = 1 to 3 is .3126 for being in 
#	general program vs. academic program.


#You can also use predicted probabilities to help you understand the model. 
#You can calculate predicted probabilities for each of our outcome levels

head(pp <- fitted(test))


#Examine the changes in predicted probability associated with one of our two variables
#Hold write at its mean and examining the predicted probabilities for each level of ses

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")


#look at the averaged predicted probabilities for different values of the continuous predictor 
#variable write within each level of ses.

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),3))

## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.write[, 3:5], pp.write$ses, colMeans)

#Plot the predicted probabilities against the writing score by 
#the level of ses for different levels of the outcome variable.

## melt data set to long for ggplot2

require(reshape2)

lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(pp.write)
head(lpp)  # view first few rows

## plot predicted probabilities across write values for each level of ses
## facetted by program type
ggplot(lpp,aes(x=write, y=probability, colour = ses))+geom_line()+facet_grid(variable ~., scales = "free")

#ml ends here



#	Ordinal logistic regression
#	Similar to doing Ordered probit regression only coefficients interpretations are different


#ologit=read_excel("ologit.xlsx", sheet="ologit")
ologit=read_dta("ologit.dta")
ologit$apply=factor(ologit$apply, levels = c(0, 1, 2), labels=c("unlikely", "somewhat likely", "very likely"))
ologit$pared=factor(ologit$pared, levels = c(0, 1), labels=c("no", "yes"))
ologit$public=factor(ologit$public, levels = c(0, 1), labels=c("private", "public"))


View(ologit)

#A study looks at factors that influence the decision of whether to apply to graduate school. 
#College juniors are asked if they are unlikely, somewhat likely, 
#or very likely to apply to graduate school. Hence, our outcome variable has three categories. 
#Data on parental educational status, whether the undergraduate institution is public or private, 
#and current GPA is also collected. 
#The researchers have reason to believe that the “distances” between these three points are not equal. 
#For example, the “distance” between “unlikely” and “somewhat likely” may be shorter than 
#the distance between “somewhat likely” and “very likely”.


head(ologit)
str(ologit)

ftable(ologit$apply)

lapply(ologit[,c("apply","pared","public")],table)

## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ public + apply + pared, data = ologit))

summary(ologit$gpa)

sd(ologit$gpa)

#Visualize the data

ggplot(ologit, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


## fit ordered logit model and store results 'm'
m <- polr(apply ~ pared + public + gpa, data = ologit, Hess=TRUE)

## view a summary of the model
summary(m)

#	logit (P(Y \le 1)) = 2.20 – 1.05*PARED – (-0.06)*PUBLIC – 0.616*GPA
#	logit (P(Y \le 2)) = 4.30 – 1.05*PARED – (-0.06)*PUBLIC – 0.616*GPA


## store table
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m)) # default method gives profiled CIs

confint.default(m) # CIs assuming normality

## odds ratios
exp(coef(m))

## OR and CI
exp(cbind(OR = coef(m), ci))

#These coefficients are called proportional odds ratios and we would interpret these 
#pretty much as we would odds ratios from a binary logistic regression

#Interpreting the odds ratio

#Parental Education
#(*) For students whose parents did attend college, the odds of being more likely 
#(i.e., very or somewhat likely versus unlikely) to apply is 2.85 times that of students 
#whose parents did not go to college, holding constant all other variables.

#School Type
#For students in public school, the odds of being more likely (i.e., very or somewhat likely 
#versus unlikely) to apply is 5.71% lower [i.e., (1 -0.943) x 100%] than private school students, 
#holding constant all other variables.
#(*) For students in private school, the odds of being more likely 
#to apply is 1.06 times [i.e., 1/0.943] that of public school students, 
#holding constant all other variables (positive odds ratio).

#GPA
#(*) For every one unit increase in student’s GPA the odds of being more likely 
#to apply (very or somewhat likely versus unlikely) is multiplied 1.85 times (i.e., increases 85%), 
#holding constant all other variables.


#We can plot the predicted probilities

newdat <- data.frame(
  pared = rep(0:1, 200),
  public = rep(0:1, each = 200),
  gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))

newdat$pared=factor(newdat$pared, levels = c(0, 1), labels=c("no", "yes"))
newdat$public=factor(newdat$public, levels = c(0, 1), labels=c("private", "public"))

View(newdat)

newdat <- cbind(newdat, predict(m, newdat, type = "probs"))

##show first few rows
head(newdat)

#Now we can reshape the data long with the reshape2 package and plot all of the predicted probabilities 

lnewdat <- melt(newdat, id.vars = c("pared", "public", "gpa"),
  variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat)


ggplot(lnewdat, aes(x = gpa, y = Probability, colour = Level)) +
  geom_line() + facet_grid(pared ~ public, labeller="label_both")




#ologit stops here.


#	Poisson regression

# The dataset

#make this example reproducible
set.seed(1)

#create dataset
data <- data.frame(offers = c(rep(0, 50), rep(1, 30), rep(2, 10), rep(3, 7), rep(4, 3)),
                   division = sample(c("A", "B", "C"), 100, replace = TRUE),
                   exam = c(runif(50, 60, 80), runif(30, 65, 95), runif(20, 75, 95)))


data$division = factor(data$division, levels=unique(data$division))

View(data)

head(data)

summary(data)


#view mean exam score by number of offers
#library(dplyr)
data %>% group_by(offers) %>%  summarise(mean_exam = mean(exam))


#create histogram
ggplot(data, aes(offers, fill = division)) +  geom_histogram(binwidth=.5, position="dodge")

#Most players received either zero or one offer. 
#This is typical for datasets that follow Poisson distributions:


#fit the model
model <- glm(offers ~ division + exam, family = "poisson", data = data)

#view model output
summary(model)

exp(coef(model))

#expected log count for number of offers for a one-unit increase in exam is 0.08614

#e0.08614 = 1.08996. This means there is a 9% increase in the number of offers received 
#for each additional point scored on the entrance exam.

#e0.26906  = 1.31. This means players in division C receive 31% more offers than players in division A. 
#Note that this difference is not statistically significant (p = 0.329).

#We are most interested in the residual deviance, which has a value of 82.741 on 96 degrees of freedom

pchisq(82.741, 96, lower.tail = FALSE)

#P-value of 0.8305031 means the data fits the model reasonably well

#Visualizing the Results

#find predicted number of offers using the fitted Poisson regression model
data$phat <- predict(model, type="response")

View(data)


#create plot that shows number of offers based on division and exam score

ggplot(data, aes(x = exam, y = phat, color = division)) +
  geom_point(aes(y = offers), alpha = .7, position = position_jitter(h = .2)) +
  geom_line() +
  labs(x = "Entrance Exam Score", y = "Expected number of scholarship offers")

#Players from division C (the green line) are expected to get more offers in general than 
players from either division A or division B.


#Reporting the Results

#A Poisson regression was run to predict the number of scholarship offers received by baseball 
#players based on division and entrance exam scores. 
#For each additional point scored on the entrance exam, 
#there is a 9% increase in the number of offers received (p < 0.0001). 
#Division was found to not be statistically significant.



#	Over-dispersion poisson model

#Use the data set “warpbreaks”.
#Considering “breaks” as the response variable.
#The wool “type” and “tension” are taken as predictor variables.


#Data

#data("warpbreaks", package = "irr")
data(warpbreaks) #Sample data in R

View(warpbreaks)

#save(warpbreaks, file="warpbreaks.rda")

summary(warpbreaks)

#create histogram

mean(warpbreaks$breaks)
var(warpbreaks$breaks) # calculate variance

#The variance is much greater than the mean, 
#which suggests that we will have over-dispersion in the model


# model poisson regression using glm()
poisson.model <- glm(breaks ~ wool + tension, family = "poisson", data = warpbreaks)
summary(poisson.model)

#If the Residual Deviance is greater than the degrees of freedom, then over-dispersion exists. 
#This means that the estimates are correct, but the standard errors (standard deviation) are 
#wrong and unaccounted for by the model.
#So, to have a more correct standard error we can use a quasi-poisson model:

# model quasi-poisson regression using glm()
poisson.model2<- glm(breaks ~ wool + tension, family = "quasipoisson", data = warpbreaks)
summary(poisson.model2)

#The coefficients are the same, but the standard errors are different.

exp(coef(poisson.model2))

#The estimate for wool. Its value is -0.20599, and the exponent of -0.20599 is 0.8138425.
# now 1-0.8138425=0.1861575, thus we conclude that if we change wool type from A to B, 
# the number of breaks will fall by 18.6% assuming all other variables are the same.

#Predicting From The Model

# make a dataframe with new data
newdata = data.frame(wool = "B", tension = "M")

# use 'predict()' to run model on new data
predict(poisson.model2, newdata = newdata, type = "response")

#model is predicting there will be roughly 24 breaks with wool type B and tension level M.


#Visualize the outputs

#install.packages("jtools")
#install.packages("broom")
#install.packages("broom.mixed")
#install.packages("ggstance")
#install.packages("interactions")

library(jtools)
library(broom)
library(ggstance)
library(interactions)
library(broom.mixed)

# plot regression coefficients for poisson.model2
plot_summs(poisson.model2, scale = TRUE, exp = TRUE)


# plot regression coefficients for poisson.model2 and poisson.model
plot_summs(poisson.model, poisson.model2, scale = TRUE, exp = TRUE)

#to visualize the interaction between categorical predictor variables


cat_plot(poisson.model2, pred = wool, modx = tension, data = warpbreaks)

# argument 1: regression model
# pred: The categorical variable that will appear on x-axis
# modx: Moderator variable that has an effect in combination to pred on outcome

cat_plot(poisson.model2, pred = tension, modx = wool, data = warpbreaks)
cat_plot(poisson.model2, pred = tension, modx = wool, data = warpbreaks, geom="line")
cat_plot(poisson.model2, pred = tension, modx = wool, data = warpbreaks, geom="line", plot.points = TRUE)

#warpbreaks dataset ends here



#Poisson Regression Modeling Using Rate Data

#log(X/n)=β0+∑βiXi = log(X)−log(n)=β0+∑iβiXi = log(X)=log(n)+β0+∑iβiXi

#rate data can be modeled by including the log(n) term with coefficient of 1. 
#This is called an offset. This offset is modelled with offset()


#install.packages("ISwR")
library(ISwR)

data(eba1977)

str(eba1977)

cancer=eba1977

#save(cancer, file="cancer.rda")

load("cancer.rda")


View(cancer)


# Lung cancer incidence in four Danish cities 1968-1971
# This data set contains counts of incident lung cancer cases and
# population size in four neighbouring Danish cities by age group.
# city a factor with levels Fredericia, Horsens, Kolding, and Vejle.
# age a factor with levels 40-54, 55-59, 60-64, 65-69,70-74, and 75+.
# pop a numeric vector, number of inhabitants.
# cases a numeric vector, number of lung cancer cases.

# find the log(n) of each value in 'pop' column. It is the third column
cancer$logpop = log(cancer[ ,3]) # or
cancer$logpop = log(cancer$pop)


#model the rate data with offset().
poisson.model.rate <- glm(cases ~ city + age + offset(logpop), family = "poisson", data = cancer)

#display summary
summary(poisson.model.rate)

#The residual deviance is near to degrees of freedom, 
#and the dispersion parameter is 1.5 (23.447/15) which is small, so the model is a good fit.


#predict the number of cases per 1000 population for a new data set

# create a test dataframe containing new values of variables
test.data = data.frame(city = "Kolding", age = "40-54", pop = 1000, logpop = log(1000))

# predict outcomes (responses) using 'predict()'
predicted.value <- predict(poisson.model.rate, test.data, type = "response")

# show predicted value
predicted.value

#For the city of Kolding among people in the age group 40-54, 
we could expect roughly 2 or 3 cases of lung cancer per 1000 people.


#Zero inflated poisson regression

fish=read_csv("fish.csv")
View(fish)

library(boot)


#The state wildlife biologists want to model how many fish are being caught by fishermen at a state park.
#250 groups that went to a park. 
#Each group was questioned about how many fish they caught (count), 
#how many children were in the group (child), 
#how many people were in the group (persons), 
#and whether or not they brought a camper to the park (camper).

nofish = factor(nofish)
livebait <- factor(livebait)
camper <- factor(camper)


fish$nofish = factor(fish$nofish)
fish$livebait = factor(fish$livebait, levels=unique(fish$livebait))
fish$camper = factor(fish$camper, levels=unique(fish$camper))

str(fish)

summary(fish)


## histogram with x axis in log10 scale
ggplot(fish, aes(count)) + geom_histogram() + scale_x_log10()

summary(m1 <- zeroinfl(count ~ child + camper | persons, data = fish))

mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)


dput(coef(m1, "count"))

dput(coef(m1, "zero"))

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(count ~ child + camper | persons, data = data[i, ],
    start = list(count = c(1.598, -1.0428, 0.834), zero = c(1.297, -0.564)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
res <- boot(fish, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res


## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
     bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

## compare with normal based approximation
confint(m1)

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
    bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms


newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("child", "camper", "persons")
newdata1 <- subset(newdata1, subset=(child<=persons))
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")


#fish ends here



#	ZERO-TRUNCATED POISSON


hosp=read_dta("ztp.dta")

View(hosp)

#A study of length of hospital stay, in days, as a function of age, 
#kind of health insurance and whether or not the patient died while in the hospital. 
#Length of hospital stay is recorded as a minimum of at least one day.

#1,493 observations. The length of hospital stay variable is stay. 
#The variable age gives the age group from 1 to 9 which will be treated as interval. 
#The variables hmo and died are binary indicator variables for HMO insured patients and 
patients who died while in the hospital, respectively.

hosp$hmo=factor(hosp$hmo, levels = c(0, 1), labels=c(".not insured", ".insured"))
hosp$died=factor(hosp$died, levels = c(0, 1), labels=c(".alive", ".died"))


summary(hosp)

#Visualize the data

#Histograms of stay broken down by hmo on the rows and died on the columns

ggplot(hosp, aes(stay)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(hmo ~ died, margins=TRUE, scales="free_y")


#From the histograms, it looks like the density of the distribution, 
#does vary across levels of hmo and died, 
#with shorter stays for those in HMOs (1) and shorter for those who did die, 
#including what seems to be an inflated number of 1 day stays.

ggplot(hosp, aes(factor(age), stay)) +
  geom_violin() +
  geom_jitter(size=1.5) +
  scale_y_log10() +
  stat_smooth(aes(x = age, y = stay, group=1), method="loess")

#conditional violin plots which show a kernel density estimate 
#of the distribution of stay mirrored (hence the violin) and conditional on each age group.
#The distribution of length of stay does not seem to vary much across age groups. 
#This observation from the raw data is corroborated by the relatively flat loess line.


ggplot(hosp, aes(age, fill=died)) +
  geom_histogram(binwidth=.5, position="fill") +
  facet_grid(hmo ~ ., margins=TRUE)


#For the lowest ages, a smaller proportion of people in HMOs died, but for higher ages, 
#there does not seem to be a huge difference, 
#with a slightly higher proportion in HMOs dying if anything. Overall, 
#as age group increases, the proportion of those dying increases, as expected.

#install.packages("VGAM")
library(VGAM)

#To fit the zero-truncated poisson model, we use the vglm function in the VGAM package.

m1 <- vglm(stay ~ age + hmo + died, family = pospoisson(), data = hosp)
summary(m1)


#Now let’s look at a plot of the residuals versus fitted values.

output <- data.frame(resid = resid(m1), fitted = fitted(m1))
ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_smooth(method="loess")

#There are some values that look rather extreme. 
#To see if these have much influence, we can fit lines using quantile regression, 
#these lines represent the 75th, 50th, and 25th percentiles.


ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_quantile(method="rq")


#Let’s cut the data into intervals and check box plots for each. 
#We will get the breaks from the algorithm for a histogram.


output <- within(output, {broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)})

ggplot(output, aes(broken, resid)) +
 geom_boxplot() +
 geom_jitter(alpha=.25)


#Now that feel a little more confident the model fits okay, let’s look at the coefficients.

#The value of the coefficient for age, 
#-0.0144 suggests that the log count of stay decreases by 0.0144 for each year increase in age.
#The coefficient for hmo, -0.1359 indicates that the log count of stay for HMO patient is 
#0.1359 less than for non-HMO patients.
#The log count of stay for patients who died while in the hospital was 
#0.2038 less than those patients who did not die.
#Finally, the value of the constant 2.4358 is the log count of the 
#stay when all of the predictors equal zero

dput(round(coef(m1),3))

f <- function(data, i) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = pospoisson(),
    data = data[i, ], coefstart = c(2.436, -0.014, -0.136, -0.204))
  as.vector(t(coef(summary(m))[, 1:2]))
}

set.seed(10)
res <- boot(hosp, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res


#The confidence intervals for all the parameters. 
#We start on the original scale with percentile and basic bootstrap CIs.


## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
    basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms


#Now we can estimate the incident risk ratio (IRR) for the Poisson model. 
#This is done using almost identical code as before, 
#but passing a transformation function to the h argument of boot.ci, 
#in this case, exp to exponentiate.


## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
    basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms


newdata <- expand.grid(age = 1:9, hmo = 0:1, died = 0:1)
newdata$hmo=factor(newdata$hmo, levels = c(0, 1), labels=c(".not insured", ".insured"))
newdata$died=factor(newdata$died, levels = c(0, 1), labels=c(".alive", ".died"))
newdata$yhat <- predict(m1, newdata, type = "response")


ggplot(newdata, aes(x = age, y = yhat, colour = hmo))  +
  geom_point() +
  geom_line() +
  facet_wrap(~ died)


##########

## function to return predicted values
fpred <- function(data, i, newdata) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = pospoisson(),
    data = data[i, ], coefstart = c(2.436, -0.014, -0.136, -0.204))
  predict(m, newdata, type = "response")
}

## set seed and run bootstrap with 1,200 draws
set.seed(10)
respred <- boot(hosp, fpred, R = 1200, newdata = newdata,
  parallel = "snow", ncpus = 4)

## get the bootstrapped percentile CIs
yhat <- t(sapply(1:nrow(newdata), function(i) {
  out <- boot.ci(respred, index = i, type = c("perc"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))

## merge CIs with predicted values
newdata <- cbind(newdata, yhat)

## graph with CIs
ggplot(newdata, aes(x = age, y = yhat, colour = hmo, fill = hmo))  +
  geom_ribbon(aes(ymin = pLL, ymax = pUL), alpha = .25) +
  geom_point() +
  geom_line() +
  facet_wrap(~ died)


#For Zero-truncated negative binomial regression use [family = posnegbinomial() instead of
#family = pospoisson()] and 
#is useful when there is overdispersion.



#DAY 4 ends here
