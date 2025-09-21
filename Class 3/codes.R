install.packages('boot')
install.packages('car')
install.packages('caret')
install.packages('coin')
install.packages('correlation')
install.packages('corrplot')
install.packages('dplyr')
install.packages('epitools')
install.packages('factoextra')
install.packages('FactoMineR')
install.packages('finalfit')
install.packages('gapminder')
install.packages('ggfortify')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('ggsurvfit')
install.packages('gplots')
install.packages('gridExtra')
install.packages('gtsummary')
install.packages('haven')
install.packages('HH')
install.packages('InformationValue')
install.packages('irr')
install.packages('ISLR')
install.packages('ISwR')
install.packages('labelled')
install.packages('learnr')
install.packages('lessR')
install.packages('likert')
install.packages('likert')
install.packages('lsmeans')
install.packages('magrittr')
install.packages('MASS')
install.packages('multcomp')
install.packages('nlme')
install.packages('nnet')
install.packages('officer')
install.packages('PerformanceAnalytics')
install.packages('pls')
install.packages('pscl')
install.packages('psych')
install.packages('pwr')
install.packages('questionr')
install.packages('R2wd')
install.packages('RColorBrewer')
install.packages('rcompanion')
install.packages('readr')
install.packages('readxl')
install.packages('reshape2')
install.packages('rmarkdown')
install.packages('robustbase')
install.packages('rstatix')
install.packages('RVAideMemoire')
install.packages('scales')
install.packages('smd')
install.packages('survminer')
install.packages('tidycmprsk')
install.packages('tidyverse')
install.packages('tinytex')
install.packages('vcd')
install.packages('xfun')
install.packages('yaml')

##setting a working directory
getwd()
setwd("C:/Users/USER/OneDrive/Documents/MY PAPERS/R FILES")

##Importing a data set
library(haven)
stidata_unclean <- read_dta("stidata_unclean.dta")
View(stidata_unclean)

##Data properties
View(stidata_unclean)
glimpse(stidata_unclean)
names(stidata_unclean)

##Viewing the first 6 variables
head(stidata_unclean, 8)
tail(stidata_unclean)

##Data cleaning
stidata_sorted=stidata_unclean[order (stidata_unclean$idnumber, decreasing=FALSE),]
table(stidata_unclean$idnumber)

##Correcting missing values
table(stidata_unclean$sex)
View(stidata_unclean[stidata_unclean$sex=="",])
stidata_unclean$sex[stidata_unclean$idnumber ==48] = "Male"
stidata_unclean$sex[stidata_unclean$idnumber ==213] = "Female"

##Correcting inconsistencies
table(stidata_unclean$casestatus)
View(stidata_unclean[stidata_unclean$casestatus==3,])
stidata_unclean$casestatus[stidata_unclean$idnumber ==31] = 1
stidata_unclean$casestatus[stidata_unclean$idnumber ==1] = 1

##Correcting duplicates
dups=duplicated(stidata_unclean$idnumber)
table(dups)
stidata_unclean = stidata_unclean[order(stidata_unclean$idnumber),]
View(stidata_unclean[duplicated(stidata_unclean$idnumber),])
View(stidata_unclean[stidata_unclean$idnumber==51,])
stidata_unclean = stidata_unclean[!(stidata_unclean$idnumber==51 & stidata_unclean$a1age==23),]
table(stidata_unclean$idnumber)
