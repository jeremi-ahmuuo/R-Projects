#installing packages
table(stidata_unclean$casestatus)
version

install.packages("table1")
library(table1)
table1(~factor(stidata_unclean$casestatus))
# /require(table1)

#importing data
library(haven)
stidata_unclean <- read_dta("C:/Users/Amos Mumo/OneDrive/Desktop/Stata Training/Datasets/stidata_unclean2.dta")
View(stidata_unclean)
stidata <- read_dta("C:/Users/Amos Mumo/OneDrive/Desktop/Stata Training/Datasets/stidata_unclean2.dta")
#working directory
getwd()
setwd("C:\\Users\\Amos Mumo\\OneDrive\\Desktop\\Stata Training\\Datasets")
setwd("C:/Users/Amos Mumo/OneDrive/Desktop/Stata Training/Datasets")

#viewing data
View(stidata_unclean)
str(stidata_unclean)
structure(stidata_unclean)
head(stidata_unclean)
tail(stidata)
names(stidata_unclean)
str(stidata_unclean$sex)
class(stidata$idnumber)
