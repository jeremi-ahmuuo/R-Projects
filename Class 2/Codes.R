# Data cleaning
## sorting
## duplicates
## missing values
## inconsistency

attach(stidata_unclean)

#sorting
##ascending order
stidata_unclean <- stidata_unclean[order(stidata_unclean$a1age),]
View(stidata_unclean)

##descending order
stidata_unclean <- stidata_unclean[order(-stidata_unclean$a1age),]

matrixa <- matrix(c(1,2,3,4,5,6), nrow = 2)
matrixa
matrixa[1,2]
matrixa[2,3]
matrixa[2,]
matrixa[,2]

#maximum and minimum
attach(stidata_unclean)
summary(stidata_unclean$a1age)
str(stidata_unclean$a1age)

#inconsistency
table(stidata_unclean$casestatus)

# br if casestatus == 3 (stata code)

View(stidata_unclean[casestatus == 3,])
# making corrections
stidata_unclean$casestatus[stidata_unclean$idnumber == 1 | 
                             stidata_unclean$idnumber == 31] = 1
## or change one at a time
stidata_unclean$casestatus[stidata_unclean$idnumber == 1] = 1
stidata_unclean$casestatus[stidata_unclean$idnumber == 31] = 1

stidata_unclean$casestatus[stidata_unclean$casestatus == 2] = 0

# missing values
table(sex)
table(stidata_unclean$sex)
library(table1)
table1(~stidata_unclean$sex)
colSums(is.na(stidata_unclean))


#generating new variable which is numeric
stidata_unclean$sexn[stidata_unclean$sex == "Male"] = 1
stidata_unclean$sexn[stidata_unclean$sex == "Female"] = 2
library(table1)

table1(~factor(stidata_unclean$sexn))

library(dplyr)
View(stidata_unclean[is.na(stidata_unclean$sexn),])

str(stidata_unclean$sex)
str(stidata_unclean$a2occupation)
as.factor(stidata_unclean$a2occupation)
##correcting the missing values
stidata_unclean$sexn[stidata_unclean$idnumber == 48] = 1
stidata_unclean$sexn[stidata_unclean$idnumber == 213] = 2

#labeling of values
stidata_unclean$sexn = factor(stidata_unclean$sexn, levels = c(1,2), labels = c("male", "female"))
str(stidata_unclean$sex)
str(stidata_unclean$sexn)

#duplicates
dups = duplicated(stidata_unclean$idnumber);dups
table(dups)
View(stidata_unclean[duplicated(amos$idnumber),])
View(stidata_unclean[stidata_unclean$idnumber==51,])
##dropping the duplicated one
stidata_unclean <- stidata_unclean[!(amos$idnumber == 51 & amos$a1age == 23),]
###or
stidata_unclean <- stidata_unclean[-227,]


