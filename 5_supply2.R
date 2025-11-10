library(tidyverse)
library(psych)
library(ordinal)
library(modelsummary)

# make table S2

## part1 ##

data1 <- read.csv("kaken2025new.csv")

# Variables
# dependent variables
# data1$status
data1$status <- 6-data1$status

# independent variables (vignette)
# female (gender)
# married (marital status)
# college (education)
# regular (employment status)

# independent variables (subjects)
# MID (ID)
# Q1 (gender)
table(data1$Q1)
data1$S_female <- 0 
data1$S_female[data1$Q1==2] <- 1

data1$S_nonbinary <- 0
data1$S_nonbinary[data1$Q1==3] <- 1

# Q2 (cohort)
data1$A20_30 <- 0 
data1$A40_50 <- 0
data1$A60_70 <- 0 

data1$A20_30[data1$Q2<=3] <- 1 
data1$A40_50[data1$Q2>=4 & data1$Q2<=5] <- 1
data1$A60_70[data1$Q2>=6] <- 1 

# Q3 (education)
data1$S_college <- 0
data1$S_college[data1$Q3>=5] <- 1

table(data1$Q3)

data1$S_noinfor_e <- 0
data1$S_noinfor_e[is.na(data1$Q3)] <- 1

# Q4 (employment status)
data1$S_regular <- 0
data1$S_nonreg <- 0
data1$S_self <- 0
data1$S_nojob <- 0
data1$S_noinfo <- 0

table(data1$Q4)

data1$S_regular[data1$Q4<=2] <- 1
data1$S_nonreg[data1$Q4>=3 & data1$Q4<=4] <- 1
data1$S_self[data1$Q4==5] <- 1
data1$S_nojob[data1$Q4==6] <- 1
data1$S_noinfo[is.na(data1$Q4)] <- 1

data2 <- select(data1, ID, status, female, married, college, regular, 
                S_female, S_college, S_noinfor_e, S_regular, S_nonreg,
                S_self, S_nojob, S_noinfo, A20_30, A40_50, A60_70)

data2 <- na.omit(data2)
describe(data2)

group1 <- group_by(data2, S_female, S_nojob)
k1 <- data.frame(summarize(group1, rate=n()/3205))
k1

## part2 ##
data1 <- read.csv("SSP2022.csv")
data1 <- filter(data1, Q1.2Age>=30 & Q1.2Age<=39)

# Variables
# dependent variables
# data1$status
data1$status <- 6-data1$Q15.1
data1$status[data1$status <=0] <- NA

# independent variables (vignette)
# female (gender)
# married (marital status)
# college (education)
# regular (employment status)

# independent variables (subjects)
# MID (ID)
# Q1 (gender)
table(data1$Q1.1)

data1$female <- 0 
data1$female[data1$Q1.1==2] <- 1

data1$nonbinary <- 0
data1$nonbinary[data1$Q1.1==3] <- 1

# Q4.1 (education)
table(data1$Q4.1)

data1$college <- 0
data1$college[data1$Q4.1>=6 & data1$Q4.1<=21] <- 1

# Q8.1 (marriage)
table(data1$Q8.1)

data1$married <- 0
data1$devorce <- 0

data1$married[data1$Q8.1==1] <- 1
data1$devorce[data1$Q8.1==3] <- 1

# Q7.1 (employment status)
table(data1$Q7.1)

data1$regular <- 0
data1$nonreg <- 0
data1$self <- 0
data1$nojob <- 0
data1$noinfo <- 0

data1$regular[data1$Q7.1<=2] <- 1
data1$nonreg[data1$Q7.1>=3 & data1$Q7.1<=5] <- 1
data1$nonreg[data1$Q7.1==8] <- 1
data1$self[data1$Q7.1>=6 & data1$Q7.1<=7] <- 1
data1$nojob[data1$Q7.1>=9 & data1$Q7.1<=11] <- 1
data1$noinfo[data1$Q7.1>=12] <- 1

data2a <- select(data1, status, female, nonbinary, married, devorce,
                college, regular, nonreg, self, nojob, noinfo)

data2a <- na.omit(data2a)
describe(data2a)

group2 <- group_by(data2a, female, nojob)
k2 <- data.frame(summarize(group2, rate=n()/506))
k2

## part3 ##

total <- data.frame(cbind(k1, k2))
total$weight <- total$rate.1/total$rate 

total

w1 <- total[1,7]
w2 <- total[2,7]
w3 <- total[3,7]
w4 <- total[4,7]

# run model
# Multi-level analyses 

data2$weight <- NA
data2$weight[data2$S_female==0 & data2$S_nojob==0] <- w1
data2$weight[data2$S_female==0 & data2$S_nojob==1] <- w2
data2$weight[data2$S_female==1 & data2$S_nojob==0] <- w3
data2$weight[data2$S_female==1 & data2$S_nojob==1] <- w4

result1 <-MASS::polr(factor(status)~female+married+college+regular, 
                     data2, weights = weight)
result2 <-clmm(factor(status)~female+married+college+regular+(1|ID), 
               data2, weights = weight)

result3 <-clmm(factor(status)~female+married+college+regular+
                 S_female+S_college+S_regular+(1|ID), 
               data2, weights = weight)
result4 <-clmm(factor(status)~female+married+college+regular+
                 S_female+S_college+S_regular+
                 college:S_college+regular:S_regular+(1|ID), 
               data2, weights = weight)

msummary(list(result1,result2, result3, result4), stars = T)

linearHypothesis(result1, "college=regular")
