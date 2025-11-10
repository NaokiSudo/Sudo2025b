library(tidyverse)
library(psych)
library(ordinal)
library(modelsummary)
library(car)

# The SSP2022 data is available upon request to the SSP Project (http://ssp.hus.osaka-u.ac.jp/).  
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

data2 <- select(data1, status, female, nonbinary, married, devorce,
                college, regular, nonreg, self, nojob, noinfo)

data2 <- na.omit(data2)

# descriptive statistics (table 2)

des <- describe(data2)
print(des, digits=3)

summarise(data2, N=n(), Mean=mean(status), SD=sd(status),
          Min=min(status), Max=max(status))

g1 <- ggplot(data2, aes(status))+geom_bar()
g1

# t-test for variables in SSP2022 (table 4[under side])

group1 <- group_by(data2, female)
group2 <- group_by(data2, married)
group3 <- group_by(data2, college)
group4 <- group_by(data2, regular)

summarise(group1, N=n(), Mean=mean(status), SD=sd(status),
          Min=min(status), Max=max(status))
summarise(group2, N=n(), Mean=mean(status), SD=sd(status),
          Min=min(status), Max=max(status))
summarise(group3, N=n(), Mean=mean(status), SD=sd(status),
          Min=min(status), Max=max(status))
summarise(group4, N=n(), Mean=mean(status), SD=sd(status),
          Min=min(status), Max=max(status))

t.test(status~female,data2)
t.test(status~married,data2)
t.test(status~college,data2)
t.test(status~regular,data2)

# Multi-level analyses (table 5)
result1 <-MASS::polr(factor(status)~female+nonbinary+married+devorce+college+
                       regular+self+nojob+noinfo, 
                     data2)

msummary(list(result1), stars = T)
