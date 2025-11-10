library(tidyverse)
library(psych)
library(ordinal)
library(modelsummary)

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

# descriptive statistics (table 3)

des <- describe(data2)
print(des, digits=3)

table(data2$female)
table(data2$married) 
table(data2$college) 
table(data2$regular)

data_res <- group_by(data2, ID)
data_res <- data.frame(
  summarise(data_res, S_female=mean(S_female),
            S_college=mean(S_college), 
            S_noinfor_e=mean(S_noinfor_e), 
            S_regular=mean(S_regular), 
            S_nonreg=mean(S_nonreg),
            S_self=mean(S_self), 
            S_nojob=mean(S_nojob), 
            S_noinfo=mean(S_noinfo))
)

des2 <- describe(data_res)
print(des2, digits = 3)

table(data_res$S_female) 
table(data_res$S_college) 
table(data_res$S_noinfor_e) 
table(data_res$S_regular)
table(data_res$S_nonreg) 
table(data_res$S_self)
table(data_res$S_nojob)
table(data_res$S_noinfo) 

# t-test on variables in Experimental data (table 4[upper side])

summarise(data2, N=n(), Mean=mean(status), SD=sd(status),
          Min=min(status), Max=max(status))

g1 <- ggplot(data2, aes(status))+geom_bar()
g1

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

# Multi-level analyses (table 6)
result1 <-MASS::polr(factor(status)~female+married+college+regular, 
               data2)
result2 <-clmm(factor(status)~female+married+college+regular+(1|ID), 
               data2)

result3 <-clmm(factor(status)~female+married+college+regular+
                 S_female+S_college+S_regular+(1|ID), 
                     data2)
result4 <-clmm(factor(status)~female+married+college+regular+
                 S_female+S_college+S_regular+
                 college:S_college+regular:S_regular+(1|ID), 
               data2)

msummary(list(result1,result2, result3, result4), stars = T)
