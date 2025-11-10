library(tidyverse)
library(ggplot2)
library(gridExtra)

# Experimental data

data1 <- read.csv("kaken2025new.csv") 

data1$status <- 6-data1$status

g1 <- ggplot(data1, aes(status))+
      geom_bar(fill="red", alpha=0.2)+
      xlab("Social Status (Experimantal Data)")+
      ylab("Count")

# survey data

data2 <- read.csv("SSP2022.csv") 
 # The SSP2022 data is available upon request to the SSP Project (http://ssp.hus.osaka-u.ac.jp/).  

data2 <- filter(data2, Q1.2Age>=30 & Q1.2Age<=39)
data2$status <- 6-data2$Q15.1
data2$status[data2$status <=0] <- NA

g2 <- ggplot(data2, aes(status))+
      geom_bar(fill="blue", alpha=0.2)+
      xlab("Social Status (SSP2022)")+
      ylab("Count")

# combine two graphs

g3 <- grid.arrange(g1,g2, nrow=1)
g3
ggsave("Figure2.png", dpi= 600, g3)

t.test(data1$status, data2$status)
var.test(data1$status, data2$status)

