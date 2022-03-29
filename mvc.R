##Analysis of Montana MVC data, following Van Handel et al, County-level Vulnerability
##assessment. 
#1. Environment Setup
#2. Summary of variables and correlation tables
#3. Continuous linearity assessment of indicators
#4. Estimation 

#1. Environment Setup
setwd("~/Desktop/MVC")
library(matrix)
library(lme4)
library(bbmle)
library(readxl)library(arm)
library(lme4)
library(glmmTMB) # fitting generalised linear mixed models
library(broom)
library(magrittr)
library(performance)
library(dplyr)
library("Hmisc")
#import data
mvc <- read_excel("MVC_master.xlsx")

#2. Summary of variables and correlation table
#histograms of dependent vars
par(mfrow=c(2,2))
hist(mvc$All, main="Histogram of All Crash")
hist(mvc$ED_Related, main="Histogram of ED Visits")
hist(mvc$Hosp_Related, main="Histogram of Hospital Visits")
hist(mvc$EMS_Related, main="Histogram of EMS Calls")

#histogram of independent vars
par(mfrow=c(2,2))
hist(mvc$`Pop_AVG_2010-2019`, main="Histogram of Pop")
hist(mvc$`Age_2010-2019_AVG`, main="Histogram of Age")
hist(mvc$`DVMT_2010-2019_AVG`, main="Histogram of DVMT")
hist(mvc$WG_Density_2021, main="Histogram of WG Density")

#corr between dependent vars
mvc.corr <- mvc[, c("All", 'ED_Related', 'Hosp_Related', 'EMS_Related')]
rcorr(as.matrix(mvc.corr), type = c("pearson"))

#3. Continous linearity assessment 
#find the quintiles of independent variable
v<-mvc$WG_Density_2021
i<-mvc$All
q_i <- quantile(v, probs = c(0,.25,.5,.75,1))
rate1 <- mean(i[v<q_i[2]])
rate2 <- mean(i[v>=q_i[2] & v<q_i[3]])
rate3 <- mean(i[v>=q_i[3] & v<q_i[4]])
rate4 <- mean(i[v>=q_i[4]])
rate <- c(rate1, rate2, rate3, rate4)
logits <- log(rate)

#calculate medians 
meds <- c( median(v[ v <q_i[2]]),
           median(v[ v >q_i[2] & v <q_i[3] ]),
           median(v[ v >q_i[3] & v <q_i[4] ]),
           median(v[ v >=q_i[4]] ) )
plot(meds, logits, main = "log(All) vs WG Density")
options(scipen=5)

#4. Estimation
eq1 <- All ~ (`Age_2010-2019_AVG` + WG_Density_2021) + offset(log (`Pop_AVG_2010-2019`))
eq2 <- All ~ (`Age_2010-2019_AVG` + WG_Density_2021) + offset(log (`DVMT_2010-2019_AVG`))

eq3 <- ED_Related ~ (`Age_2016-2020` + WG_Density_2021) + offset(log (`Pop_2016-2020`))
eq4 <- ED_Related ~ (`Age_2016-2020` + WG_Density_2021) + offset(log (`DVMT_2010-2019_AVG`))

eq5 <- Hosp_Related ~ (`Age_2016-2020` + WG_Density_2021) + offset(log (`Pop_2016-2020`))
eq6 <- Hosp_Related ~ (`Age_2016-2020` + WG_Density_2021) + offset(log (`DVMT_2010-2019_AVG`))

eq7 <- EMS_Related ~ (`Age_2018-2020` + WG_Density_2021) + offset(log (`Pop_2018-2020`))
eq8 <- EMS_Related ~ (`Age_2018-2020` + WG_Density_2021) + offset(log (`DVMT_2010-2019_AVG`))


poisson1 <- glm(eq1, data=mvc,family = quasipoisson(link = "log"))
p1 <- summary(poisson1)
poisson2 <- glm(eq2, data=mvc,family = quasipoisson(link = "log"))
p2 <- summary(poisson2)
poisson3 <- glm(eq3, data=mvc,family = quasipoisson(link = "log"))
p3 <- summary(poisson3)
poisson4 <- glm(eq4, data=mvc,family = quasipoisson(link = "log"))
p4 <- summary(poisson4)
poisson5 <- glm(eq5, data=mvc,family = quasipoisson(link = "log"))
p5 <- summary(poisson5)
poisson6 <- glm(eq6, data=mvc,family = quasipoisson(link = "log"))
p6 <- summary(poisson6)
poisson7 <- glm(eq7, data=mvc,family = quasipoisson(link = "log"))
p7 <- summary(poisson7)
poisson8 <- glm(eq8, data=mvc,family = quasipoisson(link = "log"))
p8 <- summary(poisson8)

#collinearity check
check_collinearity(poisson1)
check_collinearity(poisson2)
check_collinearity(poisson3)
check_collinearity(poisson4)
check_collinearity(poisson5)
check_collinearity(poisson6)
check_collinearity(poisson7)
check_collinearity(poisson8)
#round estimates for table
round(coef(p1), 4)
round(coef(p2), 4)
round(coef(p3), 4)
round(coef(p4), 4)
round(coef(p5), 4)
round(coef(p6), 4)
round(coef(p7), 4)
round(coef(p8), 4)




