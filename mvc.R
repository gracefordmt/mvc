#Environment Setup
setwd("~/Desktop/MVC")
#Import data
library(matrix)
library(lme4)
library(bbmle)
library(readxl)
mvc <- read_excel("ImpairedDriving_dataMaster_v4.xlsx")
#assign indicator variables
indicators <- c("mvc$`Sex(Male)`", "mvc$`Race(White)`")

#calculate the quintiles for each indicator, calculate the prob for each by calculating mean, 
#calculate median
for(i in indicators) {
  v <- get(i)
  q_i <- quantile(v, probs = c(0, .2, .4, .6, .8,1))
  p1_i <- mean(mvc$No[v<q_i[2]])
  p2_i <- mean(mvc$No[v>=q_i[2] & v<q_i[3]])
  p3_i <- mean(mvc$No[v>=q_i[3] & v<q_i[4]])
  p4_i <- mean(mvc$No[v>=q_i[4] & v<q_i[5]])
  p5_i <- mean(mvc$No[v>=q_i[5]])
  probs_i <- c(p1_i, p2_i, p3_i, p4_i, p5_i)
  logitsvar <- log(probs_i/(1-probs_i))
 
#calculate medians 
  meds <- c( median(v[ i <q_i[2]]),
             median(v[ i >q_i[2] & v <q_i[3] ]),
             median(v[ i >q_i[3] & v <q_i[4] ]),
             median(v[ i >q_i[4] & v <q_i[5] ]),
             median(v[ v >=q_i[5]] ) )
  plot(meds, logitsvar)
}

#run poisson model and check colinearity - using all indicators
eq <- #dependent variable ~ (#indicator variable1 + indicator variable2 + ...) + offset(log (Population))
poisson1 <- glmmTMB(eg, data=mvc, ziformula=~0,family = poisson)
summary(poisson1)
check_collinearity(poisson1)
#remove indicators as needed - not sure if there's a way to get this to run auto



