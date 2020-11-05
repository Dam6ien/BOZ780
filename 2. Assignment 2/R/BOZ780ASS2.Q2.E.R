
normDiscrete <- function(mean, sd, bins){
  smpl <- rnorm(n=10000, mean=mean, sd=sd)
  hist <- hist(x = smpl, breaks = seq(min(smpl),max(smpl),(max(smpl)-min(smpl))/bins))
  Probability <- hist$counts/10000
  Impact <- hist$mid
  solution <- data.frame(Probability,Impact)
  return(solution)
}


radiation.data <- read.csv(file = "RadiationData.csv", sep = ";")

data.prob <- data.frame()
# Calculating the probability and impact for zeta
for (i in 1:6){
  temp <- normDiscrete(mean = as.numeric((summarise_all(radiation.data,"mean"))[i]),
                      sd = as.numeric((summarise_all(radiation.data,"sd"))[i]), 
                      bins = 3)
  data.prob <- rbind(data.prob,
              cbind(temp,Var=names((summarise_all(radiation.data,"mean"))[i])))
}


#solve expected value model

library(tidyverse)
library(lpSolve)

matMaker <- function(ones,len){
  temp <- rep(0,len)
  temp[ones] <- 1
  return(temp)
}

matMaker(c(1,2,3),100)

data.meanRadition <- read.csv(file = "RadiationData.csv", sep = ";") %>% summarise_all("mean") %>% as.numeric()
#                   N  N  N  T  T T
data.Radiation <- c(16,12,8,20,12,6,
                    12,10,6,18,15,8,
                    data.meanRadition[1:3],13,10,17,
                    data.meanRadition[4:6],6,18,16,
                    9,4,11,13,5,14,
                    8,7,7,10,10,10)

data.Radiation.Normal <- c(16,12,8,
                           12,10,6,
                           data.meanRadition[1:3],
                           data.meanRadition[4:6],
                           9,4,11,
                           8,7,7)

data.Radiation.tumour <- c(20,12,6,
                           18,15,8,
                           13,10,17,
                           6,18,16,
                           13,5,14,
                           10,10,10)

# Define lpsolve parameters


obj <- c(data.Radiation.tumour,rep(0,6),data.prob[1:9,1]*data.prob[10:18,1])

constraint1.value <- c(matMaker(c(1,4,7,10,13,16),18)*data.Radiation.Normal,rep(0,6),matMaker(c(1,9),9))
constraint2.value <- c(matMaker(c(2,5,8,11,14,17),18)*data.Radiation.Normal,rep(0,6))
constraint3.value <- c(matMaker(c(3,6,9,12,15,18),18)*data.Radiation.Normal,rep(0,6))

constraint4.value <- matrix(c(matMaker(c(1:3,19),24)+matMaker(19,24)*-4,
                              matMaker(c(4:6,20),24)+matMaker(20,24)*-4,
                              matMaker(c(7:9,21),24)+matMaker(21,24)*-4,
                              matMaker(c(10:12,22),24)+matMaker(22,24)*-4,
                              matMaker(c(13:15,23),24)+matMaker(23,24)*-4,
                              matMaker(c(16:18,24),24)+matMaker(24,24)*-4),ncol = 24,byrow = T)

constraint.dir <- c(rep("<=",3),
                    rep("=",6))
constraint.rhs <- c(rep(40,3),
                    rep(0,6))

q2b.lp <- lp(direction = "max",
             objective.in = obj,
             const.mat = rbind(constraint1.value,constraint2.value,constraint3.value,constraint4.value),
             const.dir = constraint.dir,
             const.rhs = constraint.rhs,
             all.bin = T)

q2b.lp
beams <- q2b.lp$solution[1:18] %>% matrix(ncol = 3,byrow = T)
q2b.lp$solution[19:24] %>% matrix(nrow = 6,byrow = T)
beams
as.matrix(beams *matrix(data.Radiation.Normal,ncol = 3,byrow = T))%>% colSums() %>% round(digits = 2)
as.matrix(beams *matrix(data.Radiation.tumour,ncol = 3,byrow = T))%>% colSums()

