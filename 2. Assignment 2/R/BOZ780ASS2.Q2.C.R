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
data.sdRadition <- read.csv(file = "RadiationData.csv", sep = ";") %>% summarise_all("sd") %>% as.numeric()
N = 3900


data.Radiation.Normal <- c(16,12,8,
                           12,10,6,
                           0,0,0,
                           0,0,0,
                           9,4,11,
                           8,7,7)

data.Radiation.tumour <- c(20,12,6,
                           18,15,8,
                           13,10,17,
                           6,18,16,
                           13,5,14,
                           10,10,10)

constraint1.value <- rep(data.Radiation.Normal,N) %>% matrix(ncol = 18,byrow = T)

for (i in 7:12){
  constraint1.value[,i] <- round(rnorm(n = N, mean =data.meanRadition[i-6] , sd = data.sdRadition[i-6]),digits = 4)
}
constraint1.value <- cbind(constraint1.value,matrix(data = 0,nrow = N,ncol = 6))

# Define lpsolve parameters


obj <- c(data.Radiation.tumour,rep(0,6))

constraint2.value <- matrix(c(matMaker(c(1:3,19),24)+matMaker(19,24)*-4,
                              matMaker(c(4:6,20),24)+matMaker(20,24)*-4,
                              matMaker(c(7:9,21),24)+matMaker(21,24)*-4,
                              matMaker(c(10:12,22),24)+matMaker(22,24)*-4,
                              matMaker(c(13:15,23),24)+matMaker(23,24)*-4,
                              matMaker(c(16:18,24),24)+matMaker(24,24)*-4),ncol = 24,byrow = T)

constraint.dir <- c(rep("<=",N),
                    rep("=",6))
constraint.rhs <- c(rep(40,N),
                    rep(0,6))

q2c.lp <- lp(direction = "max",
             objective.in = obj,
             const.mat = rbind(constraint1.value,constraint2.value),
             const.dir = constraint.dir,
             const.rhs = constraint.rhs,
             all.bin = T)

q2c.lp
beams <- q2b.lp$solution[1:18] %>% matrix(ncol = 3,byrow = T)
q2b.lp$solution[19:24] %>% matrix(nrow = 6,byrow = T)
beams

as.matrix(beams *matrix(data.Radiation.tumour,ncol = 3,byrow = T))%>% colSums()


