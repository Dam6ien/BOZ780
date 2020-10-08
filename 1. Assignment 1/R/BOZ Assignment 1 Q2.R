library(tidyverse)
library(lpSolve)
matmaker <- function(index,length){
  vec <- rep(0,length)
  vec[index] <- 1
  return(vec)
}

#======================== Q2
#=========== Parameters
i <- 4
j <- 6
k <- 3

uij <- 1000* c(31, 0, 0, 0, 0,27,
                0,25,21,32,32, 0,
                0,37,29,38,28, 0,
                0,20,23,22,20, 0)
aj <- 1000* rep(c(80,70,80,90,120,65),i)
ci <- rep(c(340000, 300000, 840000, 85000),j) %>% matrix(nrow = i, byrow = F) %>% t() %>% as.vector()
ri <- rep(c(62000 ,29000 ,38000 ,45000),j) %>% matrix(nrow = i, byrow = F)  %>% t() %>% as.vector()
dk <- c(0,0,0)

dk.k <- c(rep(0,j*i),c(1,1,1))
uij.k <- c(dij, dk)
aj.k <- c(aj, dk)
ci.k <- c(ci, dk)
ri.k <- c(ri, dk)

len <- length(ri.k)

fn.cost <- function(lpsolve.out){
   cost.landused <- sum(lpsolve.out[1:24]*ri)
   cost.landunused <- -sum(lpsolve.out[1:24]*ri) + sum(1000*c(80,70,80,90,120,65))
   cost.construction <- sum(lpsolve.out[1:24]*ci)
   cost.userdays <- sum(lpsolve.out[1:24]*uij)
   return(paste(paste("Used land",cost.landused,sep = ": "),
                paste("Unused land",cost.landunused,sep = ": "),
                paste("Construction cost",cost.construction,sep = ": "),
                paste("User days",cost.userdays,sep = ": "),sep = "       "))
}

#=========== Constraints
cons1.lhs <- c(ri.k*matmaker(c(1,7,13,19),len),
               ri.k*matmaker(c(1+1,7+1,13+1,19+1),len),
               ri.k*matmaker(c(1+2,7+2,13+2,19+2),len),
               ri.k*matmaker(c(1+3,7+3,13+3,19+3),len),
               ri.k*matmaker(c(1+4,7+4,13+4,19+4),len),
               ri.k*matmaker(c(1+5,7+5,13+5,19+5),len)) %>% matrix(nrow = 6, byrow = T)
cons1.rhs <- 1000*c(80,70,80,90,120,65)

cons2.lhs <- (rep(1,len)*matmaker(c(7,13,19,12,18,24,2,3,4,5),len))#[1:24] %>% matrix(nrow = 4, byrow = T)
cons2.rhs <- 0

cons3.lhs <- (rep(1,len)*matmaker(c(1,6),len))#[1:24] %>% matrix(nrow = 4, byrow = T)
cons3.rhs <- 1

base.cons.lhs <- rbind(cons1.lhs,cons2.lhs,cons3.lhs)
base.cons.rhs <- c(cons1.rhs, cons2.rhs, cons3.rhs)
base.cons.dir <- c(rep("<=",j), "=","<=")
#======================== Q2B
q2b.obj <- rep(1,len)*matmaker(c(27,26,25),len)

cons1.lhs <- (ri.k+dk.k*matmaker(25,len))#[1:24] %>% matrix(nrow = 4, byrow = T)
cons1.rhs <- -40000 + (1000*sum(80,70,80,90,120,65))
cons2.lhs <- ci.k-dk.k*matmaker(26,len)
cons2.rhs <- 1200000
cons3.lhs <- uij.k+dk.k*matmaker(27,len)
cons3.rhs <- 200000

q2b.cons.lhs <- rbind(base.cons.lhs, cons1.lhs, cons2.lhs, cons3.lhs)
q2b.cons.rhs <- c(base.cons.rhs, cons1.rhs, cons2.rhs, cons3.rhs)
q2b.cons.dir <- c(base.cons.dir, ">=","<",">=")

q2b.lp <- lp(direction = "min",
   objective.in = q2b.obj, 
   const.mat = q2b.cons.lhs, 
   const.rhs = q2b.cons.rhs, 
   const.dir = q2b.cons.dir, 
   binary.vec = 1:24)
q2b.lp
q2b.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2b.lp$solution[25:27]
q2b.lp$solution %>% fn.cost()
#======================== Q2C
#========== P1
q2c.obj.p1 <- rep(1,len)*matmaker(c(25),len)
  
q2c.lp <- lp(direction = "min",
             objective.in = q2c.obj.p1, 
             const.mat = q2b.cons.lhs, 
             const.rhs = q2b.cons.rhs, 
             const.dir = q2b.cons.dir, 
             binary.vec = 1:24)
q2c.lp
q2c.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2c.lp$solution[25:27] %>% as.integer()
q2c.lp$solution %>% fn.cost
#========== P2
q2c.obj.p2 <- rep(1,len)*matmaker(c(26),len)

cons1.lhs <- rep(1,len)*matmaker(25,len)
cons1.rhs <- q2c.lp$solution[25]
cons1.dir <- "<="

q2c.cons.lhs.p2 <- rbind(q2b.cons.lhs, cons1.lhs)
q2c.cons.rhs.p2 <- c(q2b.cons.rhs,cons1.rhs)
q2c.cons.dir.p2 <- c(q2b.cons.dir,cons1.dir)

q2c.lp.p2 <- lp(direction = "min",
             objective.in = q2c.obj.p2, 
             const.mat = q2c.cons.lhs.p2, 
             const.rhs = q2c.cons.rhs.p2, 
             const.dir = q2c.cons.dir.p2, 
             binary.vec = 1:24)
q2c.lp.p2
q2c.lp.p2$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2c.lp.p2$solution[25:27] %>% as.integer()
q2c.lp.p2$solution %>% fn.cost()
#========== P3
q2c.obj.p3 <- rep(1,len)*matmaker(c(27),len)

cons1.lhs <- rep(1,len)*matmaker(26,len)
cons1.rhs <- q2c.lp.p2$solution[26]
cons1.dir <- "<="

q2c.cons.lhs.p3 <- rbind(q2c.cons.lhs.p2, cons1.lhs)
q2c.cons.rhs.p3 <- c(q2c.cons.rhs.p2,cons1.rhs)
q2c.cons.dir.p3 <- c(q2c.cons.dir.p2,cons1.dir)

q2c.lp.p3 <- lp(direction = "min",
                objective.in = q2c.obj.p3, 
                const.mat = q2c.cons.lhs.p3, 
                const.rhs = q2c.cons.rhs.p3, 
                const.dir = q2c.cons.dir.p3, 
                binary.vec = 1:24)
q2c.lp.p3
q2c.lp.p3$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2c.lp.p3$solution[25:27] %>% as.integer()
q2c.lp.p3$solution %>% fn.cost()
#======================== Q2D
q2d.obj <- rep(1,len)*matmaker(c(27,26,25),len)
q2d.obj[25] <- 1000
q2d.obj[26] <- 10
q2d.obj[27] <- 1

q2d.lp <- lp(direction = "min",
             objective.in = q2d.obj, 
             const.mat = q2b.cons.lhs, 
             const.rhs = q2b.cons.rhs, 
             const.dir = q2b.cons.dir, 
             binary.vec = 1:24)
q2d.lp
q2d.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2d.lp$solution[25:27]
q2d.lp$solution %>% fn.cost()


#======================== Q2E
q2e.obj <- 0.26*(ci.k - uij.k - ri.k)     #ri is treated as maximization
q2e.obj[25:27] <- c(1,1,1) 
q2e.lp <- lp(direction = "min",
             objective.in = q2e.obj, 
             const.mat = q2b.cons.lhs, 
             const.rhs = q2b.cons.rhs, 
             const.dir = q2b.cons.dir, 
             binary.vec = 1:24)
q2e.lp
q2e.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2e.lp$solution[25:27]
q2e.lp$solution %>% fn.cost()


# Results
q2b.lp$solution %>% fn.cost()
q2b.lp
q2b.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2b.lp$solution[25:27] %>% as.integer()

q2c.lp$solution %>% fn.cost()
q2c.lp
q2c.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2c.lp$solution[25:27] %>% as.integer()

q2c.lp.p2$solution %>% fn.cost()
q2c.lp.p2
q2c.lp.p2$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2c.lp.p2$solution[25:27] %>% as.integer()

q2c.lp.p3$solution %>% fn.cost()
q2c.lp.p3
q2c.lp.p3$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2c.lp.p3$solution[25:27] %>% as.integer()

q2d.lp$solution %>% fn.cost()
q2d.lp
q2d.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2d.lp$solution[25:27] %>% as.integer()

q2e.lp$solution %>% fn.cost()
q2e.lp
q2e.lp$solution[1:24] %>% matrix(nrow = i, byrow = T)
q2e.lp$solution[25:27] %>% as.integer()
