library(gMOIP)
library(tidyverse)
library(gridExtra)
library(lpSolve)

### ================ Question 1 

lengthX <- 6
lengthY <- 6

x <- 0:lengthX
c1 <- (x)*2/5-2
c2 <- -x +3
c3 <- -2*x+4
c4 <- rep(0,lengthX+1)

df.points <- data.frame(x=c(0,1,1,2,5),
                        y=c(5,2,1,1,0))
df.pointsOptimal <- data.frame(x=c(1,5),
                               y=c(2,0))
point.coords = paste(df.points$x,df.points$y,sep=",")
z1.intercept <- df.points$y -1/5*df.points$x
z2.intercept <- df.points$y +4 *df.points$x


data.frame(x, c1,c2,c3,c4) %>% 
  ggplot(aes(x = x))+
  geom_line(aes(y=c1), alpha = 0.6)+
  geom_line(aes(y=c2), alpha = 0.6)+
  geom_line(aes(y=c3), alpha = 0.6)+
  geom_line(aes(y=c4), alpha = 0.6)+
  geom_ribbon(aes(ymin = pmax(c1,c2,c3,c4),
                  ymax = lengthY),
                  fill = 'gold', 
                  alpha = 0.6)+
  geom_point(data = df.pointsOptimal, aes(x=x, y=y), color = "turquoise", size = 5)+
  geom_point(data = df.points, aes(x=x, y=y))+
  coord_cartesian(xlim = c(0,lengthX), ylim = c(0,lengthY))+
  labs(x="x2", y="x1")+
  geom_abline(slope = 1/5, intercept = z1.intercept, linetype = "dashed", color = "black")+
  geom_abline(slope = -4, intercept = z2.intercept, linetype = "dotted", color = "black")+
  geom_label(data = df.points,aes(x+.3,y,label=point.coords))


# Question 1 c - pre-emptive

z1 <- lp(direction = "min",
   objective.in = c(5,-1),
   const.mat = matrix(c(-5,1,1,0,1,
                         2,1,2,1,0),ncol = 2,byrow = FALSE),
   const.dir = c("<=",">=",">=",">=",">="),
   const.rhs = c(10,3,4,0,0))

z2 <- lp(direction = "min",
         objective.in = c(5,-1),
         const.mat = matrix(c(-5,1,1,0,1,0,
                               2,1,2,1,0,0,
                               0,0,0,0,0,1),ncol = 3,byrow = FALSE),
         const.dir = c("<=",">=",">=",">=",">=","="),
         const.rhs = c(10,3,4,0,0,z1$objval))

z1
z2
z1$solution
z2$solution






