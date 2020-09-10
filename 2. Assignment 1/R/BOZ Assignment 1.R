library(tidyverse)
library(gridExtra)
library(lpSolve)

### ================ Question 1 

w <- 12
h <- 12



lp(direction = "min",
   objective.in = c(1,4),
   const.mat = matrix(c(-5,1,1,0,1,
                        2,1,2,1,0),ncol = 2,byrow = FALSE),
   const.dir = c("<=",">=",">=",">=",">="),
   const.rhs = c(10,3,4,0,0))$solution


lengthX <- 6
lengthY <- 5

x <- 0:lengthX
c1 <- (x)*2/5-2
c2 <- -x +3
c3 <- -2*x+4
c4 <- rep(0,lengthX+1)

df.points <- data.frame(x=c(0,1,1,2,5,0),
                        y=c(5,2,1,1,0,4))
df.pointsOptimal <- data.frame(x=c(0,5),
                               y=c(4,0))
point.coords = paste(df.points$x,df.points$y,sep=",")
z1.intercept <- df.points$y -1/5*df.points$x
z2.intercept <- df.points$y +4 *df.points$x

# ==== Optimal points
pointOptimal.coords = paste(df.pointsOptimal$x,df.pointsOptimal$y,sep=",")
data.frame(x, c1,c2,c3,c4) %>% 
  ggplot(aes(x = x))+
  geom_line(aes(y=c1), alpha = 0.6)+
  geom_line(aes(y=c2), alpha = 0.6)+
  geom_line(aes(y=c3), alpha = 0.6)+
  geom_line(aes(y=c4), alpha = 0.6)+
  geom_ribbon(aes(ymin = pmax(c1,c2,c3,c4),
                  ymax = lengthY),
                  fill = 'gold', 
                  alpha = 0.8)+
  geom_point(data = df.pointsOptimal, aes(x=x, y=y), color = "turquoise", size = 5)+
  geom_point(data = df.pointsOptimal, aes(x=x, y=y))+
  coord_cartesian(xlim = c(0,lengthX), ylim = c(0,lengthY))+
  labs(x="X2", y="X1", title = "Optimal points for Z1 and Z2")+
  geom_abline(slope = 1/5, intercept = c(4,-1), linetype = "dashed", color = "black")+
  geom_abline(slope = -4, intercept = c(4,20), linetype = "dotted", color = "black")+
  geom_label(data = df.pointsOptimal,aes(x+.5,y,label=pointOptimal.coords))+
  theme_light()

ggsave("Optimal points for Z1 and Z2.pdf",dpi = "retina", width = w, height = h, units = "cm")

# === Which points are eff

data.frame(x, c1,c2,c3,c4) %>% 
  ggplot(aes(x = x))+
  geom_line(aes(y=c1), alpha = 0.6)+
  geom_line(aes(y=c2), alpha = 0.6)+
  geom_line(aes(y=c3), alpha = 0.6)+
  geom_line(aes(y=c4), alpha = 0.6)+
  geom_ribbon(aes(ymin = pmax(c1,c2,c3,c4),
                  ymax = lengthY),
              fill = 'gold', 
              alpha = 0.8)+
  geom_point(data = df.pointsOptimal, aes(x=x, y=y), color = "turquoise", size = 5)+
  geom_point(data = df.points, aes(x=x, y=y))+
  coord_cartesian(xlim = c(0,lengthX), ylim = c(0,lengthY))+
  labs(x="X2", y="X1", title = "Efficient, dominated and other points")+
  geom_abline(slope = 1/5, intercept = z1.intercept, linetype = "dashed", color = "black")+
  geom_abline(slope = -4, intercept = z2.intercept, linetype = "dotted", color = "black")+
  geom_label(data = df.points,aes(x+.5,y,label=point.coords))+
  theme_light()

ggsave("Efficient, dominated and other points.pdf",dpi = "retina", width = w, height = h, units = "cm")

# Question 1 c - pre-emptive

z1 <- lp(direction = "min",
   objective.in = c(5,-1),
   const.mat = matrix(c(-5,1,1,0,1,
                         2,1,2,1,0),ncol = 2,byrow = FALSE),
   const.dir = c("<=",">=",">=",">=",">="),
   const.rhs = c(10,3,4,0,0))

z2 <- lp(direction = "min",
         objective.in = c(1,4),
         const.mat = matrix(c(-5,1,1,0,1,1,0,
                               2,1,2,1,0,0,1),ncol = 2,byrow = FALSE),
         const.dir = c("<=",">=",">=",">=",">=","<=","<="),
         const.rhs = c(10,3,4,0,0,z1$solution[1],z1$solution[2]))

z1
z2
z1$solution
z2$solution

df.pointsOptimal <- data.frame(y=c(z1$solution[1],z2$solution[1]),
                               x=c(z1$solution[2],z2$solution[2]))
point.coords = paste(df.pointsOptimal$x,df.pointsOptimal$y,sep=",")
z1.intercept <- df.pointsOptimal$y -1/5*df.pointsOptimal$x
z2.intercept <- df.pointsOptimal$y +4 *df.pointsOptimal$x


data.frame(x,c1,c2,c3,c4) %>% 
  ggplot(aes(x = x))+
  geom_line(aes(y=c1), alpha = 0.6)+
  geom_line(aes(y=c2), alpha = 0.6)+
  geom_line(aes(y=c3), alpha = 0.6)+
  geom_line(aes(y=c4), alpha = 0.6)+
  geom_ribbon(aes(ymin = pmax(c1,c2,c3,c4),
                  ymax = lengthY),
              fill = 'gold', 
              alpha = 0.8)+
  geom_point(data = df.pointsOptimal[2,], aes(x=x, y=y), color = "turquoise", size = 5)+
  geom_point(data = df.pointsOptimal, aes(x=x, y=y))+
  coord_cartesian(xlim = c(0,lengthX), ylim = c(0,lengthY))+
  labs(x="X2", y="X1", title = "Pre-emptive")+
  geom_abline(slope = 1/5, intercept = z1.intercept, linetype = "dashed", color = "black")+
  geom_abline(slope = -4, intercept = z2.intercept, linetype = "dotted", color = "black")+
  geom_label(data = df.pointsOptimal,aes(x+.5,y,label=point.coords))+
  theme_light()

ggsave("Pre-emptive.pdf",dpi = "retina", width = w, height = h, units = "cm")

# Eff fronteir 

z2.seq <- seq(0,5,0.5)
df.eff <- data.frame(Obj=z2.seq, x=z2.seq, y=z2.seq)
n = 1
for (i in z2.seq){
  
  ef.front <- lp(direction = "min",
     objective.in = c(5,-1),
     const.mat = matrix(c(-5,1,1,0,1,0,
                           2,1,2,1,0,1),ncol = 2,byrow = FALSE),
     const.dir = c("<=",">=",">=",">=",">=","<="),
     const.rhs = c(10,3,4,0,0,i))
  df.eff$Obj[n] <- ef.front$objval
  df.eff$x[n] <- ef.front$solution[2]
  df.eff$y[n] <- ef.front$solution[1]
  n <- n+1
}

df.eff %>% 
  ggplot(aes(x=x, y=y))+
  geom_path()+
  geom_point()+
  labs(x="X2", y="X1", title = "Efficient frontier")+
  geom_label(aes(x,y+.3,label=paste(x,y,sep=",")))+
  theme_light()
  
ggsave("Efficient frontier.pdf",dpi = "retina", width = w, height = h, units = "cm")


