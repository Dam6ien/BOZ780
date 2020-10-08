# Sample problem 8.3 (Pre emptive)

# Question 1 c - pre-emptive

z1 <- lp(direction = "max",
         objective.in = c(1,0),
         const.mat = matrix(c(1,1,1,0,
                              0,1,0,1),ncol = 2,byrow = FALSE),
         const.dir = c("<=","<=",">=",">="),
         const.rhs = c(3,5,0,0))

z2 <- lp(direction = "max",
         objective.in = c(2,3),
         const.mat = matrix(c(1,1,1,0,1,
                              0,1,0,1,0),ncol = 2,byrow = FALSE),
         const.dir = c("<=","<=",">=",">=",">="),
         const.rhs = c(3,5,0,0,z1$solution[1]))

z1
z2
z1$solution
z2$solution
