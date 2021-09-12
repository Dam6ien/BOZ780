# Varibale definition

```{r}
t <- 12
j <- 5
r <- 800
e <- 0.0000001

obj.xt <- e*(r^(-1) +1)

#        d1, d2, x1 -> x12
obj <- c(1,1,-rep(obj.xt,12))

const1.lhs.q <- data.frame()
const1.rhs.q <- data.frame()
const2.lhs.q <- data.frame()
const2.rhs.q <- data.frame()

const1.eql <- rep("<=", N*t) %>% matrix(byrow = TRUE, ncol = 1)
const2.eql <- rep(">=",N*t) %>% matrix(byrow = TRUE, ncol = 1)

for ( i in 1:N){
  
  const.random <- fn.s_jt() # for each q Zb changes, not for each constraint 
  
  const1.lhs <- cbind(rep(0,t),rep(-1,t),diag(t))
  const1.rhs <- r-c(sum(matMaker(ones = seq(1, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(2, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(3, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(4, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(5, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(6, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(7, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(8, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(9, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(10, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(11, to = t*j, by = 12),len = t*j)*const.random),
                    sum(matMaker(ones = seq(12, to = t*j, by = 12),len = t*j)*const.random)) %>% matrix(byrow = TRUE, nrow = t)
  
  
  #const2.lhs <- c(1,0,rep(1,12)/r)
  const2.lhs <-cbind(rep(1,t),rep(0,t),diag(t)*obj.xt)
  
  #const2.rhs <- 0.8 - sum(const.random/(r*t*j))
  
  const2.rhs<- 0.8 -c(sum(matMaker(ones = seq(1, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(2, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(3, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(4, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(5, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(6, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(7, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(8, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(9, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(10, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(11, to = t*j, by = 12),len = t*j)*const.random),
                      sum(matMaker(ones = seq(12, to = t*j, by = 12),len = t*j)*const.random))/r %>% matrix(byrow = TRUE, nrow = t)
  
  const1.lhs.q <- rbind(const1.lhs.q, const1.lhs) %>% as.matrix()
  const1.rhs.q <- rbind(const1.rhs.q, const1.rhs) %>% as.matrix()
  const2.lhs.q <- rbind(const2.lhs.q, const2.lhs) %>% as.matrix()
  const2.rhs.q <- rbind(const2.rhs.q, const2.rhs) %>% as.matrix()
}


#const1.lhs.q
#const1.rhs.q
#const2.lhs.q
#const2.rhs.q

const.lhs <- rbind(const1.lhs.q,const2.lhs.q)
const.rhs <- rbind(const1.rhs.q,const2.rhs.q)
const.eql <- rbind(const1.eql,const2.eql)

lp <- lp(direction = "min", 
         objective.in = obj, 
         const.mat = const.lhs, 
         const.dir = const.eql, 
         const.rhs = const.rhs)

lp

lp$solution %>% round(digits = 2)


```

# Solving the linear programme
```{r}

var.Utilisation <- c()


for (i in 1:10000){
  var.Utilisation[i] <- mean((matrix(data = fn.s_jt(),byrow = TRUE, ncol = 12)%>% colSums() + lp$solution[3:14])/r)
}

var.Utilisation %>% as.data.frame %>% ggplot(aes(x = var.Utilisation))+geom_histogram()



```