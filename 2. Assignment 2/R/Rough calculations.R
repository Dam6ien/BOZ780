N_Prob_FN <- function(n,a,z){
  return(2*n*a^-1*log(12/a)+2*a^-1*log(2/z)+2*n)
}


N_Prob_FN(n = 1, a = 0.05, z = 0.01)
N_Prob_FN(n = 10, a = 0.1, z = 0.01)

N_Prob_FN(n = 5, a = 0.05, z = 0.05)

N_Prob_FN(n = 2, a = 0.01, z = 0.01)


#Prob impact for uniform dist

fn.UniGen <- function(n=100000, a, b, bins, plot=T){
  x <- runif(n=n, min=a, max=b)
  
  #hist gen
  breaks = seq(min(x),max(x),(max(x)-min(x))/bins)
  h <- hist(x, breaks = breaks, plot=plot)
  
  mat <- matrix(nrow=bins,ncol=2)
  colnames(mat)=c("Probability","Outcome")
  mat[,1] <- round(x=h$counts/n,digits = 4)
  mat[,2] <- round(x=h$mids, digits=4)
  
  if(plot){
    print(mat,quote = F)
  }
  #x
}


set.seed(20201010)

fn.UniDiscrete <- function(start, end, bins){
  hist <- hist(x = runif(n=10000, min=start, max=end),
               breaks = seq(start,end,(end-start)/bins))
  Probability <- hist$counts/10000
  Impact <- hist$mid
  solution <- data.frame(Probability,Impact)
  return(solution)
}



fn.NormalDisc <- function(n=10000,mean=0,sd=1,bins=5,plot=T){
  x = rnorm(n=n, mean=mean, sd=sd)
  
  breaks = seq(min(x),max(x),(max(x)-min(x))/bins)
  h = hist(x, breaks = breaks,plot = plot)
  
  mat <- matrix(nro=bins, ncol=2)
  colnames(mat) <- c("Prob","Impact")
  mat[,1] <- round(x=h$counts/n,digits = 4)
  mat[,2] <- round(x=h$mids,digits = 4)
  if (plot){print(mat,quote = F)}
  return(mat)
}


normDiscrete <- function(mean, sd, bins){
  hist <- hist(x = rnorm(n=10000, mean=mean, sd=sd),
               breaks = seq(start,end,(end-start)/bins))
  Probability <- hist$counts/10000
  Impact <- hist$mid
  solution <- data.frame(Probability,Impact)
  return(solution)
}





fn.UniDiscrete(start = 5.4, end = 6.6, bins = 5)


fn.NormalDisc <- function(n=10000,mean=0,sd=1,bins=5,plot=T){
  x = rnorm(n=n, mean=mean, sd=sd)
  
  breaks = seq(min(x),max(x),(max(x)-min(x))/bins)
  h = hist(x, breaks = breaks,plot = plot)
  
  mat <- matrix(nro=bins, ncol=2)
  colnames(mat) <- c("Prob","Impact")
  mat[,1] <- round(x=h$counts/n,digits = 4)
  mat[,2] <- round(x=h$mids,digits = 4)
  if (plot){print(mat,quote = F)}
}








