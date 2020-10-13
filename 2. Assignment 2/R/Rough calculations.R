N_Prob_FN <- function(n,a,z){
  return(2*n*a^-1*log(12/a)+2*a^-1*log(2/z)+2*n)
}


N_Prob_FN(n = 1, a = 0.05, z = 0.01)
N_Prob_FN(n = 10, a = 0.1, z = 0.01)

N_Prob_FN(n = 5, a = 0.05, z = 0.05)

