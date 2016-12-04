SIRclosefunc=function(t, x, parms){
  S = x[1]  # the value of S at time t
  I = x[2]  # the value of I at time t
  R = x[3]  # the value of R at time t
  
  if (S < 0 & !is.na(S)) S=0 
  if (I < 0 & !is.na(I)) I=0 
  
  with(as.list(parms),{
    npop = S+I+R  
    dS = -beta*S*I/npop
    dI = +beta*S*I/npop - gamma*I  
    dR = +gamma*I                  
    if (t >= closedur & t<= closedur + 7){
      dS = -kappa*beta*S*I/npop
      dI = +kappa*beta*S*I/npop - gamma*I
      dR = 0
    }
    out = c(dS,dI,dR)
    list(out)
  })
}
