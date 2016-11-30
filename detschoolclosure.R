SIRVfunc=function(t, x, vparameters){
  S    = x[1]  # the value of S at time t
  I    = x[2]  # the value of I at time t
  R    = x[3]  # the value of R at time t
  V    = x[4]  # the value of Rvac at time t
  
  if (S < 0 & !is.na(S)) S=0 
  if (I < 0 & !is.na(I)) I=0 
  
  with(as.list(vparameters),{
    npop = S+I+R+V   
    dS    = -beta*S*I/npop - nu*S          
    dI    = +beta*S*I/npop - gamma*I  
    dR    = +gamma*I                  
    dV    = +nu*S
    out = c(dS,dI,dR,dV)
    list(out)
  })
}