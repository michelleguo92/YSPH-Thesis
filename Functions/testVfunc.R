SIRVfunc=function(t, x, vparameters){
  S = x[1]  # the value of S at time t
  I = x[2]  # the value of I at time t
  R = x[3]  # the value of R at time t
  V = x[4]
  
  if (S < 0 & !is.na(S)) S=0 # this is a cross check to ensure that we always have sensical values of S
  if (I < 0 & !is.na(I)) I=0 # this is a cross check to ensure that we always have sensical values of I
  
  with(as.list(vparameters),{
    npop = S+I+R+V   # the population size is always S+I+R because there are no births or deaths in the model
    dS = -beta*S*I/npop - nu*S           # the derivative of S wrt time
    dI = +beta*S*I/npop - gamma*I  # the derivative of I wrt time
    dR = +gamma*I
    dV = +nu*S
    out = c(dS,dI,dR,dv)
    list(out)
  })
}
