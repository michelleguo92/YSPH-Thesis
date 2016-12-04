require(deSolve)
S0 = 999
I0 = 1       
R0 = 0       
npop = S0+I0+R0
beta  = 0.25
gamma = 1/8
vt = seq(0,500,1)
parms  = c(gamma=gamma,beta=beta)
control = as.data.frame(lsoda(inits, vt, SIRfunc, parms))

#1.1##########################################################
beta  = 0.6
gamma = 1/2 
kappa = 0
parms  = c(gamma=gamma,beta=beta,kappa=kappa)
inits = c(S=S0,I=I0,R=R0)
closedur = 70
closure = as.data.frame(lsoda(inits, vt, SIRclosefunc, parms))

