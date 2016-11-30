S_0 = 999
I_0 = 1       
R_0 = 0       
V_0 = 0
npop = S_0+I_0+R_0+V_0  
beta  = 0.6
gamma = 1/2 
#pt1
vt = seq(0,100,1)
nu    = 0.5
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
detv1 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))

plot(detv1$time,detv1$V,col="blue",ylim=c(0,1000))
points(detv1$time,detv1$R,col="red")
points(detv1$time,detv1$S,col="green")
points(detv1$time,detv1$I,col="black")