par(mfrow=c(3,3))
#1.1################################################################
histogramplot(0.6,1/2,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.6,gamma=1/2)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/2")))
#1.2################################################################
histogramplot(0.24,1/5,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.24,gamma=1/5)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/5")))
#1.3################################################################
histogramplot(0.15,1/8,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.15,gamma=1/8)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/8")))
#2.1################################################################
histogramplot(0.8,1/2,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.8,gamma=1/2)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/2")))
#2.2################################################################
histogramplot(0.32,1/5,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.32,gamma=1/5)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/5")))
#2.3################################################################
histogramplot(0.2,1/8,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.2,gamma=1/8)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/8")))
#3.1################################################################
histogramplot(1,1/2,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=1,gamma=1/2)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=1/2")))
#3.2################################################################
histogramplot(0.4,1/5,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.4,gamma=1/5)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=1/5")))
#3.3################################################################
histogramplot(0.25,1/8,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.25,gamma=1/8)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Rmax = max(sirmodel$R,na.rm=TRUE)
abline(v=Rmax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=1/8")))

######Params for vac model#########
S_0 = 999
I_0 = 1       
R_0 = 0       
V_0 = 0
npop = S_0+I_0+R_0+V_0
vt = seq(0,200,1)  
beta  = 0.6
gamma = 1/2 
time_vaccination_begins = 1   # vaccination begins on this day
time_vaccination_ends   = 100  # vaccination ends this day 
#pt1########################################
nu    = 0
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
#detv1 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
detv1 = as.data.frame(lsoda(inits, vt, Vacfunc, vparameters))
IV1 <- as.vector(aggregate(R ~ V, detv1, max), mode='numeric')
#pt2#########################################
nu    = 1/1
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv2 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV2 <- aggregate(R ~ V, detv2, max)
test <- rbind(IV1, as.vector(IV2[which(IV2$R == max(IV2$R)),], mode='numeric'))