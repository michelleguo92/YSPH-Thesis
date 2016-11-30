#########################################################
#initialize beta,gamma,nu,S0,I0,R0,V0,tau0,maxtime,nsims#
#########################################################
require("deSolve")

beta <- 1
gamma <- 1/2
S0 <- 999
I0 <- 1
R0 <- 0
V0 <- 100 #ADD THIS COMPARTMENT
tau0 <- 0
maxtime <- 3000
nsims <- 1000

################################
#Tau vs infected no vaccination#
################################
par(mfrow=c(3,3))
infectedplot(.6,1/2,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/2")))
infectedplot(.24,1/5,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/5")))
infectedplot(.15,1/8,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/8")))
infectedplot(.8,1/2,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/2")))
infectedplot(.32,1/5,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/5")))
infectedplot(.2,1/8,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/8")))
infectedplot(1,1/2,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=2.0, ",gamma, "=1/2")))
infectedplot(.4,1/5,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=2.0, ",gamma, "=1/5")))
infectedplot(.25,1/8,999,1,0,0,3000,1000)
title(main=expression(paste(R0,"=2.0, ",gamma, "=1/8")))

###################################
#HISTOGRAM WITH DETERMINISTIC LINE#
###################################
par(mfrow=c(1,1))
histogramplot(0.6,1/2,999,1,0,0,3000,1000)       
vt = seq(0,3000,1)
parameters=c(beta=0.6,gamma=1/2)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(ode(inits, vt, SIRfunc, parameters))
Imax = max(sirmodel$I)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste("Histogram of infected, ", 
                            R0,"=1.2, ",gamma, "=1/2")),
      xlab="# infected", ylab="frequency")

##############################
#changing parameters N = 1000#
##############################
par(mfrow=c(3,3))
#1.1################################################################
histogramplot(0.6,1/2,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.6,gamma=1/2)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/2")))
#1.2################################################################
histogramplot(0.24,1/5,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.24,gamma=1/5)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/5")))
#1.3################################################################
histogramplot(0.15,1/8,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.15,gamma=1/8)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=1/8")))
#2.1################################################################
histogramplot(0.8,1/2,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.8,gamma=1/2)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/2")))
#2.2################################################################
histogramplot(0.32,1/5,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.32,gamma=1/5)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/5")))
#2.3################################################################
histogramplot(0.2,1/8,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.2,gamma=1/8)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=1/8")))
#3.1################################################################
histogramplot(1,1/2,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=1,gamma=1/2)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=1/2")))
#3.2################################################################
histogramplot(0.4,1/5,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.4,gamma=1/5)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=1/5")))
#3.3################################################################
histogramplot(0.25,1/8,999,1,0,0,3000,1000)
vt = seq(0,3000,1)
vparameters=c(beta=0.25,gamma=1/8)
inits=c(S=999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=1/8")))

###############################
#changing parameters N = 10000#
###############################
par(mfrow=c(3,3))
#1.1################################################################
histogramplot(.6,1/2,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.6,gamma=1/2)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=2")))
#1.2################################################################
histogramplot(.24,1/5,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.24,gamma=1/5)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=5")))
#1.3################################################################
histogramplot(.15,1/8,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.15,gamma=1/8)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.2, ",gamma, "=8")))
#2.1################################################################
histogramplot(.8,1/2,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.8,gamma=1/2)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=2")))
#2.2################################################################
histogramplot(.32,1/5,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.32,gamma=1/5)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=5")))
#2.3################################################################
histogramplot(.2,1/8,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.2,gamma=1/8)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=1.6, ",gamma, "=8")))
#3.1################################################################
histogramplot(1,1/2,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=1,gamma=1/2)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=2")))
#3.2################################################################
histogramplot(.4,1/5,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.4,gamma=1/5)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=5")))
#3.3################################################################
histogramplot(.25,1/8,9999,1,0,0,30000,1000)
vt = seq(0,30000,1)
vparameters=c(beta=.25,gamma=1/8)
inits=c(S=9999,I=1,R=0)
sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
Imax = max(sirmodel$I,na.rm=TRUE)
abline(v=Imax,col=2,lty=3,lwd=3)
title(main=expression(paste(R0,"=2, ",gamma, "=8")))

#############################################################
#B=0.6, G=1/2: Vaccination Rate vs Epidemic Size Stochastic#
#############################################################
S_0 = 999
I_0 = 1       
R_0 = 0       
V_0 = 0
npop = S_0+I_0+R_0+V_0
vt = seq(0,200,1)  
beta  = 0.6
gamma = 1/2 
#pt1
nu    = 0
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
#detv1 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
detv1 = as.data.frame(lsoda(inits, vt, Vacfunc, vparameters))
IV1 <- as.vector(aggregate(R ~ V, detv1, max), mode='numeric')
#pt2
nu    = 1/1
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv2 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV2 <- aggregate(R ~ V, detv2, max)
test <- rbind(IV1, as.vector(IV2[which(IV2$R == max(IV2$R)),], mode='numeric'))
#pt3
nu    = 1/10
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv3 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV3 <- aggregate(I ~ V, detv3, max)
test <- rbind(test, as.vector(IV3[which(IV3$I == max(IV3$I)),], mode='numeric'))
#pt4
nu    = 1/20
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv4 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV4 <- aggregate(I ~ V, detv4, max)
test <- rbind(test, as.vector(IV4[which(IV4$I == max(IV4$I)),], mode='numeric'))
#pt5
nu    = 1/30
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv5 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV5 <- aggregate(I ~ V, detv5, max)
test <- rbind(test, as.vector(IV5[which(IV5$I == max(IV5$I)),], mode='numeric'))
#pt6
nu    = 1/40
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv6 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV6 <- aggregate(I ~ V, detv6, max)
test <- rbind(test, as.vector(IV6[which(IV6$I == max(IV6$I)),], mode='numeric'))
#pt7
nu    = 1/50
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv7 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV7 <- aggregate(I ~ V, detv7, max)
test <- rbind(test, as.vector(IV7[which(IV7$I == max(IV7$I)),], mode='numeric'))
#pt8
nu    = 1/60
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv8 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV8 <- aggregate(I ~ V, detv8, max)
test <- rbind(test, as.vector(IV8[which(IV8$I == max(IV8$I)),], mode='numeric'))
test <- test[order(test[,1],test[,2],decreasing=FALSE),]
##############################################################################################
par(mfrow=c(1,1))
v1 = vaccinate(beta=0.6,gamma=1/2,nu=1/1,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
plot(v1,ylim=c(0,1000),xlab='',ylab='',col="red")
v2 = vaccinate(beta=0.6,gamma=1/2,nu=1/2,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v2, col="darkorange")
v3 = vaccinate(beta=0.6,gamma=1/2,nu=1/3,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v3, col="gold")
v4 = vaccinate(beta=0.6,gamma=1/2,nu=1/5,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v4, col="hotpink")
v5 = vaccinate(beta=0.6,gamma=1/2,nu=1/7,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v5, col="green")
v6 = vaccinate(beta=0.6,gamma=1/2,nu=1/10,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v6, col="seagreen")
v7 = vaccinate(beta=0.6,gamma=1/2,nu=1/12,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v7, col="turquoise")
v8 = vaccinate(beta=0.6,gamma=1/2,nu=1/13,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v8, col="blue")
v9 = vaccinate(beta=0.6,gamma=1/2,nu=1/15,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v9, col="purple")
v10 = vaccinate(beta=0.6,gamma=1/2,nu=1/17,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v10,col="violet")
v11 = vaccinate(beta=0.6,gamma=1/2,nu=1/20,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v11,col="black")
v12 = vaccinate(beta=0.6,gamma=1/2,nu=1/22,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v12,col="gray")
v13 = vaccinate(beta=0.6,gamma=1/2,nu=1/24,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v13,col="slategray")
v14 = vaccinate(beta=0.6,gamma=1/2,nu=1/26,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v14,col="firebrick")
v15 = vaccinate(beta=0.6,gamma=1/2,nu=1/28,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v15,col="chartreuse")
v16 = vaccinate(beta=0.6,gamma=1/2,nu=1/30,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v16,col="cadetblue1")
#points(test,type='o',lwd=5) UNCOMMENT WHEN DET IS WORKING
title(main=expression(paste("Vaccination Rate vs Epidemic Size, ", 
                            alpha,"=1-60, ",R0,"=1.2, ",gamma, "=1/2")),
      xlab="# vaccinated", ylab="Epidemic Size")


#################################################################
#VB=2.4 G=1/5: vaccination Rate vs Epidemic Size Deterministic#
#################################################################
S_0 = 999
I_0 = 1       
R_0 = 0       
V_0 = 0
npop = S_0+I_0+R_0+V_0
vt = seq(0,100,1)  
beta  = 1/.24
gamma = 1/5 
#pt1
nu    = 0
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
detv1 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV1 <- as.vector(aggregate(I ~ V, detv1, max), mode='numeric')
#pt2
nu    = 1/5
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv2 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV2 <- aggregate(I ~ V, detv2, max)
test <- rbind(IV1, as.vector(IV2[which(IV2$I == max(IV2$I)),], mode='numeric'))
#pt3
nu    = 1/10
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv3 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV3 <- aggregate(I ~ V, detv3, max)
test <- rbind(test, as.vector(IV3[which(IV3$I == max(IV3$I)),], mode='numeric'))
#pt5
nu    = 1/30
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv5 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV5 <- aggregate(I ~ V, detv5, max)
test <- rbind(test, as.vector(IV5[which(IV5$I == max(IV5$I)),], mode='numeric'))
#pt6
nu    = 1/40
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv6 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV6 <- aggregate(I ~ V, detv6, max)
test <- rbind(test, as.vector(IV6[which(IV6$I == max(IV6$I)),], mode='numeric'))
#pt7
nu    = 1/50
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv7 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV7 <- aggregate(I ~ V, detv7, max)
test <- rbind(test, as.vector(IV7[which(IV7$I == max(IV7$I)),], mode='numeric'))
#pt8
nu    = 1/60
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv8 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV8 <- aggregate(I ~ V, detv8, max)
test <- rbind(test, as.vector(IV8[which(IV8$I == max(IV8$I)),], mode='numeric'))
test <- test[order(test[,1],test[,2],decreasing=FALSE),]
##############################################################################################
v1 = vaccinate(beta=1/.24,gamma=1/5,nu=1/1,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
plot(v1,xlab='',ylab='',col="red")
v2 = vaccinate(beta=1/.24,gamma=1/5,nu=1/2,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v2, col="darkorange")
v3 = vaccinate(beta=1/.24,gamma=1/5,nu=1/3,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v3, col="gold")
v4 = vaccinate(beta=1/.24,gamma=1/5,nu=1/4,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v4, col="hotpink")
v5 = vaccinate(beta=1/.24,gamma=1/5,nu=1/5,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v5, col="green")
v6 = vaccinate(beta=1/.24,gamma=1/5,nu=1/7,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v6, col="seagreen")
v7 = vaccinate(beta=1/.24,gamma=1/5,nu=1/10,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v7, col="turquoise")
v8 = vaccinate(beta=1/.24,gamma=1/5,nu=1/15,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v8, col="blue")
v9 = vaccinate(beta=1/.24,gamma=1/5,nu=1/20,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v9, col="purple")
v10 = vaccinate(beta=1/.24,gamma=1/5,nu=1/25,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v10,col="violet")
v11 = vaccinate(beta=1/.24,gamma=1/5,nu=1/30,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v11,col="black")
v12 = vaccinate(beta=1/.24,gamma=1/5,nu=1/35,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v12,col="gray")
v13 = vaccinate(beta=1/.24,gamma=1/5,nu=1/40,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v13,col="slategray")
v14 = vaccinate(beta=1/.24,gamma=1/5,nu=1/45,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v14,col="firebrick")
v15 = vaccinate(beta=1/.24,gamma=1/5,nu=1/50,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v15,col="chartreuse")
v16 = vaccinate(beta=1/.24,gamma=1/5,nu=1/60,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v16,col="cadetblue1")
points(test,type='o',lwd=5)
title(main=expression(paste("Vaccination Rate vs Epidemic Size, ", 
                            alpha,"=1-60, ",R0,"=1.2, ",gamma, "=5")),
      xlab="# vaccinated", ylab="Epidemic Size")


#################################################
#B=4, G=1/2: Vaccination Rate vs Epidemic Size#
#################################################
S_0 = 999
I_0 = 1       
R_0 = 0       
V_0 = 0
npop = S_0+I_0+R_0+V_0
vt = seq(0,100,1)  
beta  = 4
gamma = 1/2 
#pt1
nu    = 0
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
detv1 = as.data.frame(ode(inits, vt, SIRVfunc, vparameters, method="lsoda"))
IV1 <- as.vector(aggregate(I ~ V, detv1, max), mode='numeric')
#pt2
nu    = 1
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
detv2 = as.data.frame(ode(inits, vt, SIRVfunc, vparameters, method="lsoda"))
IV2 <- as.vector(aggregate(I ~ V, detv2, max), mode='numeric')
test <- rbind(IV2, as.vector(IV2[which(IV2$I == max(IV2$I)),], mode='numeric'))
#pt3
nu    = 1/10
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv3 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV3 <- aggregate(I ~ V, detv3, max)
test <- rbind(IV1, as.vector(IV3[which(IV3$I == max(IV3$I)),], mode='numeric'))
#pt5
nu    = 1/30
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv5 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV5 <- aggregate(I ~ V, detv5, max)
test <- rbind(test, as.vector(IV5[which(IV5$I == max(IV5$I)),], mode='numeric'))
#pt6
nu    = 1/40
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv6 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV6 <- aggregate(I ~ V, detv6, max)
test <- rbind(test, as.vector(IV6[which(IV6$I == max(IV6$I)),], mode='numeric'))
#pt7
nu    = 1/50
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv7 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV7 <- aggregate(I ~ V, detv7, max)
test <- rbind(test, as.vector(IV7[which(IV7$I == max(IV7$I)),], mode='numeric'))
#pt8
nu    = 1/60
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv8 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV8 <- aggregate(I ~ V, detv8, max)
test <- rbind(test, as.vector(IV8[which(IV8$I == max(IV8$I)),], mode='numeric'))
test <- test[order(test[,1],test[,2],decreasing=FALSE),]
##############################################################################################
v1 = vaccinate(beta=4,gamma=1/2,nu=1,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
plot(v1,xlab='',ylab='',col="red")
v2 = vaccinate(beta=4,gamma=1/2,nu=1/2,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v2, col="darkorange")
v3 = vaccinate(beta=4,gamma=1/2,nu=1/3,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v3, col="gold")
v4 = vaccinate(beta=4,gamma=1/2,nu=1/4,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v4, col="hotpink")
v5 = vaccinate(beta=4,gamma=1/2,nu=1/5,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v5, col="green")
v6 = vaccinate(beta=4,gamma=1/2,nu=1/7,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v6, col="seagreen")
v7 = vaccinate(beta=4,gamma=1/2,nu=1/10,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v7, col="turquoise")
v8 = vaccinate(beta=4,gamma=1/2,nu=1/15,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v8, col="blue")
v9 = vaccinate(beta=4,gamma=1/2,nu=1/20,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v9, col="purple")
v10 = vaccinate(beta=4,gamma=1/2,nu=1/25,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v10,col="violet")
v11 = vaccinate(beta=4,gamma=1/2,nu=1/30,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v11,col="black")
v12 = vaccinate(beta=4,gamma=1/2,nu=1/35,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v12,col="gray")
v13 = vaccinate(beta=4,gamma=1/2,nu=1/40,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v13,col="slategray")
v14 = vaccinate(beta=4,gamma=1/2,nu=1/45,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v14,col="firebrick")
v15 = vaccinate(beta=4,gamma=1/2,nu=1/50,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v15,col="chartreuse")
v16 = vaccinate(beta=4,gamma=1/2,nu=1/60,S0=999,I0=1,R0=0,V0=0,tau0=0,maxtime=3000,nsims=1000)
points(v16,col="cadetblue1")
#points(test,type='o',lwd=5)
title(main=expression(paste("Vaccination Rate vs Epidemic Size, ", 
                            alpha,"=1-60, ",beta,"=1, ",gamma, "=2")),
      xlab="# vaccinated", ylab="Epidemic Size")

############################################################
#B=1/3, G=1/5: Vaccination Rate vs Epidemic Size Stochastic#
############################################################

v1 = vaccinate(beta=1/3,gamma=1/5,nu=1/1,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
plot(v1, xlab='',ylab='',col="red")
v2 = vaccinate(beta=1/3,gamma=1/5,nu=1/2,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v2, col="darkorange")
v3 = vaccinate(beta=1/3,gamma=1/5,nu=1/3,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v3, col="gold")
v4 = vaccinate(beta=1/3,gamma=1/5,nu=1/4,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v4, col="hotpink")
v5 = vaccinate(beta=1/3,gamma=1/5,nu=1/5,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v5, col="green")
v6 = vaccinate(beta=1/3,gamma=1/5,nu=1/7,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v6, col="seagreen")
v7 = vaccinate(beta=1/3,gamma=1/5,nu=1/10,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v7, col="turquoise")
v8 = vaccinate(beta=1/3,gamma=1/5,nu=1/15,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v8, col="blue")
v9 = vaccinate(beta=1/3,gamma=1/5,nu=1/20,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v9, col="purple")
v10 = vaccinate(beta=1/3,gamma=1/5,nu=1/25,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v10,col="violet")
v11 = vaccinate(beta=1/3,gamma=1/5,nu=1/30,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v11,col="black")
v12 = vaccinate(beta=1/3,gamma=1/5,nu=1/35,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v12,col="gray")
v13 = vaccinate(beta=1/3,gamma=1/5,nu=1/40,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v13,col="slategray")
v14 = vaccinate(beta=1/3,gamma=1/5,nu=1/45,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v14,col="firebrick")
v15 = vaccinate(beta=1/3,gamma=1/5,nu=1/50,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v15,col="chartreuse")
v16 = vaccinate(beta=1/3,gamma=1/5,nu=1/60,S0=999,I0=1,R0=0,V0=1,tau0=0,maxtime=3000,nsims=1000)
points(v16,col="cadetblue1")


title(main=expression(paste("Vaccination Rate vs Epidemic Size, ", 
                            alpha,"=1-60, ",beta,"=3, ",gamma, "=5")),
      xlab="# vaccinated", ylab="Epidemic Size")


