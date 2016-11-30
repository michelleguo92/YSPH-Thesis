S_0 = 999
I_0 = 1       
R_0 = 0       
V_0 = 0
npop = S_0+I_0+R_0+V_0
vt = seq(0,100,1)  
beta  = 0.8
gamma = 1/2 
vt = seq(0,3000,1)
#pt1
nu    = 0
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
detv1 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV1 = matrix(NA,nrow=1,ncol=2)
IV1[,1] = max(detv1$V)
IV1[,2] = max(detv1$R)
#pt2
nu    = 0.01
vparameters = c(gamma=gamma,beta=beta,nu=nu)
inits = c(S=S_0,I=I_0,R=R_0,V=V_0)
detv2 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV2 = matrix(NA,nrow=1,ncol=2)
IV2[,1] = max(detv2$V)
IV2[,2] = max(detv2$R)
test <- rbind(IV1, IV2)
#pt3
nu    = 0.02
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv3 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV3 = matrix(NA,nrow=1,ncol=2)
IV3[,1] = max(detv3$V)
IV3[,2] = max(detv3$R)
test <- rbind(test,IV3)
#pt4
nu    = 0.15
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv4 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV4 = matrix(NA,nrow=1,ncol=2)
IV4[,1] = max(detv4$V)
IV4[,2] = max(detv4$R)
test <- rbind(test,IV4)
#pt5
nu    = 0.20
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv5 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV5 = matrix(NA,nrow=1,ncol=2)
IV5[,1] = max(detv5$V)
IV5[,2] = max(detv5$R)
test <- rbind(test,IV5)
#pt6
nu    = 0.25
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv6 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV6 = matrix(NA,nrow=1,ncol=2)
IV6[,1] = max(detv6$V)
IV6[,2] = max(detv6$R)
test <- rbind(test,IV6)
#pt7
nu    = 0.50
vparameters = c(gamma=gamma,beta=beta,nu=nu)
detv7 = as.data.frame(lsoda(inits, vt, SIRVfunc, vparameters))
IV7 = matrix(NA,nrow=1,ncol=2)
IV7[,1] = max(detv7$V)
IV7[,2] = max(detv7$R)
test <- rbind(test,IV7)
#############################################################
par(mfrow=c(1,1))
v1 = vaccinate(beta,gamma,nu=0.50,S0,I0,R0,V0=0,tau0,maxtime,nsims)
plot(v1,xlim=c(0,1000),ylim=c(0,1000),xlab='',ylab='',col="red")
v2 = vaccinate(beta,gamma,nu=0.15,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v2, col="darkorange")
v3 = vaccinate(beta,gamma,nu=0.10,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v3, col="gold")
v4 = vaccinate(beta,gamma,nu=0.05,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v4, col="hotpink")
v5 = vaccinate(beta,gamma,nu=0.02,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v5, col="green")
v6 = vaccinate(beta,gamma,nu=0.009,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v6, col="seagreen")
v7 = vaccinate(beta,gamma,nu=0.005,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v7, col="turquoise")
v8 = vaccinate(beta,gamma,nu=0.003,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v8, col="blue")
v9 = vaccinate(beta,gamma,nu=0.001,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v9, col="purple")
v0 = vaccinate(beta,gamma,nu=0,S0,I0,R0,V0=0,tau0,maxtime,nsims)
points(v0, col="black")
points(test,type='o',lwd=3)
title(main=expression(paste("Vaccination Rate vs Epidemic Size, ", 
                            alpha,"=0-0.5, ",R0,"=1.6, ",gamma, "=1/2")),
      xlab="# vaccinated", ylab="Epidemic Size")
