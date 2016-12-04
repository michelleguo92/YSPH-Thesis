#Parameters####################################################
S0 = 999
I0 = 1       
R0 = 0    
tau0 = 0
npop = S0+I0+R0+V0
vt = seq(0,30000,1)  
maxtime = 30000
nsims = 1000
closeeff = 0.8
closureduration = 7
kappa = 0
#1.1###########################################################
beta  = 0.6
gamma = 1/2 
parms  = c(gamma=gamma,beta=beta,kappa=kappa)
inits = c(S=S0,I=I0,R=R0)
closedur = 70
closure = as.data.frame(lsoda(inits, vt, SIRclosefunc, parms))
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="red")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="red")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="red")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="red")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="red")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="red")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="red")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="red")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="red")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="red")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=1.2, ", gamma,"=2")), 
      xlab="Closure Threshold", ylab="Epidemic Size")

#1.2###########################################################
beta  = 0.24
gamma = 1/5 
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="darkorange")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="darkorange")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="darkorange")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="darkorange")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="darkorange")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="darkorange")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="darkorange")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="darkorange")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="darkorange")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="darkorange")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=1.2, ", gamma,"=5")),
      xlab="Closure Threshold", ylab="Epidemic Size")

#1.3###########################################################
beta  = 0.15
gamma = 1/8 
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="seagreen")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="seagreen")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="seagreen")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="seagreen")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="seagreen")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="seagreen")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="seagreen")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="seagreen")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="seagreen")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="seagreen")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=1.2, ", gamma,"=8")),
      xlab="Closure Threshold", ylab="Epidemic Size")

#2.1###########################################################
beta  = 0.8
gamma = 1/2 
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="blue")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="blue")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="blue")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="blue")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="blue")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="blue")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="blue")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="blue")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="blue")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="blue")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=1.6, ", gamma,"=2")),
      xlab="Closure Threshold", ylab="Epidemic Size")

#2.2###########################################################
beta  = 0.32
gamma = 1/5 
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="purple")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="purple")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="purple")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="purple")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="purple")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="purple")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="purple")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="purple")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="purple")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="purple")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=1.6, ", gamma,"=5")),
      xlab="Closure Threshold", ylab="Epidemic Size")

#2.3#########################################################
beta  = 0.2
gamma = 1/8 
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="brown")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="brown")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="brown")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="brown")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="brown")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="brown")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="brown")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="brown")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="brown")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="brown")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=1.6, ", gamma,"=8")),
      xlab="Closure Threshold", ylab="Epidemic Size")

#3.1#########################################################
beta  = 1.0
gamma = 1/2 
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="hotpink")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="hotpink")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="hotpink")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="hotpink")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="hotpink")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="hotpink")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="hotpink")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="hotpink")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="hotpink")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="hotpink")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=2.0, ", gamma,"=2")),
      xlab="Closure Threshold", ylab="Epidemic Size")

#3.2#########################################################
beta  = 0.4
gamma = 1/5 
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="maroon")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="maroon")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="maroon")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="maroon")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="maroon")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="maroon")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="maroon")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="maroon")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="maroon")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="maroon")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=2.0, ", gamma,"=5")),
      xlab="Closure Threshold", ylab="Epidemic Size")

#3.3#########################################################
beta  = 0.25
gamma = 1/8
t1 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=0,closureduration)
plot(t1,xlim=c(0,80),ylim=c(0,1000),xlab='',ylab='',col="dimgray")
t2 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=5,closureduration)
points(t2, col="dimgray")
t3 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=10,closureduration)
points(t3, col="dimgray")
t4 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=20,closureduration)
points(t4, col="dimgray")
t5 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=30,closureduration)
points(t5, col="dimgray")
t6 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=40,closureduration)
points(t6, col="dimgray")
t7 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=50,closureduration)
points(t7, col="dimgray")
t8 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=60,closureduration)
points(t8, col="dimgray")
t9 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=70,closureduration)
points(t9, col="dimgray")
t0 = schoolclosure(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,
                   closeeff,closurethresh=80,closureduration)
points(t0, col="dimgray")
title(main=expression(paste("Epidemic Size with School Closing, ",R0,"=2.0, ", gamma,"=8")),
      xlab="Closure Threshold", ylab="Epidemic Size")
