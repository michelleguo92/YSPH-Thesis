histogramplot <- function(beta,gamma,S0,I0,R0,tau0,maxtime,nsims){
  N <- S0 + I0 + R0
  i <- 1 #time step during replication
  j <- 1 #replication index
  #simulate data
  
  output <- matrix(NA,nrow=maxtime-1,ncol=4)
  output <- rbind(c(S0,I0,R0,tau0),output)
  sims = list()
  infected <- list()
  ninf <- matrix(data=NA, nrow=nsims, ncol=maxtime)
  nrec <- matrix(data=NA, nrow=nsims, ncol=maxtime)
  maxtau <- matrix(data=NA, nrow=nsims, ncol=maxtime)
  maxinf <- array(data=NA, dim=nsims)
  maxrec <- array(data=NA, dim=nsims)
  #Run gillespie simulation with above parameters
  
  for (j in 1:nsims) {
    i = 1
    t = 0
    dt = 0
    while (t<maxtime && output[i,2]>0) { 
      rho = (beta*output[i,2]*output[i,1])/N #time to next infection
      mu = gamma*output[i,2]                 #time to next recovery
      lambda = (rho + mu)            #UPDATE WHEN YOU ADD NEW COMPARTMENT
      p1 = rho/lambda
      p2 = mu/lambda
      u1 = runif(1)
      u2 = runif(1)
      dt = -(1/lambda)*log(1 - u1) #update tau
      if (u2 < p1){
        output[i+1,1] = output[i,1] - 1
        output[i+1,2] = output[i,2] + 1
        output[i+1,3] = output[i,3]
      } else{
        output[i+1,1] = output[i,1]
        output[i+1,2] = output[i,2] - 1
        output[i+1,3] = output[i,3] + 1
      } 
      t = t + dt
      output[i+1,4] = t
      i = i + 1
    }
    sims[[j]] = output[]
    output <- matrix(NA,nrow=maxtime-1,ncol=4)
    output <- rbind(c(S0,I0,R0,tau0),output)
  }
  ##PLOT ALL INFECTED
  #make matrix with only infected for all simulations --> WORKS
  for (i in 1:nsims) {
    ninf[i,] = sims[[i]][,2]
  }
  
  for (i in 1:nsims) {
    nrec[i,] = sims[[i]][,3]
  }
  
  for (i in 1:nsims) {
    maxtau[i,] = max(sims[[i]][,4],na.rm=TRUE)
  }
  
  maxt <- max(maxtau)
  maxinf <- apply(ninf,1,max,na.rm=TRUE)
  maxrec <- apply(nrec,1,max,na.rm=TRUE)
    
  #histogram of total infected --> WORKS
  #hist(maxinf,xaxt='n',xlim=c(0,500),breaks=20,main=NULL,xlab=NULL,ylab=NULL)
  hist(maxrec,xaxt='n',xlim=c(0,1000),breaks=20,main=NULL,xlab=NULL,ylab=NULL)
  axis(1, at = seq(0, 1000, 50)) 
  
}