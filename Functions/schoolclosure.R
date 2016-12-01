schoolclosure <- function(beta,gamma,S0,I0,R0,tau0,maxtime,nsims,closeeff,closurethresh,closureduration){
  N <- S0 + I0 + R0
  i <- 1 #time step during replication
  j <- 1 #replication index
  #simulate data
  
  sims = list()
  infected <- list()
  nrec <- matrix(data=NA, nrow=nsims, ncol=maxtime)
  maxtau <- matrix(data=NA, nrow=nsims, ncol=maxtime)
  #Run gillespie simulation with above parameters
  
  for (j in 1:nsims) {
    print(j)
    i = 1
    t = 0
    dt = 0
    tempeff = 0
    closed = 0
    tempduration = 0
    durationclosed = 0
    
    # Moved output initialization here. You were initially setting it elsewhere, but this placement makes more sense
    output <- matrix(NA,nrow=maxtime-1,ncol=4)
    output <- rbind(c(S0,I0,R0,tau0),output)
    while (t<maxtime && output[i,2]>0 && output[i,1]>0) { 
      rho = ((1-tempeff)*(beta*output[i,1]*output[i,2]))/N      #time to next infection
      mu = gamma*output[i,2]                                    #time to next recovery                
      lambda = (rho + mu)                                       #UPDATE WHEN YOU ADD NEW COMPARTMENT
      p1 = rho/lambda
      p2 = mu/lambda
      u1 = runif(1)
      u2 = runif(1)
      dt = -(1/lambda)*log(1 - u1) #update tau
      if (u2 < p1){
        output[i+1,1] = output[i,1] - 1
        output[i+1,2] = output[i,2] + 1
        output[i+1,3] = output[i,3]
      } else {
        output[i+1,1] = output[i,1]
        output[i+1,2] = output[i,2] - 1
        output[i+1,3] = output[i,3] + 1
      } 
      t = t + dt
      output[i+1,4] = t
      i = i + 1
      if (closed == 0 && output[i,2] > closurethresh) {
        closed = 1
        tempeff = closeeff #update this if count for I exceeds threshold
      }
      else if (closed == 1 && durationclosed >= closureduration) {
        closed = 0
        tempeff = 0
        durationclosed = 0
      }
      if (closed == 1) {
        durationclosed = durationclosed + 1
      }
    }
    # Moved sims here so that you didn't have to insert output into sims
    sims[[j]] = output[]
  }
  for (i in 1:nsims) {
    nrec[i,] = sims[[i]][,3] 
  }
  
  for (i in 1:nsims) {
    maxtau[i,] = max(sims[[i]][,4],na.rm=TRUE)
  }
  
  maxt = max(maxtau)
  
  plot(1,type="n",xlab="Time",ylab="Total infected",
       main=NULL,xlim=c(0, 500), 
       ylim=c(0, 1000))
  
  for (i in 1:nsims) {
    lines(sims[[i]][,4], sims[[i]][,3], col=i)
  }
}