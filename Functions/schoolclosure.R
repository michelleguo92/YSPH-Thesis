vaccinate <- function(beta,gamma,nu,S0,I0,R0,V0,tau0,maxtime,nsims,closureeff,closurethresh,closureduration){
  N <- S0 + I0 + R0 + V0
  i <- 1 #time step during replication
  j <- 1 #replication index
  #simulate data
  
  output <- matrix(NA,nrow=maxtime-1,ncol=5)
  output <- rbind(c(S0,I0,R0,V0,tau0),output)
  sims = list()
  infected <- list()
  nrec <- matrix(data=NA, nrow=nsims, ncol=maxtime)
  nvax <- matrix(data=NA, nrow=nsims, ncol=maxtime)
  maxrec <- array(data=NA, dim=nsims)
  maxvax <- array(data=NA, dim=nsims)
  #Run gillespie simulation with above parameters
  
  for (j in 1:nsims) {
    i = 1
    t = 0
    dt = 0
    tempclosureeff = 0
    closed = 0
    while (t<maxtime && output[i,2]>0 && output[i,1]>0) { 
      rho = (beta*output[i,1]*output[i,2])/N #time to next infection
      mu = (1-tempclosureeff)*gamma*output[i,2]                 #time to next recovery
      alpha = nu*output[i,1]                 #time to next vaccination 
      lambda = (rho + mu + alpha)            #UPDATE WHEN YOU ADD NEW COMPARTMENT
      p1 = rho/lambda
      p2 = mu/lambda
      u1 = runif(1)
      u2 = runif(1)
      dt = -(1/lambda)*log(1 - u1) #update tau
      if (u2 < p1){
        output[i+1,1] = output[i,1] - 1
        output[i+1,2] = output[i,2] + 1
        output[i+1,3] = output[i,3]
        output[i+1,4] = output[i,4]
      } else if (u2 >= p1 && u2 < p1+p2){
        output[i+1,1] = output[i,1]
        output[i+1,2] = output[i,2] - 1
        output[i+1,3] = output[i,3] + 1
        output[i+1,4] = output[i,4]
      } else if (u2 >= p1+p2){
        output[i+1,1] = output[i,1] - 1
        output[i+1,2] = output[i,2] 
        output[i+1,3] = output[i,3] 
        output[i+1,4] = output[i,4] + 1
      }
      t = t + dt
      output[i+1,5] = t
      i = i + 1
      if (closed = 0 && output[i,2] > closurethresh) {
        ifclosed = 1
        tempclosureeff = closureeff #update this if count for I exceeds threshold
      }
      if (closed = 1 && tempduration >= closureduration) {
        ifclosed = 0
        tempclosureeff = 0
      }
    }
    sims[[j]] = output[]
    output <- matrix(NA,nrow=maxtime-1,ncol=5)
    output <- rbind(c(S0,I0,R0,V0,tau0),output)
  }
  for (i in 1:nsims) {
    nrec[i,] = sims[[i]][,3]
  }
  
  for (i in 1:nsims) {
    nvax[i,] = sims[[i]][,4]
  }
  result <- matrix(NA,nrow=nsims,ncol=2)
  maxrec <- apply(nrec,1,max,na.rm=TRUE)
  maxvax <- apply(nvax,1,max,na.rm=TRUE)
  result[,1] <- maxvax
  result[,2] <- maxrec
  #
  return(result)
  
}