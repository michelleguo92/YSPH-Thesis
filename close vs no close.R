#1.1###########################################################
par(mfrow=c(1,2))
schoolclosure(0.60,1/2,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=1.2, ", gamma,"=2")))
infectedplot(0.60,1/2,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=1.2, ", gamma,"=2")))
#1.2###########################################################
par(mfrow=c(1,2))
schoolclosure(0.24,1/5,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=1.2, ", gamma,"=5")))
infectedplot(0.24,1/5,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=1.2, ", gamma,"=5")))
#1.3###########################################################
par(mfrow=c(1,2))
schoolclosure(0.15,1/8,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=1.2, ", gamma,"=8")))
infectedplot(0.15,1/8,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=1.2, ", gamma,"=8")))
#2.1###########################################################
par(mfrow=c(1,2))
schoolclosure(0.80,1/2,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=1.6, ", gamma,"=2")))
infectedplot(0.80,1/2,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=1.6, ", gamma,"=2")))
#2.2###########################################################
par(mfrow=c(1,2))
schoolclosure(0.32,1/5,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=1.6, ", gamma,"=5")))
infectedplot(0.32,1/5,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=1.6, ", gamma,"=5")))
#2.3#########################################################
par(mfrow=c(1,2))
schoolclosure(0.20,1/8,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=1.6, ", gamma,"=8")))
infectedplot(0.20,1/8,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=1.6, ", gamma,"=8")))
#3.1#########################################################
par(mfrow=c(1,2))
schoolclosure(1.00,1/2,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=2.0, ", gamma,"=2")))
infectedplot(1.00,1/2,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=2.0, ", gamma,"=2")))
#3.2#########################################################
par(mfrow=c(1,2))
schoolclosure(0.40,1/5,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=2.0, ", gamma,"=5")))
infectedplot(0.40,1/5,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=2.0, ", gamma,"=5")))
#3.3#########################################################
par(mfrow=c(1,2))
schoolclosure(0.25,1/8,999,1,0,0,3000,1000,0.8,20,4)
title(main=expression(paste("With Closing, ", 
                            R0,"=2.0, ", gamma,"=8")))
infectedplot(0.25,1/8,999,1,0,0,3000,1000)
title(main=expression(paste("Without Closing, ", 
                            R0,"=2.0, ", gamma,"=8")))

