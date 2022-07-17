setwd('C:/Users/mihal/OneDrive')

library(R2WinBUGS)
library(BRugs)


data2 <- read.table('C:/Users/mihal/OneDrive/2017_datadata.txt',sep="") # Read in the data in a rectangular format
data <- data.frame(ht=data2[,1],at=data2[,2],goals1=data2[,3],goals2=data2[,4]) # Create a df
n <- nrow(data) # Define the sample size
inits <- list ( list ( mu = 0.5, home = 0.5, a = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       d = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) ) )  # Set up the initial values
attach(data)
data.names <- c(names(data), 'n', 'K') # Defining the names of the data objects
K <- 20
parameter.names <- c('mu','home','a','d') # Defining the names of the parameters we wish to monitor
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323"

model1.sim <- bugs( data.names, inits, model.file = "Vanilla_model.txt", parameters = parameter.names,
                    n.chains = 1, n.iter = 1000, n.burnin=500, n.thin=1,  bugs.directory = openbugs.dir, debug=T, 
                    program="OpenBUGS") # Generating random values using OpenBUGS

# Posterior summaries
print(model1.sim,3) 
names(model1.sim) # Attributes of the model1.sim




#
# Function for creating trace and ergodic mean plots
# 2013 April by Ioannis Ntzoufras
#

plot.trace <- function( bugs.object, nrow=5, ncol=NULL, ergodic=FALSE){ 
  mcmc.output<-bugs.object$sims.matrix 
  n.iter <- nrow(mcmc.output)
  n.par  <- ncol(mcmc.output)
  if (is.null(ncol)) ncol <- (n.par %/% nrow)+1*( (n.par %% nrow)!=0 )
  
  par(mfcol=c(nrow,ncol) )
  if (ergodic){ 
    for (k in 1:n.par){ 
      plot( cumsum(mcmc.output[,k])/1:n.iter, type='l', main=colnames(mcmc.output)[k]) }
    
  }else{
    for (k in 1:n.par){ plot( mcmc.output[,k], type='l', main=colnames(mcmc.output)[k]) }
  }
}

par(mfrow = c(3,1))

# Trace plots - in a window with 1 row and 1 column
plot.trace( model1.sim,1,1) # All values are within a zone without strong periodicities and especially 
# tendecies, so we can assume convergence

# Ergodic mean plots
plot.trace( model1.sim,1,1, ergodic=T) # Stabilized ergodic mean thus it's an indicaton of the 
# convergence of the algorithm ( it seems to have converged after about 90 iterations )







# --------------------------------
# Plots without functions
# -------------------------------- 

# check the components of model1.sim 
names(model1.sim)

# see the first lines of model1.sim$sims.matrix
head(model1.sim$sims.matrix)

# Histogram of "mu"
hist(model1.sim$sims.matrix[,1]) 

# Histogram of "home"
hist(model1.sim$sims.matrix[,2])


# Histogram of "a[1]"
hist(model1.sim$sims.matrix[,3]) 

# Smoothed density plot of the posterior of "mu"
plot(density(model1.sim$sims.matrix[,1]))

# Smoothed density plot of the posterior of "home"
plot(density(model1.sim$sims.matrix[,2]))

# Smoothed density plot of the posterior of "a[1]"
plot(density(model1.sim$sims.matrix[,3]), main = "Smoothed density plot of the posterior of the attacking abilities of node 1")

# Trace plot of "mu"
plot(model1.sim$sims.matrix[,1],type='l')

# Trace plot of "home"
plot(model1.sim$sims.matrix[,2],type='l')

# Trace plot of "a[1]"
plot(model1.sim$sims.matrix[,3],type='l')

par(mfrow = c(3,1))
# Ergodic Trace plot of "mu"
x <- model1.sim$sims.matrix[,1]
plot( cumsum(x)/1:length(x), type='l',main = "Ergodic Trace Plot of 'mu'") # The algorithm seems to have converged

# Ergodic Trace plot of "home"
x <- model1.sim$sims.matrix[,2]
plot( cumsum(x)/1:length(x), type='l', main = "Ergodic Trace Plot of 'home'") # The algorithm seems to have converged 


# Ergodic Trace plot of "a[1]"
x <- model1.sim$sims.matrix[,3]
plot( cumsum(x)/1:length(x), type='l',main = "Ergodic Trace Plot of 'a[1]'") # The algorithm seems to have converged

# Autocorrelation of "mu"
acf(model1.sim$sims.matrix[,1]) # we observe that there are not significant autocorrelations!


# Autocorrelation of "home"
acf(model1.sim$sims.matrix[,2]) # we observe that there are not significant autocorrelations!


# Autocorrelation of "a[1]"
acf(model1.sim$sims.matrix[,3]) # we observe that there are not significant autocorrelations!


#
# Function for checking the centrality of zero
# 2013 April by Ioannis Ntzoufras
# 

p0 <- function( bugs.object, digits=3){ 
  mcmc.output <- bugs.object$sims.matrix 
  n.iter <- nrow(mcmc.output)
  n.par  <- ncol(mcmc.output)
  mcmc.output <- mcmc.output[ , -n.par] 
  temp <- apply( mcmc.output < 0, 2, mean)
  res <- pmin( temp, 1-temp)
  return( round(res,digits) )
}


# Calculating the probability of zero to be central in the posterior densities
p0(model1.sim)



par(mfrow = c(3,3)) # For multiple visualization
# Second way to get the Ergodic mean plots
a <- model1.sim$sims.matrix[,1]
b <- model1.sim$sims.matrix[,2]
c <- model1.sim$sims.matrix[,3]
d <- model1.sim$sims.matrix[,4]
e <- model1.sim$sims.matrix[,5]
f <- model1.sim$sims.matrix[,6]
g <- model1.sim$sims.matrix[,7]
h <- model1.sim$sims.matrix[,8]
i <- model1.sim$sims.matrix[,9]

Iterations <- 1:length(a)
plot(Iterations, cumsum(a)/Iterations, xlab="Iterations", ylab='a', type='l', main = 'Ergodic Mean Plot of "mu"') # The algorithm seems to have converged after about 120 iterations
plot(Iterations, cumsum(b)/Iterations, xlab="Iterations", ylab='b', type='l', main = 'Ergodic Mean Plot of "home"') # The algorithm seems to have converged after about 120 iterations
plot(Iterations, cumsum(c)/Iterations, xlab="Iterations", ylab='c', type='l', main = 'Ergodic Mean Plot of "a[1]"') # The algorithm seems to have converged after about 80 iterations
plot(Iterations, cumsum(d)/Iterations, xlab="Iterations", ylab='c', type='l', main = 'Ergodic Mean Plot of "a[2]"') # The algorithm seems to have converged after about 80 iterations
plot(Iterations, cumsum(e)/Iterations, xlab="Iterations", ylab='c', type='l', main = 'Ergodic Mean Plot of "a[3]"') # The algorithm seems to have converged after about 120 iterations
plot(Iterations, cumsum(f)/Iterations, xlab="Iterations", ylab='c', type='l', main = 'Ergodic Mean Plot of "a[4]"') # The algorithm seems to have converged after about 80 iterations
plot(Iterations, cumsum(g)/Iterations, xlab="Iterations", ylab='c', type='l', main = 'Ergodic Mean Plot of "a[5]"') # The algorithm seems to have converged after about 60 iterations
plot(Iterations, cumsum(h)/Iterations, xlab="Iterations", ylab='c', type='l', main = 'Ergodic Mean Plot of "a[6]"') # The algorithm seems to have converged after about 40 iterations
plot(Iterations, cumsum(i)/Iterations, xlab="Iterations", ylab='c', type='l', main = 'Ergodic Mean Plot of "a[7]"') # The algorithm seems to have converged after about 100 iterations








# Negative Binomial model
data2 <- read.table('C:/Users/mihal/OneDrive/2017_datadata.txt',sep="") # Read in the data in a rectangular format
data <- data.frame(ht=data2[,1],at=data2[,2],goals1=data2[,3],goals2=data2[,4]) # Create a df
n <- nrow(data) # Define the sample size
inits <- list ( list ( mu = 0.5, home = 0.5, a = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       d = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), r=1 ) )  # Set up the initial values
attach(data)
data.names <- c(names(data), 'n', 'K') # Defining the names of the data objects
K <- 20
parameter.names <- c('mu','home','a','d') # Defining the names of the parameters we wish to monitor
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323"




model2.sim <- bugs( data.names, inits, model.file = "NB_model.txt", parameters = parameter.names,
                    n.chains = 1, n.iter = 1000, n.burnin=500, n.thin=1,  bugs.directory = openbugs.dir, debug=T, 
                    program="OpenBUGS") # Generating random values using OpenBUGS

# Posterior summaries
print(model2.sim,3) # DIC value 2176
names(model2.sim) # Attributes of the model2.sim






# Interpreting the Vanilla model
mu <- 0.01716
home <- 0.2852
exp(mu+home)
exp(mu)









# Regeneration of the League using the Vanilla model
data2 <- read.table('C:/Users/mihal/OneDrive/2017_datadata.txt',sep="") # Read in the data in a rectangular format
data <- data.frame(ht=data2[,1],at=data2[,2],goals1=data2[,3],goals2=data2[,4]) # Create a df
n <- nrow(data) # Define the sample size
inits <- list ( list ( mu = 0.5, home = 0.5, a = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       d = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), r=1 ) )  # Set up the initial values
attach(data)
data.names <- c(names(data), 'n', 'K') # Defining the names of the data objects
K <- 20
parameter.names <- c('mu','home','a','d','total.points','rank.probs','ranks') # Defining the names of the parameters we wish to monitor
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323"




model3.sim <- bugs( data.names, inits, model.file = "Regen_model_UK_2017.txt", parameters = parameter.names,
                    n.chains = 1, n.iter = 6000, n.burnin=1000, n.thin=1,  bugs.directory = openbugs.dir, debug=T, 
                    program="OpenBUGS") # Generating random values using OpenBUGS

# Posterior summaries
print(model3.sim,3) # DIC value 2184
names(model3.sim) # Attributes of the model3.sim

rank(model3.sim$median$a) # Node 11 ( Manchester City has the highest attacking abilities as expected )


total_points <- model3.sim$mean$total.points
round(total_points)

nodes_points_mat <- matrix(c(1:20,round(total_points)), nrow = 2,ncol=20,byrow =T)
nodes_points_mat
colnames(nodes_points_mat) <- c("Arsenal","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
"Huddersfield","Leicester","Liverpool","Manchester City","Manchester United","Newcastle","Southampton","Stoke City",
"Swansea City","Tottenham","Watford","West Brom","West Ham")
row.names(nodes_points_mat) <- c("Node","Points")
mat <- as.matrix(sort(nodes_points_mat[2,],decreasing = T))
mat
colnames(mat) <- c("Points")
mat




# Random Effects model 1
data2 <- read.table('C:/Users/mihal/OneDrive/2017_datadata.txt',sep="") # Read in the data in a rectangular format
data <- data.frame(ht=data2[,1],at=data2[,2],goals1=data2[,3],goals2=data2[,4]) # Create a df
n <- nrow(data) # Define the sample size
inits <- list ( list ( mu = 0.5, home = 0.5, a = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       d = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), tau=1 ) )  # Set up the initial values
attach(data)
data.names <- c(names(data), 'n', 'K') # Defining the names of the data objects
K <- 20
parameter.names <- c('mu','home','a','d') # Defining the names of the parameters we wish to monitor
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323"


model4.sim <- bugs( data.names, inits, model.file = "DP_RE_model.txt", parameters = parameter.names,
                    n.chains = 1, n.iter = 1000, n.burnin=500, n.thin=1,  bugs.directory = openbugs.dir, debug=T, 
                    program="OpenBUGS") # Generating random values using OpenBUGS

# Posterior summaries
print(model4.sim,3) # DIC value 2189
names(model4.sim) # Attributes of the model4.sim









# Random Effects model 2
data2 <- read.table('C:/Users/mihal/OneDrive/2017_datadata.txt',sep="") # Read in the data in a rectangular format
data <- data.frame(ht=data2[,1],at=data2[,2],goals1=data2[,3],goals2=data2[,4]) # Create a df
n <- nrow(data) # Define the sample size
inits <- list ( list ( mu = 0.5, home = 0.5, a = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       d = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), tau1=1,tau2=1 ) )  # Set up the initial values
attach(data)
data.names <- c(names(data), 'n', 'K') # Defining the names of the data objects
K <- 20
parameter.names <- c('mu','home','a','d') # Defining the names of the parameters we wish to monitor
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323"


model5.sim <- bugs( data.names, inits, model.file = "DP_RE_2_model.txt", parameters = parameter.names,
                    n.chains = 1, n.iter = 1000, n.burnin=500, n.thin=1,  bugs.directory = openbugs.dir, debug=T, 
                    program="OpenBUGS") # Generating random values using OpenBUGS

# Posterior summaries
print(model5.sim,3) # DIC value 2184
names(model5.sim) # Attributes of the model5.sim













# Random Effects model 3
data2 <- read.table('C:/Users/mihal/OneDrive/2017_datadata.txt',sep="") # Read in the data in a rectangular format
data <- data.frame(ht=data2[,1],at=data2[,2],goals1=data2[,3],goals2=data2[,4]) # Create a df
n <- nrow(data) # Define the sample size
inits <- list ( list ( mu = 0.5, home = 0.5, a = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       d = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), tau1=1,tau2=1 ) )  # Set up the initial values
attach(data)
data.names <- c(names(data), 'n', 'K') # Defining the names of the data objects
K <- 20
parameter.names <- c('mu','home','a','d') # Defining the names of the parameters we wish to monitor
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323"


model6.sim <- bugs( data.names, inits, model.file = "DP_RE_3_model.txt", parameters = parameter.names,
                    n.chains = 1, n.iter = 1000, n.burnin=500, n.thin=1,  bugs.directory = openbugs.dir, debug=T, 
                    program="OpenBUGS") # Generating random values using OpenBUGS

# Posterior summaries
print(model6.sim,3) # DIC value 2189
names(model6.sim) # Attributes of the model6.sim














# Regeneration of the League using the NB model
data2 <- read.table('C:/Users/mihal/OneDrive/2017_datadata.txt',sep="") # Read in the data in a rectangular format
data <- data.frame(ht=data2[,1],at=data2[,2],goals1=data2[,3],goals2=data2[,4]) # Create a df
n <- nrow(data) # Define the sample size
inits <- list ( list ( mu = 0.5, home = 0.5, a = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       d = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), r=1 ) )  # Set up the initial values
attach(data)
data.names <- c(names(data), 'n', 'K') # Defining the names of the data objects
K <- 20
parameter.names <- c('mu','home','a','d','total.points','rank.probs','ranks') # Defining the names of the parameters we wish to monitor
openbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323"




model7.sim <- bugs( data.names, inits, model.file = "Regen_NB_model_UK_2017.txt", parameters = parameter.names,
                    n.chains = 1, n.iter = 6000, n.burnin=1000, n.thin=1,  bugs.directory = openbugs.dir, debug=T, 
                    program="OpenBUGS") # Generating random values using OpenBUGS

# Posterior summaries
print(model7.sim,3) # DIC value 2187
names(model7.sim) # Attributes of the model7.sim

rank(model7.sim$median$a) # Node 11 ( Manchester City has the highest attacking abilities as expected )


total_points <- model7.sim$mean$total.points
round(total_points)

nodes_points_mat <- matrix(c(1:20,round(total_points)), nrow = 2,ncol=20,byrow =T)
nodes_points_mat
colnames(nodes_points_mat) <- c("Arsenal","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
                                "Huddersfield","Leicester","Liverpool","Manchester City","Manchester United","Newcastle","Southampton","Stoke City",
                                "Swansea City","Tottenham","Watford","West Brom","West Ham")
row.names(nodes_points_mat) <- c("Node","Points")
mat <- as.matrix(sort(nodes_points_mat[2,],decreasing = T))
mat
colnames(mat) <- c("Points")
mat














