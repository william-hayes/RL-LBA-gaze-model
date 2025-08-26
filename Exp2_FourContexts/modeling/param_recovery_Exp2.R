library(dplyr)
library(parallel)
library(DEoptim)

# random seed
set.seed(650361)

# number of simulated agents
n_sims <- 100

# which model version to test
args <- commandArgs(trailingOnly = TRUE)
model <- as.integer(args[1])

# read in the functions for the RL-LBA model
source('../../model_functions.R') 

# load in list of individual datasets
source('load_data.R')


##### 1. Simulate data using known parameter values #####

# function for sampling data-generating parameters
genParams <- function(model, N) {
  if (model %in% c(1,2,4,5,6)) {
    return (cbind(rgamma(n=N, shape=6, scale=30),
                  rbeta(n=N, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                  rgamma(n=N, shape=2, scale=0.5),
                  rgamma(n=N, shape=6, scale=100),
                  rgamma(n=N, shape=6, scale=100),
                  rbeta(n=N, shape1=1.1, shape2=1.1)))
  } else if (model %in% c(3,7)) {
    return (cbind(rgamma(n=N, shape=6, scale=30),
                  rbeta(n=N, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                  rgamma(n=N, shape=2, scale=0.5),
                  rgamma(n=N, shape=2, scale=0.5),
                  rgamma(n=N, shape=6, scale=100),
                  rgamma(n=N, shape=6, scale=100),
                  rbeta(n=N, shape1=1.1, shape2=1.1)))
  }
}

# data-generating parameters
params <- genParams(model, n_sims)

# which parameters/datasets to use (randomly sampled)
indices <- sample(1:length(datasets), size=n_sims, replace=TRUE)


# simulate model using data-generating parameters
sims <- lapply(1:n_sims, function(i) {
  RL_LBA_Model(params[i,], 
               version=model, 
               options=datasets[[indices[i]]]$options,
               outcomes=datasets[[indices[i]]]$outcomes, 
               gaze=datasets[[indices[i]]]$gaze_pre,
               simulate=TRUE)
})


##### 2. Fit model to simulated data sets #####

# set up cluster
n_cores <- min(16, detectCores())
cat('No. of cores: ', n_cores)
cl <- makeCluster(n_cores)

# names to export to cluster
vars <- c('minmax_scaling', 'delta_update', 
          'lbaCDF_single', 'lbaPDF_single', 'lbaPDF',
          'RL_LBA_Model', 'RL_LBA_Prior')

# DE search lower and upper bounds
lower_bounds <- switch(model,
                       rep(1e-10,6),
                       rep(1e-10,6),
                       rep(1e-10,7),
                       rep(1e-10,6),
                       rep(1e-10,6),
                       rep(1e-10,6),
                       rep(1e-10,7))

upper_bounds <- switch(model,
                       c(800, 1, 10, 3000, 3000, 1),
                       c(800, 1, 10, 3000, 3000, 1),
                       c(800, 1, 10, 10, 3000, 3000, 1),
                       c(800, 1, 10, 3000, 3000, 1),
                       c(800, 1, 10, 3000, 3000, 1),
                       c(800, 1, 10, 3000, 3000, 1),
                       c(800, 1, 10, 10, 3000, 3000, 1))


# fit model to simulated datasets 
system.time(
  fits <- lapply(1:n_sims, function(i) {
    DEoptim(fn=RL_LBA_Objective, lower=lower_bounds, upper=upper_bounds,
            control=DEoptim.control(NP=100, itermax=1000, steptol=250, trace=FALSE, cluster=cl, parVar=vars,
                                    initialpop=genParams(model, 100)),
            version=model, choice=sims[[i]][,1], RT=sims[[i]][,2], 
            options=datasets[[indices[i]]]$options, outcomes=datasets[[indices[i]]]$outcomes,
            gaze=datasets[[indices[i]]]$gaze_pre)})
)

# recovered parameters
recovered <- t(sapply(fits, function(X) X$optim$bestmem))

# save results
results <- list(generating = params, recovered = recovered)
#saveRDS(results, file=paste0('results/model', model, '_paramRecovery.RDS'))
stopCluster(cl)

