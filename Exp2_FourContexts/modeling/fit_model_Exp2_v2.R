library(dplyr)
library(parallel)
library(DEoptim)

# which model version to fit
args <- commandArgs(trailingOnly = TRUE)
model <- as.integer(args[1])

# read in the functions for the RL-LBA model (version 2)
source('../../model_functions_v2.R') 

# load in list of individual datasets
source('load_data.R')

# set up cluster
n_cores <- min(16, detectCores())
cat('No. of cores: ', n_cores)
cl <- makeCluster(n_cores)

# names to export to cluster
vars <- c('minmax_scaling', 'delta_update', 
          'lbaCDF_single', 'lbaPDF_single', 'lbaPDF',
          'RL_LBA_Model_v2', 'RL_LBA_Prior_v2')

# DE search lower and upper bounds
lower_bounds <- switch(model,
                       rep(1e-10,7),
                       rep(1e-10,7),
                       rep(1e-10,8),
                       rep(1e-10,7),
                       rep(1e-10,7),
                       rep(1e-10,7),
                       rep(1e-10,8))

upper_bounds <- switch(model,
                       c(800, 1, 10, 3000, 1, 3000, 1),
                       c(800, 1, 10, 3000, 1, 3000, 1),
                       c(800, 1, 10, 10, 3000, 1, 3000, 1),
                       c(800, 1, 10, 3000, 1, 3000, 1),
                       c(800, 1, 10, 3000, 1, 3000, 1),
                       c(800, 1, 10, 3000, 1, 3000, 1),
                       c(800, 1, 10, 10, 3000, 1, 3000, 1))


# function for generating initial population for DE optimization
genInitPop <- function(model, NP) {
  if (model %in% c(1,2,4,5,6)) {
    return (cbind(rgamma(n=NP, shape=6, scale=30),
                  rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                  rgamma(n=NP, shape=2, scale=0.5),
                  rgamma(n=NP, shape=6, scale=100),
                  rgamma(n=NP, shape=1.01, scale=0.1),
                  rgamma(n=NP, shape=6, scale=100),
                  rbeta(n=NP, shape1=1.1, shape2=1.1)))
  } else if (model %in% c(3,7)) {
    return (cbind(rgamma(n=NP, shape=6, scale=30),
                  rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                  rgamma(n=NP, shape=2, scale=0.5),
                  rgamma(n=NP, shape=2, scale=0.5),
                  rgamma(n=NP, shape=6, scale=100),
                  rgamma(n=NP, shape=1.01, scale=0.1),
                  rgamma(n=NP, shape=6, scale=100),
                  rbeta(n=NP, shape1=1.1, shape2=1.1)))
  }
}

# fit model to individual datasets 
system.time(
  fits <- lapply(datasets, function(X) {
    DEoptim(fn=RL_LBA_Objective_v2, lower=lower_bounds, upper=upper_bounds,
            control=DEoptim.control(NP=100, itermax=1000, steptol=250, trace=TRUE, cluster=cl, parVar=vars,
                                    initialpop=genInitPop(model, 100)),
            version=model, choice=X$choice, RT=X$RT, 
            options=X$options, outcomes=X$outcomes, gaze=X$gaze_pre)})
)

#saveRDS(fits, file=paste0('results/model', model, '_fits_v2.RDS'))
stopCluster(cl)

