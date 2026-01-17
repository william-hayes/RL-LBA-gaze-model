library(dplyr)
library(Rcpp)
library(DEoptim)

# random seed
set.seed(308357)

# number of simulated agents
n_sims <- 100

# which model version to test
args <- commandArgs(trailingOnly = TRUE)
model <- as.integer(args[1])
w_rel <- as.double(args[2])
free_theta <- as.integer(args[3])

# read in the functions for the RL-LBA model
sourceCpp('../../model_functions.cpp') 
source('../../model_utils.R')

# load in list of individual datasets
source('load_data.R')


##### 1. Simulate data using known parameter values #####


# function for sampling data-generating parameters
# and generating initial population for DE optimization
genParams <- function(model, NP, free_theta) {
  if (model %in% c(1,2,4,5,6)) {
    out <- cbind(rgamma(n=NP, shape=6, scale=30),
                 rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                 rgamma(n=NP, shape=2, scale=0.5),
                 rgamma(n=NP, shape=6, scale=100),
                 rgamma(n=NP, shape=6, scale=100))
  } else if (model %in% c(3,7,8)) {
    out <- cbind(rgamma(n=NP, shape=6, scale=30),
                 rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                 rgamma(n=NP, shape=2, scale=0.5),
                 rgamma(n=NP, shape=2, scale=0.5),
                 rgamma(n=NP, shape=6, scale=100),
                 rgamma(n=NP, shape=6, scale=100))
  }
  
  if ( free_theta && (model %in% c(4,5,6,7,8)) ) {
    out <- cbind(out, rgamma(n=NP, shape=2, scale=20))
  }
  
  return(out)
}


# data-generating parameters
params <- genParams(model, n_sims, free_theta)

# which datasets to use (randomly sampled)
indices <- sample(1:length(modeling_data), size=n_sims, replace=TRUE)

# simulate model using data-generating parameters
sims <- lapply(1:n_sims, function(i) {
  sim <- simFunction(model, modeling_data[[indices[i]]], 
                     w_rel=w_rel, theta=if(free_theta) NULL else 50, delta=0)
  sim(params[i,])
})


##### 2. Fit model to simulated data sets #####


# DE search lower and upper bounds
if ( free_theta ) {
  lower_bounds <- switch(model,
                         rep(1e-10,5),
                         rep(1e-10,5),
                         rep(1e-10,6),
                         rep(1e-10,6),
                         rep(1e-10,6),
                         rep(1e-10,6),
                         rep(1e-10,7),
                         rep(1e-10,7))
  
  upper_bounds <- switch(model,
                         c(800, 1, 10, 3000, 3000),
                         c(800, 1, 10, 3000, 3000),
                         c(800, 1, 10, 10, 3000, 3000),
                         c(800, 1, 10, 3000, 3000, 200),
                         c(800, 1, 10, 3000, 3000, 200),
                         c(800, 1, 10, 3000, 3000, 200),
                         c(800, 1, 10, 10, 3000, 3000, 200),
                         c(800, 1, 10, 10, 3000, 3000, 200))
} else {
  lower_bounds <- switch(model,
                         rep(1e-10,5),
                         rep(1e-10,5),
                         rep(1e-10,6),
                         rep(1e-10,5),
                         rep(1e-10,5),
                         rep(1e-10,5),
                         rep(1e-10,6),
                         rep(1e-10,6))
  
  upper_bounds <- switch(model,
                         c(800, 1, 10, 3000, 3000),
                         c(800, 1, 10, 3000, 3000),
                         c(800, 1, 10, 10, 3000, 3000),
                         c(800, 1, 10, 3000, 3000),
                         c(800, 1, 10, 3000, 3000),
                         c(800, 1, 10, 3000, 3000),
                         c(800, 1, 10, 10, 3000, 3000),
                         c(800, 1, 10, 10, 3000, 3000))
}


# DE control settings
NP <- 100
itermax <- 1000


# fit model to simulated datasets 
system.time(
  fits <- lapply(1:n_sims, function(i) {
    data <- modeling_data[[indices[i]]]
    data[['choice']] <- sims[[i]][,1]
    data[['RT']] <- sims[[i]][,2]
    objFun <- objectiveFunction(model, data, w_rel=w_rel, theta=if(free_theta) NULL else 50, delta=0)
    DEoptim(fn=objFun, lower=lower_bounds, upper=upper_bounds,
            control=DEoptim.control(NP=NP, itermax=itermax, steptol=250, trace=FALSE, 
                                    initialpop=genParams(model, NP, free_theta)))})
)

# recovered parameters
recovered <- t(sapply(fits, function(X) X$optim$bestmem))

# save results
results <- list(generating = params, recovered = recovered)
if ( free_theta ) {
  if ( w_rel > 0 ) {
    saveRDS(results, file=paste0('results/free-theta/model', model,
                                 '_wrel', gsub('\\.', '', as.character(w_rel)),
                                 '_paramRecovery.RDS'))
  } else {
    saveRDS(results, file=paste0('results/free-theta/model', model, '_paramRecovery.RDS'))
  }
} else {
  if ( w_rel > 0 ) {
    saveRDS(results, file=paste0('results/model', model,
                                 '_wrel', gsub('\\.', '', as.character(w_rel)),
                                 '_paramRecovery.RDS'))
  } else {
    saveRDS(results, file=paste0('results/model', model, '_paramRecovery.RDS'))
  }
}

