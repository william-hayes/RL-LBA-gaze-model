library(dplyr)
library(parallel)
library(DEoptim)

# which model version and batch of subjects to test
args <- commandArgs(trailingOnly = TRUE)
model <- as.integer(args[1])
batch <- as.integer(args[2])


# read in the functions for the RL-LBA model
source('../../model_functions.R') 

# load in list of individual datasets
source('load_data.R')

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


# function for generating initial population for DE optimization
genInitPop <- function(model, NP) {
  if (model %in% c(1,2,4,5,6)) {
    return (cbind(rgamma(n=NP, shape=6, scale=30),
                  rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                  rgamma(n=NP, shape=2, scale=0.5),
                  rgamma(n=NP, shape=6, scale=100),
                  rgamma(n=NP, shape=6, scale=100),
                  rbeta(n=NP, shape1=1.1, shape2=1.1)))
  } else if (model %in% c(3,7)) {
    return (cbind(rgamma(n=NP, shape=6, scale=30),
                  rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                  rgamma(n=NP, shape=2, scale=0.5),
                  rgamma(n=NP, shape=2, scale=0.5),
                  rgamma(n=NP, shape=6, scale=100),
                  rgamma(n=NP, shape=6, scale=100),
                  rbeta(n=NP, shape1=1.1, shape2=1.1)))
  }
}


# set minimum number of trials for model fitting
s <- 5

# total number of trials
N <- length(datasets[[1]]$choice)


# split individual datasets into batches of size ~5
batches <- splitIndices(length(datasets), 20)


# fit model to individual datasets 
system.time(
  results <- lapply(datasets[batches[[batch]]], function(X) {
    
    # subject ID
    subject <- X$subject
    cat(paste('Computing APE for subject', subject, '\n'))
    
    # containers for one-step-ahead log-likelihoods and parameter estimates on each iteration
    LL <- numeric(N)
    params <- matrix(nrow=N, ncol=length(lower_bounds))
    
    # iterate over trials
    for (i in (s+1):N) {
      
      # random initial population
      initPop <- genInitPop(model, 100)
      
      # fit model to data from trials 1 to i-1
      fit <- DEoptim(fn=RL_LBA_Objective, lower=lower_bounds, upper=upper_bounds,
                     control=DEoptim.control(NP=100, itermax=1000, steptol=250, trace=TRUE, cluster=cl, parVar=vars,
                                             initialpop=initPop),
                     version=model, 
                     choice=X$choice[1:(i-1)],
                     RT=X$RT[1:(i-1)], 
                     options=X$options[1:(i-1), , drop=FALSE], 
                     outcomes=X$outcomes[1:(i-1), , drop=FALSE], 
                     gaze=X$gaze_pre[1:(i-1), , drop=FALSE])
      
      # save parameter estimates
      params[i-1,] <- fit$optim$bestmem
      
      # compute log likelihoods for trials 1 to i
      logLiks <- RL_LBA_Model(params[i-1,],
                              version=model,
                              choice=X$choice[1:i],
                              RT=X$RT[1:i],
                              options=X$options[1:i, , drop=FALSE],
                              outcomes=X$outcomes[1:i, , drop=FALSE],
                              gaze=X$gaze_pre[1:i, , drop=FALSE],
                              sum_LL=FALSE)
      
      # save the one-step-ahead prediction error (log-likelihood for trial i)
      LL[i] <- tail(logLiks, 1)
      
    }
    
    return(list(subject=subject, LL=LL, params=params))})
)

#saveRDS(results, file=paste0('results/model', model, '_batch', batch, '_APE.RDS'))
stopCluster(cl)

