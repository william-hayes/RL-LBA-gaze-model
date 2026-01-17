library(dplyr)
library(DEoptim)
library(Rcpp)

# which model version and batch of subjects to test
args <- commandArgs(trailingOnly = TRUE)
model <- as.integer(args[1])
batch <- as.integer(args[2])
free_theta <- as.integer(args[3])


# read in the functions for the RL-LBA model
sourceCpp('../../model_functions.cpp') 
source('../../model_utils.R')

# load in list of individual datasets
source('load_data.R')


# DE search lower and upper bounds
if ( free_theta ) {
  lower_bounds <- switch(model,
                         rep(1e-10,6),
                         rep(1e-10,6),
                         rep(1e-10,7),
                         rep(1e-10,7),
                         rep(1e-10,7),
                         rep(1e-10,7),
                         rep(1e-10,8),
                         rep(1e-10,8))
  
  upper_bounds <- switch(model,
                         c(800, 1, 10, 3000, 3000, 1),
                         c(800, 1, 10, 3000, 3000, 1),
                         c(800, 1, 10, 10, 3000, 3000, 1),
                         c(800, 1, 10, 3000, 3000, 1, 200),
                         c(800, 1, 10, 3000, 3000, 1, 200),
                         c(800, 1, 10, 3000, 3000, 1, 200),
                         c(800, 1, 10, 10, 3000, 3000, 1, 200),
                         c(800, 1, 10, 10, 3000, 3000, 1, 200))
} else {
  lower_bounds <- switch(model,
                         rep(1e-10,6),
                         rep(1e-10,6),
                         rep(1e-10,7),
                         rep(1e-10,6),
                         rep(1e-10,6),
                         rep(1e-10,6),
                         rep(1e-10,7),
                         rep(1e-10,7))
  
  upper_bounds <- switch(model,
                         c(800, 1, 10, 3000, 3000, 1),
                         c(800, 1, 10, 3000, 3000, 1),
                         c(800, 1, 10, 10, 3000, 3000, 1),
                         c(800, 1, 10, 3000, 3000, 1),
                         c(800, 1, 10, 3000, 3000, 1),
                         c(800, 1, 10, 3000, 3000, 1),
                         c(800, 1, 10, 10, 3000, 3000, 1),
                         c(800, 1, 10, 10, 3000, 3000, 1))
}

# DE control settings
NP <- ifelse(length(lower_bounds) == 8, 120, 100)
itermax <- ifelse(length(lower_bounds) == 8, 1500, 1000)


# function for generating initial population for DE optimization
genInitPop <- function(model, NP, free_theta) {
  if (model %in% c(1,2,4,5,6)) {
    out <- cbind(rgamma(n=NP, shape=6, scale=30),
                 rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                 rgamma(n=NP, shape=2, scale=0.5),
                 rgamma(n=NP, shape=6, scale=100),
                 rgamma(n=NP, shape=6, scale=100),
                 rbeta(n=NP, shape1=1.1, shape2=1.1))
  } else if (model %in% c(3,7,8)) {
    out <- cbind(rgamma(n=NP, shape=6, scale=30),
                 rbeta(n=NP, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1),
                 rgamma(n=NP, shape=2, scale=0.5),
                 rgamma(n=NP, shape=2, scale=0.5),
                 rgamma(n=NP, shape=6, scale=100),
                 rgamma(n=NP, shape=6, scale=100),
                 rbeta(n=NP, shape1=1.1, shape2=1.1))
  }
  
  if ( free_theta && (model %in% c(4,5,6,7,8)) ) {
    out <- cbind(out, rgamma(n=NP, shape=2, scale=20))
  }
  
  return(out)
}


# set minimum number of trials for model fitting
s <- 5

# total number of trials
N <- length(modeling_data[[1]]$choice)


# split individual datasets into batches of size ~5
batches <- splitIndices(length(modeling_data), 20)


# compute accumulative one-step-ahead prediction errors
if ( free_theta ) {
  system.time(
    results <- lapply(batches[[batch]], function(index) {

      cat(paste('Computing APE for subject', IDs[index], '\n'))
      
      llvals <- computeAPE(model, modeling_data[[index]], delta=0, from=s, to=N, 
                           lower_bounds=lower_bounds, upper_bounds=upper_bounds,
                           NP=NP, itermax=itermax, initialpop=genInitPop(model, NP, 1), progress=TRUE)
      
      return( list(subject=IDs[index], LL=llvals, APE=sum(-llvals)) )
    })
  )
} else {
  system.time(
    results <- lapply(batches[[batch]], function(index) {
      
      cat(paste('Computing APE for subject', IDs[index], '\n'))
      
      llvals <- computeAPE(model, modeling_data[[index]], theta=50, delta=0, from=s, to=N, 
                           lower_bounds=lower_bounds, upper_bounds=upper_bounds,
                           NP=NP, itermax=itermax, initialpop=genInitPop(model, NP, 0), progress=TRUE)
      
      return( list(subject=IDs[index], LL=llvals, APE=sum(-llvals)) )
    })
  )
}


if ( free_theta ) {
  saveRDS(results, file=paste0('results/free-theta/model', model, '_batch', batch, '_APE.RDS'))
} else {
  saveRDS(results, file=paste0('results/model', model, '_batch', batch, '_APE.RDS'))
}

