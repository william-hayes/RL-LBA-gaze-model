library(dplyr)
library(Rcpp)
library(DEoptim)

# Note: no seed was set for Models 1-7 when the models were originally fit.
# The output of DEoptim is a random variable. 
# Thus, fits may not be *identical* to the original, but very similar.
set.seed(82053)

# which model version to fit
args <- commandArgs(trailingOnly = TRUE)
model <- as.integer(args[1])
w_rel <- as.double(args[2])
free_theta <- as.integer(args[3])

# read in the functions for the RL-LBA model
sourceCpp('../../model_functions.cpp') 
source('../../model_utils.R')

# load in list of individual datasets
source('load_data.R')


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


# function for generating initial population for DE optimization
genInitPop <- function(model, NP, free_theta) {
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

# fit model to individual datasets 
system.time(
  fits <- lapply(modeling_data, function(data) {
    objFun <- objectiveFunction(model, data, w_rel=w_rel, theta=if(free_theta) NULL else 50, delta=0)
    DEoptim(fn=objFun, lower=lower_bounds, upper=upper_bounds,
            control=DEoptim.control(NP=NP, itermax=itermax, steptol=250, trace=FALSE, 
                                    initialpop=genInitPop(model, NP, free_theta)))})
)



if ( free_theta ) {
  if ( w_rel > 0 ) {
    saveRDS(fits, file=paste0('results/free-theta/model', model,
                              '_wrel', gsub('\\.', '', as.character(w_rel)),
                              '_fits.RDS'))
  } else {
    saveRDS(fits, file=paste0('results/free-theta/model', model, '_fits.RDS'))
  }
} else {
  if ( w_rel > 0 ) {
    saveRDS(fits, file=paste0('results/model', model,
                              '_wrel', gsub('\\.', '', as.character(w_rel)),
                              '_fits.RDS'))
  } else {
    saveRDS(fits, file=paste0('results/model', model, '_fits.RDS'))
  }
}

