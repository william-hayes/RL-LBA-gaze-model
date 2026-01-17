#### Model Utilities ####

#### CONTENTS: 
#### 1. Function factory for creating objective (log-posterior) functions
#### 2. Function factory for creating log-likelihood functions
#### 3. Function factory for creating model simulation functions
#### 4. Function for computing accumulative one-step-ahead prediction error (APE)


#### NOTE:
#### The following functions must first be sourced from model_functions.cpp:
#### - Model_Objective
#### - Model_LL
#### - Simulate_Model


# 1. Objective function factory: Given a model index, data (list), and parameter constraints (optional), 
#    return an objective function that accepts a vector of parameters as the only argument
objectiveFunction <- function(model, data, w_rel=NULL, theta=NULL, delta=NULL, n_obs=NULL) {
  data <- append(data, list(model=model), after=0)
  if ( !is.null(w_rel) ) {
    data <- append(data, list(w_rel=w_rel))
  }
  if ( !is.null(theta) ) {
    data <- append(data, list(theta=theta))
  }
  if ( !is.null(delta) ) {
    data <- append(data, list(delta=delta))
  }
  if ( !is.null(n_obs) ) {
    data <- append(data, list(n_obs=n_obs))
  }
  function(params) { do.call(Model_Objective, append(list(params), data)) }
}


# 2. Log-likelihood function factory: Given a model index, data (list), and parameter constraints (optional), 
#    return a log-likelihood function that accepts a vector of parameters as the only argument
llFunction <- function(model, data, w_rel=NULL, theta=NULL, delta=NULL, n_obs=NULL) {
  data <- append(data, list(model=model), after=0)
  if ( !is.null(w_rel) ) {
    data <- append(data, list(w_rel=w_rel))
  }
  if ( !is.null(theta) ) {
    data <- append(data, list(theta=theta))
  }
  if ( !is.null(delta) ) {
    data <- append(data, list(delta=delta))
  }
  if ( !is.null(n_obs) ) {
    data <- append(data, list(n_obs=n_obs))
  }
  function(params) { do.call(Model_LL, append(list(params), data)) }
}


# 3. Simulation function factory: Given a model index, data (list), and parameter constraints (optional), 
#    return a simulation function that accepts a vector of parameters as the only argument
simFunction <- function(model, data, w_rel=NULL, theta=NULL, delta=NULL) {
  data <- append(data, list(model=model), after=0)
  if ( !is.null(w_rel) ) {
    data <- append(data, list(w_rel=w_rel))
  }
  if ( !is.null(theta) ) {
    data <- append(data, list(theta=theta))
  }
  if ( !is.null(delta) ) {
    data <- append(data, list(delta=delta))
  }
  data[["choice"]] <- NULL; data[["RT"]] <- NULL
  function(params) { sims <- do.call(Simulate_Model, append(list(params), data)); cbind(sims[[1]], sims[[2]])}
}


# 4. Function for computing accumulative one-step-ahead prediction error (APE)
computeAPE <- function(model, data, ..., from, to, lower_bounds, upper_bounds,
                       NP, itermax, initialpop=NULL, sum=FALSE, progress=FALSE) {
  
  indices <- seq(from, to-1) 
  LL <- numeric(length(indices)) # container for one-step-ahead log-likelihoods
  
  if ( progress )
    pb <- utils::txtProgressBar(min=1, max=length(indices), style=3)
  
  for (i in seq_along(indices)) {
    
    # define the objective function and log-likelihood function for this iteration
    objFun <- objectiveFunction(model, data, ..., n_obs=indices[i])
    llFun <- llFunction(model, data, ..., n_obs=indices[i]+1)
    
    # fit model to observations 1 through n
    fit <- DEoptim(fn=objFun, 
                   lower=lower_bounds, upper=upper_bounds,
                   control=DEoptim.control(NP=NP, itermax=itermax, steptol=250, 
                                           initialpop=initialpop, trace=FALSE))
    
    # save parameter estimates
    params <- fit$optim$bestmem
    
    # compute log likelihoods for observations 1 through n+1
    logLiks <- llFun(params)
    
    # save the one-step-ahead prediction error (log-likelihood for trial n+1)
    LL[i] <- tail(logLiks, 1)
    
    # update progress bar
    utils::setTxtProgressBar(pb, i)
    
  }
  
  if ( progress ) 
    close(pb)
  
  if ( sum ) 
    return( sum(LL) )
  else
    return( LL )
}