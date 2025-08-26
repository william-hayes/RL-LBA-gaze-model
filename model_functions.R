##### LBA Functions #####

# cumulative density function (CDF) for a single accumulator
# i.e., the probability that the accumulator crosses the threshold before time t
lbaCDF_single <- function(t, b, A, v, s) {
  term1 <- (b - A - t*v) * pnorm((b - A - t*v) / (t*s))
  term2 <- (b - t*v) * pnorm((b - t*v) / (t*s))
  term3 <- t*s * dnorm((b - A - t*v) / (t*s))
  term4 <- t*s * dnorm((b - t*v) / (t*s))
  return (pmin(pmax(1 + (term1 - term2 + term3 - term4) / A, 0), 1))
}

# probability density function (PDF) for a single accumulator
lbaPDF_single <- function(t, b, A, v, s) {
  term1 <- s * dnorm((b - A - t*v) / (t*s))
  term2 <- v * pnorm((b - A - t*v) / (t*s))
  term3 <- s * dnorm((b - t*v) / (t*s))
  term4 <- v * pnorm((b - t*v) / (t*s))
  return (pmax((term1 - term2 - term3 + term4) / A, 0))
}

# PDF for an observed choice/RT pair.
# The accumulator for the chosen option crosses at time t, before any other accumulators have crossed
lbaPDF <- function(rt, resp, b, A, v, s, t0, log=FALSE) {
  t <- pmax(rt - t0, 0)
  pd <- lbaPDF_single(t, b, A, v[resp], s) * prod(1 - lbaCDF_single(t, b, A, v[-resp], s))
  if (log) return(log(pd))
  return(pd)
}

# function for simulating LBA accumulation process
lbaSIM <- function(b, A, v, s, t0, RT_lb=250, RT_ub=10000) {
  start_points <- runif(n=length(v), 0, A)
  crossing_times <- rep(Inf, length(v))
  rt <- min(t0 + crossing_times)
  max_iter <- 1000
  iter <- 0
  while (rt < RT_lb | rt > RT_ub) {
    drift_rates <- rnorm(n=length(v), v, s)
    drift_rates[drift_rates < 0] <- 0
    distances <- b - start_points
    crossing_times <- distances / drift_rates
    rt <- min(t0 + crossing_times)
    iter <- iter + 1
    if (iter == max_iter) break
  }
  resp <- which.min(t0 + crossing_times)
  return(c(response=resp, rt=rt))
}


##### RL Functions #####

# range normalization function
minmax_scaling <- function(x, lower, upper) {
  if (lower == upper) {
    return (rep(1/length(x), length(x)))
  } else{
    return ((x - lower) / (upper - lower))
  }
}

# delta updating function
delta_update <- function(Q, R, rate) {
  return (Q + rate * (R - Q))
}


##### Model Log-Likelihood Function #####

# 1: linear, no gaze
# 2: linear, multiplicative gaze
# 3: linear, additive gaze
# 4: nonlinear, no gaze
# 5: nonlinear, pre-multiplicative gaze
# 6: nonlinear, post-multiplicative gaze
# 7: nonlinear, additive gaze
RL_LBA_Model <- function(params, version, choice=NULL, RT=NULL, options, outcomes, 
                         avail=options, gaze=NULL, RT_cutoffs=c(250, 10000), simulate=FALSE, sum_LL=TRUE) {
  if (version %in% c(3,7)) {
    t0 <- params[1]            # nondecision time
    learn_rate <- params[2]    # learning rate
    Q_beta <- params[3]        # effect of Q values on drift rate
    gaze_beta <- params[4]     # effect of gaze on drift rate
    threshold_sep <- params[5] # threshold separation
    upper_bound <- params[6]   # start point upper bound
    w_rel <- ifelse(length(params) == 7, params[7], 0)  # relative encoding
  } else if (version %in% c(1,2,4,5,6)) {
    t0 <- params[1]            # nondecision time
    learn_rate <- params[2]    # learning rate
    dscale <- params[3]        # drift rate scaling parameter
    threshold_sep <- params[4] # threshold separation
    upper_bound <- params[5]   # start point upper bound
    w_rel <- ifelse(length(params) == 6, params[6], 0)  # relative encoding
  }
  drift_sd <- 0.1            # drift rate standard deviation (fixed)
  threshold <- upper_bound + threshold_sep
  
  if (simulate) {
    # initialize containers for simulated choices and RTs
    choice <- integer(nrow(avail))
    RT <- numeric(nrow(avail))
  } else {
    # initialize container for log-likelihoods
    log_likelihoods <- numeric(length(choice))
  }
  
  # initialize Q values
  Q <- rep(0.5, max(options))
  
  # global minimum / maximum outcome values
  min_outcome <- min(outcomes, na.rm=T)
  max_outcome <- max(outcomes, na.rm=T)
  
  
  # iterate across the observations
  for (i in 1:length(choice)) {
    
    # mean drift rates depend on the model version
    if (version %in% c(1,3)) {
      Q_values <- Q[avail[i,]]
    } else if (version == 2) {
      Q_values <- gaze[i,] * Q[avail[i,]]
    } else if (version == 5) {
      expQ <- exp(50 * gaze[i,] * Q[avail[i,]])
      Q_values <- expQ / sum(expQ)
    } else if (version == 6) {
      expQ <- exp(50 * Q[avail[i,]])
      Q_values <- gaze[i,] * (expQ / sum(expQ))
    } else {
      expQ <- exp(50 * Q[avail[i,]])
      Q_values <- expQ / sum(expQ)
    } 
    
    if (version %in% c(3,7)) {
      drifts <- Q_beta * Q_values + gaze_beta * gaze[i,]
    } else {
      drifts <- dscale * Q_values
    }
    
    if (simulate) {
      sims <- lbaSIM(b=threshold, A=upper_bound, v=drifts, s=drift_sd, t0=t0,
                     RT_lb=RT_cutoffs[1], RT_ub=RT_cutoffs[2])
      choice[i] <- sims[['response']]
      RT[i] <- sims[['rt']]
    } else {
      if (RT[i] >= RT_cutoffs[1] & RT[i] <= RT_cutoffs[2]) {
        # log-likelihood of choice-RT pair
        LL <- lbaPDF(rt=RT[i], resp=choice[i], b=threshold, A=upper_bound,
                     v=drifts, s=drift_sd, t0=t0, log=TRUE)
        log_likelihoods[i] <- LL
      }
    }
    
    # if feedback is provided, encode outcomes and update Q values
    if (all(!is.na(outcomes[i,]))) {
      R_abs <- minmax_scaling(outcomes[i,], min_outcome, max_outcome)
      R_rel <- minmax_scaling(outcomes[i,], min(outcomes[i,]), max(outcomes[i,]))
      
      Q[options[i,]] <- delta_update(Q[options[i,]], R_abs * (1-w_rel) + R_rel * w_rel, learn_rate)
    }
    
  }
  
  if (simulate) {
    return (cbind(choice, RT))
  } else {
    if (sum_LL) {
      # return the summed log-likelihood
      return (sum(log_likelihoods))
    } else {
      # return a vector of log-likelihoods (one per observation)
      return (log_likelihoods)
    }
  }
}


##### Log-Prior Function #####


RL_LBA_Prior <- function(params, version) {
  if (version %in% c(3,7)) {
    t0 <- params[1]            # nondecision time
    learn_rate <- params[2]    # learning rate
    Q_beta <- params[3]        # effect of Q values on drift rate
    gaze_beta <- params[4]     # effect of gaze on drift rate
    threshold_sep <- params[5] # threshold separation
    upper_bound <- params[6]   # start point upper bound
    w_rel <- ifelse(length(params) == 7, params[7], NA)  # relative encoding
    
    return(
      dgamma(t0, shape=6, scale=30, log=TRUE) +
        dbeta(learn_rate, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1, log=TRUE) +
        dgamma(Q_beta, shape=2, scale=0.5, log=TRUE) +
        dgamma(gaze_beta, shape=2, scale=0.5, log=TRUE) +
        dgamma(threshold_sep, shape=6, scale=100, log=TRUE) +
        dgamma(upper_bound, shape=6, scale=100, log=TRUE) +
        ifelse(!is.na(w_rel), dbeta(w_rel, shape1=1.1, shape2=1.1, log=TRUE), 0)
    )
  } else if (version %in% c(1,2,4,5,6)) {
    t0 <- params[1]            # nondecision time
    learn_rate <- params[2]    # learning rate
    dscale <- params[3]        # drift rate scaling parameter
    threshold_sep <- params[4] # threshold separation
    upper_bound <- params[5]   # start point upper bound
    w_rel <- ifelse(length(params) == 6, params[6], NA)  # relative encoding

    return(
      dgamma(t0, shape=6, scale=30, log=TRUE) +
        dbeta(learn_rate, shape1=0.1*(5-2) + 1, shape2=(1-0.1)*(5-2) + 1, log=TRUE) +
        dgamma(dscale, shape=2, scale=0.5, log=TRUE) +
        dgamma(threshold_sep, shape=6, scale=100, log=TRUE) +
        dgamma(upper_bound, shape=6, scale=100, log=TRUE) +
        ifelse(!is.na(w_rel), dbeta(w_rel, shape1=1.1, shape2=1.1, log=TRUE), 0)
    )
  }
}


##### Model Objective Function #####


RL_LBA_Objective <- function(params, version, choice, RT, options, outcomes, 
                             avail=options, gaze=NULL, RT_cutoffs=c(250, 10000)) {
  
  return(-1 * (RL_LBA_Model(params, version, choice, RT, options, outcomes,
                            avail, gaze, RT_cutoffs) +
                 RL_LBA_Prior(params, version)))
}


##### Function for Extracting Latent Variables #####

RL_LBA_LatentVars <- function(params, version, choice=NULL, RT=NULL, options, outcomes, avail=options, gaze=NULL) {
  if (version %in% c(3,7)) {
    t0 <- params[1]            # nondecision time
    learn_rate <- params[2]    # learning rate
    Q_beta <- params[3]        # effect of Q values on drift rate
    gaze_beta <- params[4]     # effect of gaze on drift rate
    threshold_sep <- params[5] # threshold separation
    upper_bound <- params[6]   # start point upper bound
    w_rel <- ifelse(length(params) == 7, params[7], 0)  # relative encoding
  } else if (version %in% c(1,2,4,5,6)) {
    t0 <- params[1]            # nondecision time
    learn_rate <- params[2]    # learning rate
    dscale <- params[3]        # drift rate scaling parameter
    threshold_sep <- params[4] # threshold separation
    upper_bound <- params[5]   # start point upper bound
    w_rel <- ifelse(length(params) == 6, params[6], 0)  # relative encoding
  }
  drift_sd <- 0.1            # drift rate standard deviation (fixed)
  threshold <- upper_bound + threshold_sep

  # initialize containers
  Q_all <- matrix(nrow=nrow(avail), ncol=max(options))
  Q_avail <- matrix(nrow=nrow(avail), ncol=ncol(avail))
  driftMeans <- matrix(nrow=nrow(avail), ncol=ncol(avail))
  
  # initialize Q values
  Q <- rep(0.5, max(options))
  
  # global minimum / maximum outcome values
  min_outcome <- min(outcomes, na.rm=T)
  max_outcome <- max(outcomes, na.rm=T)
  
  
  # iterate across the observations
  for (i in 1:nrow(avail)) {
    
    Q_all[i,] <- Q
    Q_avail[i,] <- Q[avail[i,]]
    
    # mean drift rates depend on the model version
    if (version %in% c(1,3)) {
      Q_values <- Q[avail[i,]]
    } else if (version == 2) {
      Q_values <- gaze[i,] * Q[avail[i,]]
    } else if (version == 5) {
      expQ <- exp(50 * gaze[i,] * Q[avail[i,]])
      Q_values <- expQ / sum(expQ)
    } else if (version == 6) {
      expQ <- exp(50 * Q[avail[i,]])
      Q_values <- gaze[i,] * (expQ / sum(expQ))
    } else {
      expQ <- exp(50 * Q[avail[i,]])
      Q_values <- expQ / sum(expQ)
    } 
    
    if (version %in% c(3,7)) {
      drifts <- Q_beta * Q_values + gaze_beta * gaze[i,]
    } else {
      drifts <- dscale * Q_values
    }
    
    driftMeans[i,] <- drifts
    
    # if feedback is provided, encode outcomes and update Q values
    if (all(!is.na(outcomes[i,]))) {
      R_abs <- minmax_scaling(outcomes[i,], min_outcome, max_outcome)
      R_rel <- minmax_scaling(outcomes[i,], min(outcomes[i,]), max(outcomes[i,]))
      
      Q[options[i,]] <- delta_update(Q[options[i,]], R_abs * (1-w_rel) + R_rel * w_rel, learn_rate)
    }
    
  }
  
  return(list(Q_all=Q_all, Q_avail=Q_avail, gaze=gaze, driftMeans=driftMeans))
}

