#include <Rcpp.h>
using namespace Rcpp;


// LBA FUNCTIONS

/* cumulative density function (CDF) for a single accumulator
    i.e., the probability that the accumulator crosses the threshold before time t */
double lbaCDF_single_cpp(double t, double b, double A, double v, double s) {
  double b_A_tv = b - A - t*v;
  double b_tv = b - t*v;
  double ts = t*s;
  double term1 = b_A_tv * R::pnorm(b_A_tv / ts, 0.0, 1.0, true, false);
  double term2 = b_tv * R::pnorm(b_tv / ts, 0.0, 1.0, true, false);
  double term3 = ts * R::dnorm(b_A_tv / ts, 0.0, 1.0, false);
  double term4 = ts * R::dnorm(b_tv / ts, 0.0, 1.0, false);
  double out = 1 + (term1 - term2 + term3 - term4) / A;
  if (out < 0) {
    return 0;
  }
  else if (out > 1) {
    return 1;
  }
  else {
    return out;
  }
}


/* probability density function (PDF) for a single accumulator */
double lbaPDF_single_cpp(double t, double b, double A, double v, double s) {
  double b_A_tv = b - A - t*v;
  double b_tv = b - t*v;
  double ts = t*s;
  double term1 = s * R::dnorm(b_A_tv / ts, 0.0, 1.0, false);
  double term2 = v * R::pnorm(b_A_tv / ts, 0.0, 1.0, true, false);
  double term3 = s * R::dnorm(b_tv / ts, 0.0, 1.0, false);
  double term4 = v * R::pnorm(b_tv / ts, 0.0, 1.0, true, false);
  double out = (term1 - term2 - term3 + term4) / A;
  if (out < 0) {
    return 0;
  }
  else {
    return out;
  }
}



/* LBA log likelihood function for a single choice/RT pair.
    The accumulator for the chosen option crosses at time t, before any other accumulators have crossed */
double lbaLogLik_cpp(double rt, int resp, double b, double A,
                     NumericVector v, double s, double t0) {
  int n = v.size();
  double t = rt - t0;
  if ( t < 0.0 ) {
    t = 0.0;
  }
  
  double out = 0.0;
  for ( int i = 0; i < n; ++i ) {
    if ( i == resp ) {
      out += log( lbaPDF_single_cpp(t, b, A, v[i], s) );
    }
    else {
      out += log( 1 - lbaCDF_single_cpp(t, b, A, v[i], s) );
    }
  }
  
  return out;
}


/* function for simulating LBA accumulation process */
NumericVector lbaSimulator(double b, double A, NumericVector v, double s, double t0,
                           double rt_lower, double rt_upper) {
  int n = v.size();
  NumericVector start_points = Rcpp::runif(n, 0.0, A);
  NumericVector crossing_times(n, R_PosInf);
  double rt = min(t0 + crossing_times);
  int max_iter = 1000;
  int iter = 0;
  NumericVector drift_rates(n);
  NumericVector distances(n);
  while ( rt < rt_lower || rt > rt_upper ) {
    drift_rates = Rcpp::rnorm(n, 0.0, s) + v;
    drift_rates = clamp(0.0, drift_rates, R_PosInf);
    distances = b - start_points;
    crossing_times = distances / drift_rates;
    rt = min(t0 + crossing_times);
    iter += 1;
    if ( iter == max_iter )
      break;
  }
  int resp = which_min(t0 + crossing_times);
  NumericVector out = NumericVector::create(resp, rt);
  
  return out;
}


// RL FUNCTIONS

/* range normalization function */
NumericVector range_rescale(const NumericVector& X, double lower, double upper) {
  int n = X.size();
  NumericVector out(n);

  if ( lower == upper ) {
    out.fill( 1.0 / n );
  }
  else {
    for( int i = 0; i < n; ++i ) {
      out[i] = (X[i] - lower) / (upper - lower);
    }
  }

  return out;
}


/* delta rule updating function */
NumericVector Q_update(const NumericVector& Q, const NumericVector& X, double rate) {
  int n = Q.size();
  NumericVector out(n);

  for( int i = 0; i < n; ++i ) {
    // do not update Q if outcome is NA
    if ( Rcpp::traits::is_na<REALSXP>(X[i]) )
      out[i] = Q[i];
    else 
      out[i] = Q[i] + rate * (X[i] - Q[i]);
  }

  return out;
}


// LINKING FUNCTION

NumericVector compute_drifts(const int model, const NumericVector& Q, const NumericVector& gaze,
                             double b_Q, double b_gaze, double theta) {
  if ( model == 1 ) {
    return b_Q * Q;
  }
  else if ( model == 2 ) {
    return b_Q * (Q * gaze);
  }
  else if ( model == 3 ) {
    return b_Q * Q + b_gaze * gaze;
  }
  else if ( model == 4 ) {
    int n = Q.size();
    NumericVector expQ(n);
    
    for ( int i = 0; i < n; ++i ) {
      expQ[i] = exp(theta * Q[i]);
    }
    
    return b_Q * (expQ / sum(expQ));
  }
  else if ( model == 5 ) {
    int n = Q.size();
    NumericVector expQ(n);
    
    for ( int i = 0; i < n; ++i ) {
      expQ[i] = exp(theta * Q[i] * gaze[i]);
    }
    
    return b_Q * (expQ / sum(expQ));
  }
  else if ( model == 6 ) {
    int n = Q.size();
    NumericVector expQ(n);
    
    for ( int i = 0; i < n; ++i ) {
      expQ[i] = exp(theta * Q[i]);
    }
    
    return b_Q * (expQ / sum(expQ)) * gaze;
  }
  else if ( model == 7 ) {
    int n = Q.size();
    NumericVector expQ(n);
    
    for ( int i = 0; i < n; ++i ) {
      expQ[i] = exp(theta * Q[i]);
    }
    
    return b_Q * (expQ / sum(expQ)) + b_gaze * gaze;
  }
  else if ( model == 8 ) {
    // log-sum-exp trick for numerical stability
    int n = Q.size();
    NumericVector V = theta * (Q + b_gaze * gaze);
    double Vmax = max(V);
    double logDenom = Vmax + log(sum(exp(V - Vmax)));
    NumericVector softmaxQ(n);
    
    for ( int i = 0; i < n; ++i ) {
      softmaxQ[i] = exp(V[i] - logDenom);
    }
    
    return b_Q * softmaxQ;
  }
  else {
    stop("Unknown model.");
  }
}



// MODEL LOG-LIKELIHOOD FUNCTION


// [[Rcpp::export]]
NumericVector Model_LL(const NumericVector params, const int model, const IntegerVector choice, const NumericVector RT,
                       const IntegerMatrix options, const NumericMatrix outcomes, const IntegerMatrix avail,
                       const NumericMatrix gaze, int n_opt, double min_o, double max_o, double rt_lower, double rt_upper, 
                       double w_rel=-1.0, double theta=-1.0, double delta=-1.0, double Q_init=0.5, int n_obs=-1) {
  double t0 = 0.0;
  double learn_rate = 0.0;
  double beta_Q = 0.0;
  double beta_gaze = 0.0;
  double threshold_sep = 0.0;
  double upper_bound = 0.0;
  if ( model == 1 || model == 2 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    threshold_sep = params[3];
    upper_bound = params[4];
    if ( w_rel < 0.0 || w_rel > 1.0 )
      w_rel = params[5];
  }
  else if ( model == 3 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    beta_gaze = params[3];
    threshold_sep = params[4];
    upper_bound = params[5];
    if ( w_rel < 0.0 || w_rel > 1.0 )
      w_rel = params[6];
  }
  else if ( model == 4 || model == 5 || model == 6 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    threshold_sep = params[3];
    upper_bound = params[4];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[5];
      if ( theta < 0.0 ) {
        theta = params[6];
      }
    }
    else {
      if ( theta < 0.0 ) {
        theta = params[5];
      }
    }
  }
  else if ( model == 7 || model == 8 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    beta_gaze = params[3];
    threshold_sep = params[4];
    upper_bound = params[5];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[6];
      if ( theta < 0.0 ) {
        theta = params[7];
      }
    }
    else {
      if ( theta < 0.0 ) {
        theta = params[6];
      }
    }
  }
  else {
    stop("Unknown model.");
  }
  
  if ( delta < 0.0 )
    delta = params(params.length() - 1);
  
  double drift_sd = 0.1;
  double threshold = 0.0;
  
  // container for log-likelihoods
  if ( n_obs < 0 )
    n_obs = choice.size();
  NumericVector LL (n_obs, 0.0);
  
  // initial Q values
  NumericVector Q (n_opt, Q_init);
  
  // additional containers to use later
  int K = avail.ncol();
  NumericVector drifts (K);
  NumericVector R_abs (K);
  NumericVector R_rel (K);
  
  for ( int i = 0; i < n_obs; ++i ) {
    
    // mean drift rates
    drifts = compute_drifts(model, Q[ avail(i, _) ], gaze(i, _), beta_Q, beta_gaze, theta);
    
    // trial-dependent decision threshold when delta > 0
    threshold = upper_bound + threshold_sep / (1 + delta * i);
    
    // log-likelihood of current choice-RT pair
    if ( RT[i] >= rt_lower && RT[i] <= rt_upper ) {
      LL[i] += lbaLogLik_cpp( RT[i], choice[i], threshold, upper_bound, drifts, drift_sd, t0 );
    }
    
    // range-normalized rewards
    R_abs = range_rescale( outcomes(i, _), min_o, max_o );
    R_rel = range_rescale( outcomes(i, _), min(outcomes(i, _)), max(outcomes(i, _)) );
    
    // update Q values
    Q[ options(i, _) ] = Q_update( Q[ options(i, _) ], (1-w_rel) * R_abs + w_rel * R_rel, learn_rate );
    
  }
  
  return LL;
}



// LOG-PRIOR FUNCTION

double LogPrior(const NumericVector params, const int model, 
                double w_rel=-1.0, double theta=-1.0, double delta=-1.0) {
  double out = 0.0;
  
  double t0 = 0.0;
  double learn_rate = 0.0;
  double beta_Q = 0.0;
  double beta_gaze = 0.0;
  double threshold_sep = 0.0;
  double upper_bound = 0.0;
  if ( model == 1 || model == 2 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    threshold_sep = params[3];
    upper_bound = params[4];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[5];
      out += R::dbeta(w_rel, 1.1, 1.1, true);
    }
  }
  else if ( model == 3 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    beta_gaze = params[3];
    threshold_sep = params[4];
    upper_bound = params[5];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[6];
      out += R::dbeta(w_rel, 1.1, 1.1, true);
    }
    out += R::dgamma(beta_gaze, 2.0, 0.5, true);
  }
  else if ( model == 4 || model == 5 || model == 6 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    threshold_sep = params[3];
    upper_bound = params[4];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[5];
      out += R::dbeta(w_rel, 1.1, 1.1, true);
      if ( theta < 0.0 ) {
        theta = params[6];
        out += R::dgamma(theta, 2.0, 20.0, true);
      }
    }
    else {
      if ( theta < 0.0 ) {
        theta = params[5];
        out += R::dgamma(theta, 2.0, 20.0, true);
      }
    }
  }
  else if ( model == 7 || model == 8 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    beta_gaze = params[3];
    threshold_sep = params[4];
    upper_bound = params[5];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[6];
      out += R::dbeta(w_rel, 1.1, 1.1, true);
      if ( theta < 0.0 ) {
        theta = params[7];
        out += R::dgamma(theta, 2.0, 20.0, true);
      }
    }
    else {
      if ( theta < 0.0 ) {
        theta = params[6];
        out += R::dgamma(theta, 2.0, 20.0, true);
      }
    }
    out += R::dgamma(beta_gaze, 2.0, 0.5, true);
  }
  else {
    stop("Unknown model.");
  }
  
  if ( delta < 0.0 ) {
    delta = params(params.length() - 1);
    out += R::dgamma(delta, 1.01, 0.1, true);
  }
  
  out += R::dgamma(t0, 6.0, 30.0, true) +
    R::dbeta(learn_rate, 1.3, 3.7, true) +
    R::dgamma(beta_Q, 2.0, 0.5, true) +
    R::dgamma(threshold_sep, 6.0, 100.0, true) +
    R::dgamma(upper_bound, 6.0, 100.0, true);
  
  return out;
}




// MODEL OBJECTIVE FUNCTION (NEGATIVE LOG-POSTERIOR)

// [[Rcpp::export]]
double Model_Objective(const NumericVector params, const int model, const IntegerVector choice, const NumericVector RT,
                       const IntegerMatrix options, const NumericMatrix outcomes, const IntegerMatrix avail,
                       const NumericMatrix gaze, int n_opt, double min_o, double max_o, double rt_lower, double rt_upper, 
                       double w_rel=-1.0, double theta=-1.0, double delta=-1.0, double Q_init=0.5, int n_obs=-1) {
  
  double out = -1.0 * (sum(Model_LL(params, model, choice, RT, options, outcomes, avail, gaze,
                                    n_opt, min_o, max_o, rt_lower, rt_upper, w_rel, theta, delta, Q_init, n_obs)) +
                                      LogPrior(params, model, w_rel, theta, delta));
  
  return out;
}




// MODEL SIMULATION FUNCTIONS

// [[Rcpp::export]]
List Simulate_Model(const NumericVector params, const int model, const IntegerMatrix options,
                    const NumericMatrix outcomes, const IntegerMatrix avail, const NumericMatrix gaze,
                    int n_opt, double min_o, double max_o, double rt_lower, double rt_upper, 
                    double w_rel=-1.0, double theta=-1.0, double delta=-1.0, double Q_init=0.5, bool latent=false) {
  double t0 = 0.0;
  double learn_rate = 0.0;
  double beta_Q = 0.0;
  double beta_gaze = 0.0;
  double threshold_sep = 0.0;
  double upper_bound = 0.0;
  if ( model == 1 || model == 2 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    threshold_sep = params[3];
    upper_bound = params[4];
    if ( w_rel < 0.0 || w_rel > 1.0 )
      w_rel = params[5];
  }
  else if ( model == 3 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    beta_gaze = params[3];
    threshold_sep = params[4];
    upper_bound = params[5];
    if ( w_rel < 0.0 || w_rel > 1.0 )
      w_rel = params[6];
  }
  else if ( model == 4 || model == 5 || model == 6 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    threshold_sep = params[3];
    upper_bound = params[4];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[5];
      if ( theta < 0.0 ) {
        theta = params[6];
      }
    }
    else {
      if ( theta < 0.0 ) {
        theta = params[5];
      }
    }
  }
  else if ( model == 7 || model == 8 ) {
    t0 = params[0];
    learn_rate = params[1];
    beta_Q = params[2];
    beta_gaze = params[3];
    threshold_sep = params[4];
    upper_bound = params[5];
    if ( w_rel < 0.0 || w_rel > 1.0 ) {
      w_rel = params[6];
      if ( theta < 0.0 ) {
        theta = params[7];
      }
    }
    else {
      if ( theta < 0.0 ) {
        theta = params[6];
      }
    }
  }
  else {
    stop("Unknown model.");
  }
  
  if ( delta < 0.0 )
    delta = params(params.length() - 1);
  
  double drift_sd = 0.1;
  double threshold = 0.0;
  
  
  // containers for simulated choices and RTs
  int N = avail.nrow();
  IntegerVector choice(N);
  NumericVector RT(N);
  
  // initial Q values
  NumericVector Q (n_opt, Q_init);
  
  // additional containers to use later
  int K = avail.ncol();
  NumericVector drifts (K);
  NumericVector Q_trial (K);
  NumericVector R_abs (K);
  NumericVector R_rel (K);
  NumericVector sims(2);
  
  // containers for latent variables
  NumericMatrix Q_all(N, n_opt);
  NumericMatrix Q_avail(N, K);
  NumericMatrix drift_means(N, K);
  
  
  for ( int i = 0; i < N; ++i ) {
    
    // mean drift rates
    drifts = compute_drifts(model, Q[ avail(i, _) ], gaze(i, _), beta_Q, beta_gaze, theta);
    
    // trial-dependent decision threshold when delta > 0
    threshold = upper_bound + threshold_sep / (1 + delta * i);
    
    // save latent variables, if requested
    if ( latent ) {
      drift_means(i, _) = drifts;
      Q_trial = Q[ avail(i, _) ];
      Q_avail(i, _) = Q_trial;
      Q_all(i, _) = Q;
    }
    
    // simulate choice and RT
    sims = lbaSimulator( threshold, upper_bound, drifts, drift_sd, t0, rt_lower, rt_upper );
    choice[i] = static_cast<int>(sims[0]);
    RT[i] = sims[1];
    
    // range-normalized rewards
    R_abs = range_rescale( outcomes(i, _), min_o, max_o );
    R_rel = range_rescale( outcomes(i, _), min(outcomes(i, _)), max(outcomes(i, _)) );
    
    // update Q values
    Q[ options(i, _) ] = Q_update( Q[ options(i, _) ], (1-w_rel) * R_abs + w_rel * R_rel, learn_rate );
    
  }

  List out = List::create(choice, RT);
  if ( latent ) {
    out = List::create(choice, RT, Q_all, Q_avail, drift_means);
  }
  
  return out;
}


