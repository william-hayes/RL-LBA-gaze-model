library(ggplot2)
library(gridExtra)
library(viridis)
library(Rcpp)

#### Code for creating Figures S1 and S2 ####


# Model functions
# (these are also defined in model_functions.cpp, but not exported)
cppFunction(
  "NumericVector lbaSimulator(double b, double A, NumericVector v, double s, double t0,
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
}"
)


cppFunction(
  'NumericVector compute_drifts(const int model, const NumericVector& Q, const NumericVector& gaze,
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
}'
)
  


#### 1. Linear vs. non-linear linking function: Choice-RT patterns (S1 Fig)  ####

set.seed(27938)

# Q values vary between 0 and 1
Q1 <- seq(0, 1, length.out=100)
Q2 <- seq(0, 1, length.out=100)
Q_vals <- expand.grid(Q1, Q2)
N <- nrow(Q_vals)

# Q + gaze model (Model 3)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- as.numeric(Q_vals[i,1:2])
  gaze <- c(0.5, 0.5)
  drift_means <- compute_drifts(model=3, Q=Q, gaze=gaze, b_Q=0.32, b_gaze=0.27, theta=0) 
  sims <- replicate(n=500, lbaSimulator(b=1086.19, A=631.09, v=drift_means, s=0.1, t0=133.44,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

Q_vals$prop2_model3 <- prop2
Q_vals$meanRT_model3 <- meanRT

p1 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=prop2_model3)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000", midpoint=0.5, name='prop2') +
  labs(x='Q1', y='Q2', title='Q + gaze') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5)) 

p2 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=meanRT_model3)) +
  geom_tile() +
  scale_fill_gradient(low = "grey90", high='black', name='meanRT') +
  labs(x='Q1', y='Q2', title='Q + gaze') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5))


# softmax(Q) + gaze model (Model 7)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- as.numeric(Q_vals[i,1:2])
  gaze <- c(0.5, 0.5)
  drift_means <- compute_drifts(model=7, Q=Q, gaze=gaze, b_Q=0.28, b_gaze=0.27, theta=18.35) 
  sims <- replicate(n=500, lbaSimulator(b=1069.49, A=615.96, v=drift_means, s=0.1, t0=135.13,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

Q_vals$prop2_model7 <- prop2
Q_vals$meanRT_model7 <- meanRT

p3 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=prop2_model7)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000", midpoint=0.5, name='prop2') +
  labs(x='Q1', y='Q2', title='softmax(Q) + gaze') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5))

p4 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=meanRT_model7)) +
  geom_tile() +
  scale_fill_gradient(low = "grey90", high='black', name='meanRT') +
  labs(x='Q1', y='Q2', title='softmax(Q) + gaze') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5)) 



# softmax(Q + gaze) model (Model 8)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- as.numeric(Q_vals[i,1:2])
  gaze <- c(0.5, 0.5)
  drift_means <- compute_drifts(model=8, Q=Q, gaze=gaze, b_Q=0.47, b_gaze=0.28, theta=8.82) 
  sims <- replicate(n=500, lbaSimulator(b=993.64, A=562.17, v=drift_means, s=0.1, t0=139.61,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

Q_vals$prop2_model8 <- prop2
Q_vals$meanRT_model8 <- meanRT

p5 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=prop2_model8)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000", midpoint=0.5, name='prop2') +
  labs(x='Q1', y='Q2', title='softmax(Q + gaze)') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5))

p6 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=meanRT_model8)) +
  geom_tile() +
  scale_fill_gradient(low = "grey90", high='black', name='meanRT') +
  labs(x='Q1', y='Q2', title='softmax(Q + gaze)') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5)) 


# png('S1_Fig.png', width=7, height=8, units='in', res=300)
# grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
# dev.off()



#### 2. Gaze effects with equal Q-values (S2 Fig) ####

set.seed(98371)

# let Q values range from 0.1 to 0.9 (always tied between Option 1 and 2)
# let proportional gaze for Option 2 range from 0 to 1
Qlevels <- seq(0.1, 0.9, 0.01)
gaze2 <- seq(0, 1, 0.1)
df <- expand.grid(Qlevels, gaze2)
N <- nrow(df)

# Q model (Model 1)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=1, Q=Q, gaze=gaze, b_Q=0.51, b_gaze=0, theta=0)
  sims <- replicate(n=500, lbaSimulator(b=982.41, A=570.77, v=drift_means, s=0.1, t0=137.92,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model1 <- prop2
df$meanRT_model1 <- meanRT

p1 <- ggplot(data=df, aes(x=Var2, y=prop2_model1, group=Var1)) +
  geom_line(aes(col=Var1), show.legend=F) +
  labs(x='gaze2', y='prop2', title='Q') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p2 <- ggplot(data=df, aes(x=Var2, y=meanRT_model1, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='Q') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# Q * gaze model (Model 2)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=2, Q=Q, gaze=gaze, b_Q=0.88, b_gaze=0, theta=0)
  sims <- replicate(n=500, lbaSimulator(b=862.56, A=491.73, v=drift_means, s=0.1, t0=172.25,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model2 <- prop2
df$meanRT_model2 <- meanRT

p3 <- ggplot(data=df, aes(x=Var2, y=prop2_model2, group=Var1)) +
  geom_line(aes(col=Var1), show.legend=F) +
  labs(x='gaze2', y='prop2', title='Q * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p4 <- ggplot(data=df, aes(x=Var2, y=meanRT_model2, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='Q * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# Q + gaze model (Model 3)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=3, Q=Q, gaze=gaze, b_Q=0.32, b_gaze=0.27, theta=0)
  sims <- replicate(n=500, lbaSimulator(b=1086.19, A=631.09, v=drift_means, s=0.1, t0=133.44,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model3 <- prop2
df$meanRT_model3 <- meanRT

p5 <- ggplot(data=df, aes(x=Var2, y=prop2_model3, group=Var1)) +
  geom_line(aes(col=Var1), show.legend=F) +
  labs(x='gaze2', y='prop2', title='Q + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p6 <- ggplot(data=df, aes(x=Var2, y=meanRT_model3, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='Q + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q) model (Model 4)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=4, Q=Q, gaze=gaze, b_Q=0.48, b_gaze=0, theta=15.14)
  sims <- replicate(n=500, lbaSimulator(b=978.36, A=568.33, v=drift_means, s=0.1, t0=140.50,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model4 <- prop2
df$meanRT_model4 <- meanRT

p7 <- ggplot(data=df, aes(x=Var2, y=prop2_model4, group=Var1)) +
  geom_line(aes(col=Var1), show.legend=F) +
  labs(x='gaze2', y='prop2', title='softmax(Q)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p8 <- ggplot(data=df, aes(x=Var2, y=meanRT_model4, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q * gaze) model (Model 5)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=5, Q=Q, gaze=gaze, b_Q=0.47, b_gaze=0, theta=5.63)
  sims <- replicate(n=500, lbaSimulator(b=971.09, A=557.79, v=drift_means, s=0.1, t0=140.87,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model5 <- prop2
df$meanRT_model5 <- meanRT

p9 <- ggplot(data=df, aes(x=Var2, y=prop2_model5, group=Var1)) +
  geom_line(aes(col=Var1), show.legend=F) +
  labs(x='gaze2', y='prop2', title='softmax(Q * gaze)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p10 <- ggplot(data=df, aes(x=Var2, y=meanRT_model5, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q * gaze)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q) * gaze model (Model 6)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=6, Q=Q, gaze=gaze, b_Q=0.83, b_gaze=0, theta=14.90)
  sims <- replicate(n=500, lbaSimulator(b=863.08, A=491.66, v=drift_means, s=0.1, t0=174.99,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model6 <- prop2
df$meanRT_model6 <- meanRT

p11 <- ggplot(data=df, aes(x=Var2, y=prop2_model6, group=Var1)) +
  geom_line(aes(col=Var1), show.legend=F) +
  labs(x='gaze2', y='prop2', title='softmax(Q) * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p12 <- ggplot(data=df, aes(x=Var2, y=meanRT_model6, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q) * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q) + gaze model (Model 7)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=7, Q=Q, gaze=gaze, b_Q=0.28, b_gaze=0.27, theta=18.35)
  sims <- replicate(n=500, lbaSimulator(b=1069.49, A=615.96, v=drift_means, s=0.1, t0=135.13,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model7 <- prop2
df$meanRT_model7 <- meanRT

p13 <- ggplot(data=df, aes(x=Var2, y=prop2_model7, group=Var1)) +
  geom_line(aes(col=Var1), show.legend=F) +
  labs(x='gaze2', y='prop2', title='softmax(Q) + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p14 <- ggplot(data=df, aes(x=Var2, y=meanRT_model7, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q) + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q + gaze) model (Model 8)
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- compute_drifts(model=8, Q=Q, gaze=gaze, b_Q=0.47, b_gaze=0.28, theta=8.82)
  sims <- replicate(n=500, lbaSimulator(b=993.64, A=562.17, v=drift_means, s=0.1, t0=139.61,
                                        rt_lower=250, rt_upper=10000))
  prop2[i] <- mean(sims[1,] == 1) # 0-based indices
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model8 <- prop2
df$meanRT_model8 <- meanRT

p15 <- ggplot(data=df, aes(x=Var2, y=prop2_model8, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='softmax(Q + gaze)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid = element_blank(),
        legend.position = 'inside',
        legend.position.inside = c(.8, .4),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))

p16 <- ggplot(data=df, aes(x=Var2, y=meanRT_model8, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q + gaze)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# png('S2_Fig.png', width=12, height=6, units='in', res=300)
# grid.arrange(p1, p3, p5, p7, p9, p11, p13, p15, nrow=2, ncol=4)
# dev.off()



#### 3. NOT USED --- Gaze effects with unequal Q-values  ####

# set.seed(44271)
# 
# # let Q differences range from 
# # let proportional gaze for Option 2 range from 0 to 1
# Qdiffs <- seq(-0.4, 0.4, .01)
# gaze2 <- seq(0, 1, 0.1)
# df <- expand.grid(Qdiffs, gaze2)
# N <- nrow(df)
# 
# # Q model (Model 1)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=1, Q=Q, gaze=gaze, b_Q=0.51, b_gaze=0, theta=0)
#   sims <- replicate(n=500, lbaSimulator(b=982.41, A=570.77, v=drift_means, s=0.1, t0=137.92,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model1 <- prop2
# df$meanRT_model1 <- meanRT
# 
# p1 <- ggplot(data=df, aes(x=Var2, y=prop2_model1, group=Var1)) +
#   geom_line(aes(col=Var1), show.legend=F) +
#   labs(x='gaze2', y='prop2', title='Q') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p2 <- ggplot(data=df, aes(x=Var2, y=meanRT_model1, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='Q') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())
# 
# 
# # Q * gaze model (Model 2)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=2, Q=Q, gaze=gaze, b_Q=0.88, b_gaze=0, theta=0)
#   sims <- replicate(n=500, lbaSimulator(b=862.56, A=491.73, v=drift_means, s=0.1, t0=172.25,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model2 <- prop2
# df$meanRT_model2 <- meanRT
# 
# p3 <- ggplot(data=df, aes(x=Var2, y=prop2_model2, group=Var1)) +
#   geom_line(aes(col=Var1), show.legend=F) +
#   labs(x='gaze2', y='prop2', title='Q * gaze') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p4 <- ggplot(data=df, aes(x=Var2, y=meanRT_model2, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='Q * gaze') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())
# 
# 
# # Q + gaze model (Model 3)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=3, Q=Q, gaze=gaze, b_Q=0.32, b_gaze=0.27, theta=0)
#   sims <- replicate(n=500, lbaSimulator(b=1086.19, A=631.09, v=drift_means, s=0.1, t0=133.44,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model3 <- prop2
# df$meanRT_model3 <- meanRT
# 
# p5 <- ggplot(data=df, aes(x=Var2, y=prop2_model3, group=Var1)) +
#   geom_line(aes(col=Var1), show.legend=F) +
#   labs(x='gaze2', y='prop2', title='Q + gaze') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p6 <- ggplot(data=df, aes(x=Var2, y=meanRT_model3, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='Q + gaze') +
#   scale_color_gradient(low = "darkblue", high = "cyan", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())
# 
# 
# # softmax(Q) model (Model 4)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=4, Q=Q, gaze=gaze, b_Q=0.48, b_gaze=0, theta=15.14)
#   sims <- replicate(n=500, lbaSimulator(b=978.36, A=568.33, v=drift_means, s=0.1, t0=140.50,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model4 <- prop2
# df$meanRT_model4 <- meanRT
# 
# p7 <- ggplot(data=df, aes(x=Var2, y=prop2_model4, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='prop2', title='softmax(Q)') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         legend.position = 'inside',
#         legend.position.inside = c(.8, .4),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p8 <- ggplot(data=df, aes(x=Var2, y=meanRT_model4, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='softmax(Q)') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())
# 
# 
# # softmax(Q * gaze) model (Model 5)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=5, Q=Q, gaze=gaze, b_Q=0.47, b_gaze=0, theta=5.63)
#   sims <- replicate(n=500, lbaSimulator(b=971.09, A=557.79, v=drift_means, s=0.1, t0=140.87,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model5 <- prop2
# df$meanRT_model5 <- meanRT
# 
# p9 <- ggplot(data=df, aes(x=Var2, y=prop2_model5, group=Var1)) +
#   geom_line(aes(col=Var1), show.legend=F) +
#   labs(x='gaze2', y='prop2', title='softmax(Q * gaze)') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p10 <- ggplot(data=df, aes(x=Var2, y=meanRT_model5, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='softmax(Q * gaze)') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())
# 
# 
# # softmax(Q) * gaze model (Model 6)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=6, Q=Q, gaze=gaze, b_Q=0.83, b_gaze=0, theta=14.90)
#   sims <- replicate(n=500, lbaSimulator(b=863.08, A=491.66, v=drift_means, s=0.1, t0=174.99,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model6 <- prop2
# df$meanRT_model6 <- meanRT
# 
# p11 <- ggplot(data=df, aes(x=Var2, y=prop2_model6, group=Var1)) +
#   geom_line(aes(col=Var1), show.legend=F) +
#   labs(x='gaze2', y='prop2', title='softmax(Q) * gaze') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p12 <- ggplot(data=df, aes(x=Var2, y=meanRT_model6, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='softmax(Q) * gaze') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())
# 
# 
# # softmax(Q) + gaze model (Model 7)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=7, Q=Q, gaze=gaze, b_Q=0.28, b_gaze=0.27, theta=18.35)
#   sims <- replicate(n=500, lbaSimulator(b=1069.49, A=615.96, v=drift_means, s=0.1, t0=135.13,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model7 <- prop2
# df$meanRT_model7 <- meanRT
# 
# p13 <- ggplot(data=df, aes(x=Var2, y=prop2_model7, group=Var1)) +
#   geom_line(aes(col=Var1), show.legend=F) +
#   labs(x='gaze2', y='prop2', title='softmax(Q) + gaze') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p14 <- ggplot(data=df, aes(x=Var2, y=meanRT_model7, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='softmax(Q) + gaze') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())
# 
# 
# # softmax(Q + gaze) model (Model 8)
# prop2 <- numeric(N)
# meanRT <- numeric(N)
# for ( i in 1:N ) {
#   Q <- c(0.5 - df[i,1]/2, 0.5 + df[i,1]/2)
#   gaze <- c(1-df[i,2], df[i,2])
#   drift_means <- compute_drifts(model=8, Q=Q, gaze=gaze, b_Q=0.47, b_gaze=0.28, theta=8.82)
#   sims <- replicate(n=500, lbaSimulator(b=993.64, A=562.17, v=drift_means, s=0.1, t0=139.61,
#                                         rt_lower=250, rt_upper=10000))
#   prop2[i] <- mean(sims[1,] == 1) # 0-based indices
#   meanRT[i] <- mean(sims[2,])
# }
# 
# df$prop2_model8 <- prop2
# df$meanRT_model8 <- meanRT
# 
# p15 <- ggplot(data=df, aes(x=Var2, y=prop2_model8, group=Var1)) +
#   geom_line(aes(col=Var1), show.legend=F) +
#   labs(x='gaze2', y='prop2', title='softmax(Q + gaze)') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# 
# p16 <- ggplot(data=df, aes(x=Var2, y=meanRT_model8, group=Var1)) +
#   geom_line(aes(col=Var1)) +
#   labs(x='gaze2', y='meanRT', title='softmax(Q + gaze)') +
#   scale_color_gradient(low = "darkred", high = "yellow", name='Q2 - Q1') +
#   theme(plot.title = element_text(hjust=0.5),
#         panel.grid.minor.x = element_blank())



#### 4. NOT USED --- Visualizing the effect of gaze on mean drift rates ####

# set.seed(73644)
# 
# # Q values vary between 0 and 1
# Q1 <- seq(0, 1, length.out=100)
# Q2 <- seq(0, 1, length.out=100)
# Q_vals <- expand.grid(Q1, Q2)
# N <- nrow(Q_vals)
# 
# b_Q <- 0.36
# b_gaze <- 0.41
# theta <- 6.68
# 
# model2_effect <- function(Q_i, Q_j) {
#   b_Q * Q_i
# }
# 
# model5_effect <- function(Q_i, Q_j) {
#   S_i <- 1 / (1 + exp(-theta * ((Q_i + Q_j)*.5 - Q_j)))
#   b_Q * theta * (Q_i + Q_j) * S_i * (1 - S_i)
# }
# 
# model6_effect <- function(Q_i, Q_j) {
#   S_i <- 1 / (1 + exp(-theta*(Q_i - Q_j)))
#   b_Q * S_i
# }
# 
# model8_effect <- function(Q_i, Q_j) {
#   S_i <- 1 / (1 + exp(-theta*((Q_i - Q_j) + b_gaze*(2*.5 - 1))))
#   2 * b_Q * theta * b_gaze * S_i * (1 - S_i)
# }
# 
# model2_results <- numeric(N)
# model5_results <- numeric(N)
# model6_results <- numeric(N)
# model8_results <- numeric(N)
# for ( i in 1:N ) {
#   model2_results[i] <- model2_effect(Q_vals[i,1], Q_vals[i,2])
#   model5_results[i] <- model5_effect(Q_vals[i,1], Q_vals[i,2])
#   model6_results[i] <- model6_effect(Q_vals[i,1], Q_vals[i,2])
#   model8_results[i] <- model8_effect(Q_vals[i,1], Q_vals[i,2])
# }
# 
# Q_vals$model2 <- model2_results
# Q_vals$model5 <- model5_results
# Q_vals$model6 <- model6_results
# Q_vals$model8 <- model8_results
# 
# p1 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=model2)) +
#   geom_tile() +
#   scale_fill_viridis(name='Gaze\neffect') +
#   labs(x='Q1', y='Q2', title='Model 2: Q*gaze') +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   theme(plot.title = element_text(hjust=0.5)) 
# 
# p2 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=model5)) +
#   geom_tile() +
#   scale_fill_viridis(name='Gaze\neffect') +
#   labs(x='Q1', y='Q2', title='Model 5: softmax(Q*gaze)') +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   theme(plot.title = element_text(hjust=0.5)) 
# 
# p3 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=model6)) +
#   geom_tile() +
#   scale_fill_viridis(name='Gaze\neffect') +
#   labs(x='Q1', y='Q2', title='Model 6: softmax(Q)*gaze') +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   theme(plot.title = element_text(hjust=0.5))
# 
# p4 <- ggplot(data=Q_vals, aes(x=Var1, y=Var2, fill=model8)) +
#   geom_tile() +
#   scale_fill_viridis(name='Gaze\neffect') +
#   labs(x='Q1', y='Q2', title='Model 8: softmax(Q + gaze)') +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   theme(plot.title = element_text(hjust=0.5)) 
