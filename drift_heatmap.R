library(ggplot2)
library(gridExtra)

# load model functions
source('model_functions.R')

#### 1. Linear vs. non-linear linking function: Choice-RT patterns  ####

set.seed(27938)

# Q values vary between 0 and 1
Q1 <- seq(0, 1, length.out=100)
Q2 <- seq(0, 1, length.out=100)
Q_vals <- expand.grid(Q1, Q2)
N <- nrow(Q_vals)

# Q + gaze model 
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- as.numeric(Q_vals[i,1:2])
  gaze <- c(0.5, 0.5)
  drift_means <- 0.32 * Q + 0.27 * gaze
  sims <- replicate(n=500, lbaSIM(b=1086.19, A=631.09, v=drift_means, s=0.1, t0=133.44))
  prop2[i] <- mean(sims[1,] == 2)
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


# softmax(Q) + gaze model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  expQ <- exp(50 * as.numeric(Q_vals[i,1:2]))
  gaze <- c(0.5, 0.5)
  drift_means <- 0.27 * expQ / sum(expQ) + 0.28 * gaze
  sims <- replicate(n=500, lbaSIM(b=1065.47, A=613.52, v=drift_means, s=0.1, t0=135.98))
  prop2[i] <- mean(sims[1,] == 2)
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


# png('supp_choiceRT_fig1.png', width=7, height=5, units='in', res=300)
#grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
# dev.off()


#### 2. Gaze effects  ####

set.seed(98371)

# let Q values range from 0.1 to 0.9 (always tied between Option 1 and 2)
# let proportional gaze for Option 2 range from 0 to 1
Qlevels <- c(.1, .3, .5, .7, .9)
gaze2 <- seq(0, 1, length.out=100)
df <- expand.grid(Qlevels, gaze2)
N <- nrow(df)

# Q model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  #gaze <- c(1-df[i,2], df[i,2])
  drift_means <- 0.51 * Q
  sims <- replicate(n=500, lbaSIM(b=982.41, A=570.77, v=drift_means, s=0.1, t0=137.92))
  prop2[i] <- mean(sims[1,] == 2)
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model1 <- prop2
df$meanRT_model1 <- meanRT

p1 <- ggplot(data=df, aes(x=Var2, y=prop2_model1, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='Q') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

p2 <- ggplot(data=df, aes(x=Var2, y=meanRT_model1, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='Q') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# Q * gaze model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- 0.88 * Q * gaze
  sims <- replicate(n=500, lbaSIM(b=862.56, A=491.73, v=drift_means, s=0.1, t0=172.25))
  prop2[i] <- mean(sims[1,] == 2)
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model2 <- prop2
df$meanRT_model2 <- meanRT

p3 <- ggplot(data=df, aes(x=Var2, y=prop2_model2, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='Q * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

p4 <- ggplot(data=df, aes(x=Var2, y=meanRT_model2, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='Q * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# Q + gaze model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  Q <- c(df[i,1], df[i,1])
  gaze <- c(1-df[i,2], df[i,2])
  drift_means <- 0.32 * Q + 0.27 * gaze
  sims <- replicate(n=500, lbaSIM(b=1086.19, A=631.09, v=drift_means, s=0.1, t0=133.44))
  prop2[i] <- mean(sims[1,] == 2)
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model3 <- prop2
df$meanRT_model3 <- meanRT

p5 <- ggplot(data=df, aes(x=Var2, y=prop2_model3, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='Q + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

p6 <- ggplot(data=df, aes(x=Var2, y=meanRT_model3, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='Q + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q) model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  expQ <- exp(50 * c(df[i,1], df[i,1]))
  #gaze <- c(1-df[i,2], df[i,2])
  drift_means <- 0.47 * expQ / sum(expQ)
  sims <- replicate(n=500, lbaSIM(b=970.52, A=563.80, v=drift_means, s=0.1, t0=141.73))
  prop2[i] <- mean(sims[1,] == 2)
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model4 <- prop2
df$meanRT_model4 <- meanRT

p7 <- ggplot(data=df, aes(x=Var2, y=prop2_model4, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='softmax(Q)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

p8 <- ggplot(data=df, aes(x=Var2, y=meanRT_model4, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q * gaze) model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  gaze <- c(1-df[i,2], df[i,2])
  expQ <- exp(50 * c(df[i,1], df[i,1]) * gaze)
  drift_means <- 0.23 * expQ / sum(expQ)
  sims <- replicate(n=500, lbaSIM(b=626.61, A=346.69, v=drift_means, s=0.1, t0=187.43))
  prop2[i] <- mean(sims[1,] == 2)
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model5 <- prop2
df$meanRT_model5 <- meanRT

p9 <- ggplot(data=df, aes(x=Var2, y=prop2_model5, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='softmax(Q * gaze)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

p10 <- ggplot(data=df, aes(x=Var2, y=meanRT_model5, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q * gaze)') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q) * gaze model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  gaze <- c(1-df[i,2], df[i,2])
  expQ <- exp(50 * c(df[i,1], df[i,1]))
  drift_means <- 0.82 * (expQ / sum(expQ)) * gaze
  sims <- replicate(n=500, lbaSIM(b=858.48, A=488.34, v=drift_means, s=0.1, t0=175.98))
  prop2[i] <- mean(sims[1,] == 2)
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model6 <- prop2
df$meanRT_model6 <- meanRT

p11 <- ggplot(data=df, aes(x=Var2, y=prop2_model6, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='softmax(Q) * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

p12 <- ggplot(data=df, aes(x=Var2, y=meanRT_model6, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q) * gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# softmax(Q) + gaze model
prop2 <- numeric(N)
meanRT <- numeric(N)
for ( i in 1:N ) {
  gaze <- c(1-df[i,2], df[i,2])
  expQ <- exp(50 * c(df[i,1], df[i,1]))
  drift_means <- 0.27 * expQ / sum(expQ) + 0.28 * gaze
  sims <- replicate(n=500, lbaSIM(b=1065.47, A=613.52, v=drift_means, s=0.1, t0=135.98))
  prop2[i] <- mean(sims[1,] == 2)
  meanRT[i] <- mean(sims[2,])
}

df$prop2_model7 <- prop2
df$meanRT_model7 <- meanRT

p13 <- ggplot(data=df, aes(x=Var2, y=prop2_model7, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='prop2', title='softmax(Q) + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

p14 <- ggplot(data=df, aes(x=Var2, y=meanRT_model7, group=Var1)) +
  geom_line(aes(col=Var1)) +
  labs(x='gaze2', y='meanRT', title='softmax(Q) + gaze') +
  scale_color_gradient(low = "darkblue", high = "cyan", name='Q values') +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())


# png('supp_gaze_fig2.png', width=10, height=8, units='in', res=300)
grid.arrange(p1, p3, p5, p7, p9, p11, p13, nrow=3, ncol=3)
# dev.off()

# png('supp_gaze_fig3.png', width=10, height=8, units='in', res=300)
grid.arrange(p2, p4, p6, p8, p10, p12, p14, nrow=3, ncol=3)
# dev.off()
