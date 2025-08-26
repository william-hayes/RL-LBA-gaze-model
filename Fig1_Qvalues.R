# set a random seed to get reproducible results
set.seed(12345)

# target option: Normal(30, 2)
targetOption <- round(rnorm(n=30, mean=30, sd=2))

# contextual options for the positive skew context
posSkewOption1 <- round(rnorm(n=30, mean=20, sd=2))
posSkewOption2 <- round(rnorm(n=30, mean=23, sd=2))
posSkewOption3 <- round(rnorm(n=30, mean=47, sd=2))

# contextual options for the negative skew context
negSkewOption1 <- round(rnorm(n=30, mean=13, sd=2))
negSkewOption2 <- round(rnorm(n=30, mean=37, sd=2))
negSkewOption3 <- round(rnorm(n=30, mean=40, sd=2))

# put all of the outcomes for each context into a matrix
negSkewContext <- cbind(Option_A=negSkewOption1, 
                        Option_D=targetOption, 
                        Option_E=negSkewOption2,
                        Option_F=negSkewOption3)
posSkewContext <- cbind(Option_B=posSkewOption1, 
                        Option_C=posSkewOption2,
                        Option_D=targetOption,
                        Option_G=posSkewOption3)

source('model_functions.R')

min_outcome <- min(min(posSkewContext, negSkewContext))
max_outcome <- max(max(posSkewContext, negSkewContext))

Q_pos <- rbind(rep(.5, 4))
Q_neg <- rbind(rep(.5, 4))

for (i in 1:29) {
  if (runif(1) < .5) {
    Q_pos_updated <- delta_update(Q_pos[i,], minmax_scaling(posSkewContext[i,], min_outcome, max_outcome), 0.1)
    Q_pos <- rbind(Q_pos, Q_pos_updated)
    Q_neg <- rbind(Q_neg, Q_neg[i,])
  } else {
    Q_neg_updated <- delta_update(Q_neg[i,], minmax_scaling(negSkewContext[i,], min_outcome, max_outcome), 0.1)
    Q_neg <- rbind(Q_neg, Q_neg_updated)
    Q_pos <- rbind(Q_pos, Q_pos[i,])
  }
}

png('Q_values.png', width=3.5, height=3, units='in', res=300)
par(mar=(c(2.5,2.5,1,.5)))
plot(Q_pos[,1], type='l', ylim=c(0,1), col='red', las=1, xlab='', ylab='')
lines(Q_pos[,2], type='l', col='red')
lines(Q_pos[,3], type='l', col='red')
lines(Q_pos[,4], type='l', col='red')
lines(Q_neg[,1], type='l', col='blue')
lines(Q_neg[,2], type='l', col='blue')
lines(Q_neg[,3], type='l', col='blue')
lines(Q_neg[,4], type='l', col='blue')
dev.off()
