set.seed(18290)

# load in the RL-LBA model functions
source('../../model_functions.R')

# load in list of individual datasets
source('load_data.R')

# load model fit results
model1_fits <- readRDS('results/model1_fits.RDS')
model2_fits <- readRDS('results/model2_fits.RDS')
model3_fits <- readRDS('results/model3_fits.RDS')
model4_fits <- readRDS('results/model4_fits.RDS')
model5_fits <- readRDS('results/model5_fits.RDS')
model6_fits <- readRDS('results/model6_fits.RDS')
model7_fits <- readRDS('results/model7_fits.RDS')

# parameter estimates
params1 <- t(sapply(model1_fits, function(X) X$optim$bestmem))
params2 <- t(sapply(model2_fits, function(X) X$optim$bestmem))
params3 <- t(sapply(model3_fits, function(X) X$optim$bestmem))
params4 <- t(sapply(model4_fits, function(X) X$optim$bestmem))
params5 <- t(sapply(model5_fits, function(X) X$optim$bestmem))
params6 <- t(sapply(model6_fits, function(X) X$optim$bestmem))
params7 <- t(sapply(model7_fits, function(X) X$optim$bestmem))


# function for recoding simulated choices to 1=correct, 0=incorrect
recode_simulated_choices <- function(datasets, simulations, n_sims) {
  # datasets list and simulations list should have the same length
  stopifnot(length(datasets) == length(simulations))
  
  for (i in 1:length(datasets)) {
    for (j in 1:n_sims) {
      simulations[[i]][,1,j] <- 1*(((datasets[[i]]$correct_resp == 'left') & (simulations[[i]][,1,j] == 1)) |
                                     ((datasets[[i]]$correct_resp == 'right') & (simulations[[i]][,1,j] == 2))) +
        0*(((datasets[[i]]$correct_resp == 'left') & (simulations[[i]][,1,j] == 2)) |
             ((datasets[[i]]$correct_resp == 'right') & (simulations[[i]][,1,j] == 1)))
    }
  }
  
  return (simulations)
}

###### Simulate models using best-fitting parameters #######

model1_sims <- lapply(1:length(datasets), function(i) {
  replicate(n=100, RL_LBA_Model(params1[i,], version=1, options=datasets[[i]]$options, outcomes=datasets[[i]]$outcomes,
                                gaze=datasets[[i]]$gaze_pre, simulate=TRUE))
})

model2_sims <- lapply(1:length(datasets), function(i) {
  replicate(n=100, RL_LBA_Model(params2[i,], version=2, options=datasets[[i]]$options, outcomes=datasets[[i]]$outcomes,
                                gaze=datasets[[i]]$gaze_pre, simulate=TRUE))
})

model3_sims <- lapply(1:length(datasets), function(i) {
  replicate(n=100, RL_LBA_Model(params3[i,], version=3, options=datasets[[i]]$options, outcomes=datasets[[i]]$outcomes,
                                gaze=datasets[[i]]$gaze_pre, simulate=TRUE))
})

model4_sims <- lapply(1:length(datasets), function(i) {
  replicate(n=100, RL_LBA_Model(params4[i,], version=4, options=datasets[[i]]$options, outcomes=datasets[[i]]$outcomes,
                                gaze=datasets[[i]]$gaze_pre, simulate=TRUE))
})

model5_sims <- lapply(1:length(datasets), function(i) {
  replicate(n=100, RL_LBA_Model(params5[i,], version=5, options=datasets[[i]]$options, outcomes=datasets[[i]]$outcomes,
                                gaze=datasets[[i]]$gaze_pre, simulate=TRUE))
})

model6_sims <- lapply(1:length(datasets), function(i) {
  replicate(n=100, RL_LBA_Model(params6[i,], version=6, options=datasets[[i]]$options, outcomes=datasets[[i]]$outcomes,
                                gaze=datasets[[i]]$gaze_pre, simulate=TRUE))
})

model7_sims <- lapply(1:length(datasets), function(i) {
  replicate(n=100, RL_LBA_Model(params7[i,], version=7, options=datasets[[i]]$options, outcomes=datasets[[i]]$outcomes,
                                gaze=datasets[[i]]$gaze_pre, simulate=TRUE))
})


# recode for accuracy (1=correct, 0=incorrect)
model1_sims <- recode_simulated_choices(datasets, model1_sims, 100)
model2_sims <- recode_simulated_choices(datasets, model2_sims, 100)
model3_sims <- recode_simulated_choices(datasets, model3_sims, 100)
model4_sims <- recode_simulated_choices(datasets, model4_sims, 100)
model5_sims <- recode_simulated_choices(datasets, model5_sims, 100)
model6_sims <- recode_simulated_choices(datasets, model6_sims, 100)
model7_sims <- recode_simulated_choices(datasets, model7_sims, 100)


# saveRDS(model1_sims, file='results/model1_sims.RDS')
# saveRDS(model2_sims, file='results/model2_sims.RDS')
# saveRDS(model3_sims, file='results/model3_sims.RDS')
# saveRDS(model4_sims, file='results/model4_sims.RDS')
# saveRDS(model5_sims, file='results/model5_sims.RDS')
# saveRDS(model6_sims, file='results/model6_sims.RDS')
# saveRDS(model7_sims, file='results/model7_sims.RDS')