set.seed(93586)

# read in the functions for the RL-LBA model
sourceCpp('../../model_functions.cpp') 
source('../../model_utils.R')

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
model8_fits <- readRDS('results/model8_fits.RDS')
model4FT_fits <- readRDS('results/free-theta/model4_fits.RDS')
model5FT_fits <- readRDS('results/free-theta/model5_fits.RDS')
model6FT_fits <- readRDS('results/free-theta/model6_fits.RDS')
model7FT_fits <- readRDS('results/free-theta/model7_fits.RDS')
model8FT_fits <- readRDS('results/free-theta/model8_fits.RDS')

# parameter estimates
params1 <- t(sapply(model1_fits, function(X) X$optim$bestmem))
params2 <- t(sapply(model2_fits, function(X) X$optim$bestmem))
params3 <- t(sapply(model3_fits, function(X) X$optim$bestmem))
params4 <- t(sapply(model4_fits, function(X) X$optim$bestmem))
params5 <- t(sapply(model5_fits, function(X) X$optim$bestmem))
params6 <- t(sapply(model6_fits, function(X) X$optim$bestmem))
params7 <- t(sapply(model7_fits, function(X) X$optim$bestmem))
params8 <- t(sapply(model8_fits, function(X) X$optim$bestmem))
params4FT <- t(sapply(model4FT_fits, function(X) X$optim$bestmem))
params5FT <- t(sapply(model5FT_fits, function(X) X$optim$bestmem))
params6FT <- t(sapply(model6FT_fits, function(X) X$optim$bestmem))
params7FT <- t(sapply(model7FT_fits, function(X) X$optim$bestmem))
params8FT <- t(sapply(model8FT_fits, function(X) X$optim$bestmem))


# function for recoding simulated choices to 1=correct, 0=incorrect
recode_simulated_choices <- function(datasets, simulations, n_sims) {
  # datasets list and simulations list should have the same length
  stopifnot(length(datasets) == length(simulations))

  for (i in 1:length(datasets)) {
    for (j in 1:n_sims) {
      simulations[[i]][,1,j] <- 1*(((datasets[[i]]$EV_1 > datasets[[i]]$EV_2) & (simulations[[i]][,1,j] == 0)) |
                                     ((datasets[[i]]$EV_1 < datasets[[i]]$EV_2) & (simulations[[i]][,1,j] == 1))) +
        0*(((datasets[[i]]$EV_1 > datasets[[i]]$EV_2) & (simulations[[i]][,1,j] == 1)) |
             ((datasets[[i]]$EV_1 < datasets[[i]]$EV_2) & (simulations[[i]][,1,j] == 0)))
    }
  }

  return (simulations)
}


###### Simulate models using best-fitting parameters #######

model1_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=1, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params1[i,]))
})

model2_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=2, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params2[i,]))
})

model3_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=3, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params3[i,]))
})

model4_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=4, data=modeling_data[[i]], w_rel=0, theta=50, delta=0)
  replicate(n=100, sim(params4[i,]))
})

model5_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=5, data=modeling_data[[i]], w_rel=0, theta=50, delta=0)
  replicate(n=100, sim(params5[i,]))
})

model6_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=6, data=modeling_data[[i]], w_rel=0, theta=50, delta=0)
  replicate(n=100, sim(params6[i,]))
})

model7_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=7, data=modeling_data[[i]], w_rel=0, theta=50, delta=0)
  replicate(n=100, sim(params7[i,]))
})

model8_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=8, data=modeling_data[[i]], w_rel=0, theta=50, delta=0)
  replicate(n=100, sim(params8[i,]))
})

model4FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=4, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params4FT[i,]))
})

model5FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=5, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params5FT[i,]))
})

model6FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=6, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params6FT[i,]))
})

model7FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=7, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params7FT[i,]))
})

model8FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=8, data=modeling_data[[i]], w_rel=0, delta=0)
  replicate(n=100, sim(params8FT[i,]))
})


# recode for accuracy (1=correct, 0=incorrect)
model1_sims <- recode_simulated_choices(datasets, model1_sims, 100)
model2_sims <- recode_simulated_choices(datasets, model2_sims, 100)
model3_sims <- recode_simulated_choices(datasets, model3_sims, 100)
model4_sims <- recode_simulated_choices(datasets, model4_sims, 100)
model5_sims <- recode_simulated_choices(datasets, model5_sims, 100)
model6_sims <- recode_simulated_choices(datasets, model6_sims, 100)
model7_sims <- recode_simulated_choices(datasets, model7_sims, 100)
model8_sims <- recode_simulated_choices(datasets, model8_sims, 100)
model4FT_sims <- recode_simulated_choices(datasets, model4FT_sims, 100)
model5FT_sims <- recode_simulated_choices(datasets, model5FT_sims, 100)
model6FT_sims <- recode_simulated_choices(datasets, model6FT_sims, 100)
model7FT_sims <- recode_simulated_choices(datasets, model7FT_sims, 100)
model8FT_sims <- recode_simulated_choices(datasets, model8FT_sims, 100)



saveRDS(model1_sims, file='results/model1_sims.RDS')
saveRDS(model2_sims, file='results/model2_sims.RDS')
saveRDS(model3_sims, file='results/model3_sims.RDS')
saveRDS(model4_sims, file='results/model4_sims.RDS')
saveRDS(model5_sims, file='results/model5_sims.RDS')
saveRDS(model6_sims, file='results/model6_sims.RDS')
saveRDS(model7_sims, file='results/model7_sims.RDS')
saveRDS(model8_sims, file='results/model8_sims.RDS')
saveRDS(model4FT_sims, file='results/free-theta/model4_sims.RDS')
saveRDS(model5FT_sims, file='results/free-theta/model5_sims.RDS')
saveRDS(model6FT_sims, file='results/free-theta/model6_sims.RDS')
saveRDS(model7FT_sims, file='results/free-theta/model7_sims.RDS')
saveRDS(model8FT_sims, file='results/free-theta/model8_sims.RDS')
