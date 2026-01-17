set.seed(18290)

# for trial-dependent thresholds, set tdep_thresh to TRUE
tdep_thresh <- FALSE

# read in the functions for the RL-LBA model
sourceCpp('../../model_functions.cpp') 
source('../../model_utils.R')

# load in list of individual datasets
source('load_data.R')

# load model fit results
if ( !tdep_thresh ) {
  model1_fits <- readRDS('results/model1_fits.RDS')
  model2_fits <- readRDS('results/model2_fits.RDS')
  model3_fits <- readRDS('results/model3_fits.RDS')
  model4FT_fits <- readRDS('results/free-theta/model4_fits.RDS')
  model5FT_fits <- readRDS('results/free-theta/model5_fits.RDS')
  model6FT_fits <- readRDS('results/free-theta/model6_fits.RDS')
  model7FT_fits <- readRDS('results/free-theta/model7_fits.RDS')
  model8FT_fits <- readRDS('results/free-theta/model8_fits.RDS')
} else {
  model1_fits <- readRDS('results/tdep_thresh_model1_fits.RDS')
  model2_fits <- readRDS('results/tdep_thresh_model2_fits.RDS')
  model3_fits <- readRDS('results/tdep_thresh_model3_fits.RDS')
  model4FT_fits <- readRDS('results/free-theta/tdep_thresh_model4_fits.RDS')
  model5FT_fits <- readRDS('results/free-theta/tdep_thresh_model5_fits.RDS')
  model6FT_fits <- readRDS('results/free-theta/tdep_thresh_model6_fits.RDS')
  model7FT_fits <- readRDS('results/free-theta/tdep_thresh_model7_fits.RDS')
  model8FT_fits <- readRDS('results/free-theta/tdep_thresh_model8_fits.RDS')
}

# parameter estimates
params1 <- t(sapply(model1_fits, function(X) X$optim$bestmem))
params2 <- t(sapply(model2_fits, function(X) X$optim$bestmem))
params3 <- t(sapply(model3_fits, function(X) X$optim$bestmem))
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
      simulations[[i]][,1,j] <- 1*(((datasets[[i]]$correct_resp == 'left') & (simulations[[i]][,1,j] == 0)) |
                                     ((datasets[[i]]$correct_resp == 'right') & (simulations[[i]][,1,j] == 1))) +
        0*(((datasets[[i]]$correct_resp == 'left') & (simulations[[i]][,1,j] == 1)) |
             ((datasets[[i]]$correct_resp == 'right') & (simulations[[i]][,1,j] == 0)))
    }
  }
  
  return (simulations)
}

###### Simulate models using best-fitting parameters #######

model1_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=1, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params1[i,]))
})

model2_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=2, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params2[i,]))
})

model3_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=3, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params3[i,]))
})

model4FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=4, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params4FT[i,]))
})

model5FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=5, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params5FT[i,]))
})

model6FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=6, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params6FT[i,]))
})

model7FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=7, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params7FT[i,]))
})

model8FT_sims <- lapply(1:length(modeling_data), function(i) {
  sim <- simFunction(model=8, data=modeling_data[[i]], delta=if(tdep_thresh) NULL else 0)
  replicate(n=100, sim(params8FT[i,]))
})


# recode for accuracy (1=correct, 0=incorrect)
model1_sims <- recode_simulated_choices(datasets, model1_sims, 100)
model2_sims <- recode_simulated_choices(datasets, model2_sims, 100)
model3_sims <- recode_simulated_choices(datasets, model3_sims, 100)
model4FT_sims <- recode_simulated_choices(datasets, model4FT_sims, 100)
model5FT_sims <- recode_simulated_choices(datasets, model5FT_sims, 100)
model6FT_sims <- recode_simulated_choices(datasets, model6FT_sims, 100)
model7FT_sims <- recode_simulated_choices(datasets, model7FT_sims, 100)
model8FT_sims <- recode_simulated_choices(datasets, model8FT_sims, 100)




if ( !tdep_thresh ) {
  saveRDS(model1_sims, file='results/model1_sims.RDS')
  saveRDS(model2_sims, file='results/model2_sims.RDS')
  saveRDS(model3_sims, file='results/model3_sims.RDS')
  saveRDS(model4FT_sims, file='results/free-theta/model4_sims.RDS')
  saveRDS(model5FT_sims, file='results/free-theta/model5_sims.RDS')
  saveRDS(model6FT_sims, file='results/free-theta/model6_sims.RDS')
  saveRDS(model7FT_sims, file='results/free-theta/model7_sims.RDS')
  saveRDS(model8FT_sims, file='results/free-theta/model8_sims.RDS')
} else {
  saveRDS(model1_sims, file='results/tdep_thresh_model1_sims.RDS')
  saveRDS(model2_sims, file='results/tdep_thresh_model2_sims.RDS')
  saveRDS(model3_sims, file='results/tdep_thresh_model3_sims.RDS')
  saveRDS(model4FT_sims, file='results/free-theta/tdep_thresh_model4_sims.RDS')
  saveRDS(model5FT_sims, file='results/free-theta/tdep_thresh_model5_sims.RDS')
  saveRDS(model6FT_sims, file='results/free-theta/tdep_thresh_model6_sims.RDS')
  saveRDS(model7FT_sims, file='results/free-theta/tdep_thresh_model7_sims.RDS')
  saveRDS(model8FT_sims, file='results/free-theta/tdep_thresh_model8_sims.RDS')
}

