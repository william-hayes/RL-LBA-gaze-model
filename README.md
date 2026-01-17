# A Reinforcement Learning and Sequential Sampling Model Constrained by Gaze Data

This repo has two versions:

**Version 2.0 has updated modeling code written in Rcpp for faster run times.**

**Version 1.0 corresponds to our [bioRxiv preprint](https://www.biorxiv.org/content/10.1101/2025.08.27.672620v1).**

## Data analysis & plotting

To reproduce our analyses and figures, simply work through the following R Markdown files:

-   [Experiment 1 Analyses](Exp1_TwoContexts/analysis/Exp1_Analysis.Rmd)
-   [Experiment 2 Analyses](Exp2_FourContexts/analysis/Exp2_Analysis.Rmd)

## Model fitting

The following scripts can be used to fit any of our models to each participant's data:

-   [Experiment 1 Model Fitting](Exp1_TwoContexts/modeling/fit_model_Exp1.R)
-   [Experiment 2 Model Fitting](Exp2_FourContexts/modeling/fit_model_Exp2.R)

If working from the command line, note that each script takes a number of command line arguments.

**fit_model_Exp1.R:**

-   arg1: index of model to fit (integer between 1 and 8)

-   arg2: value at which to fix the $w_{rel}$ parameter (float between 0 and 1)

-   arg3: whether to freely estimate the $\theta$ parameter or fix it to 50 (integer: 0 or 1)

**fit_model_Exp2.R:**

-   arg1: index of model to fit (integer between 1 and 8)

-   arg2: whether to freely estimate the $\theta$ parameter or fix it to 50 (integer: 0 or 1)

-   arg3: whether to fit a decreasing threshold or a static threshold (integer: 0 or 1)

The arguments must be specified in that order. See examples below.

``` bash
cd Exp1_TwoContexts/modeling

# Fit Model 7 to the data from Experiment 1, fixing w_rel to 0 and with theta free
Rscript fit_model_Exp1.R 7 0 1
```

``` bash
cd Exp2_FourContexts/modeling

# Fit Model 8 to the data from Experiment 2, with theta free and a static decision threshold
Rscript fit_model_Exp2.R 8 1 0
```

## Accumulative one-step-ahead prediction error (APE)

The following scripts can be used to compute a model's APE for each participant:

-   [Experiment 1 APE](Exp1_TwoContexts/modeling/APE_Exp1.R)
-   [Experiment 2 APE](Exp2_FourContexts/modeling/APE_Exp2.R)

If working from the command line, note that each script takes a number of command line arguments.

**APE_Exp1.R:**

-   arg1: index of model to use (integer between 1 and 8)

-   arg2: batch number (integer between 1 and 20); see below

-   arg3: value at which to fix the $w_{rel}$ parameter (float between 0 and 1)

-   arg4: whether to freely estimate the $\theta$ parameter or fix it to 50 (integer: 0 or 1)

**APE_Exp2.R:**

-   arg1: index of model to use (integer between 1 and 8)

-   arg2: batch number (integer between 1 and 20); see below

-   arg3: whether to freely estimate the $\theta$ parameter or fix it to 50 (integer: 0 or 1)

The arguments must be specified in that order. See example below.

``` bash
cd Exp1_TwoContexts/modeling

# Compute Model 7's APE for the first batch of participants in Experiment 1,
# fixing w_rel to 0 and with theta as a free parameter
Rscript APE_Exp1.R 7 1 0 1
```

**Important:** APE computation was carried out on a computing cluster, with the workload split into 20 batches that were run in parallel across different nodes. In each batch, APE is computed for a small number of participants.

## Model simulation

The following scripts can be used to simulate all models in our tasks using each participant's fitted parameters:

-   [Experiment 1 Model Simulation](Exp1_TwoContexts/modeling/simulate_models.R)
-   [Experiment 2 Model Simulation](Exp2_FourContexts/modeling/simulate_models.R)

## Parameter recovery

The following scripts can be used to reproduce our parameter recovery experiments:

-   [Experiment 1 Parameter Recovery](Exp1_TwoContexts/modeling/param_recovery_Exp1.R)
-   [Experiment 2 Parameter Recovery](Exp2_FourContexts/modeling/param_recovery_Exp2.R)

## Other contents

-   [Reward Distributions Plot](ExpDesignFigure.R) generates the reward distributions in Figure 1A.

-   [Q Values Plot](Fig1_Qvalues.R) generates the Q-value plot in Figure 1B.

-   [Supplemental Figures S1 and S2](drift_heatmap.R) generates Figures S1 and S2.

-   R Markdown files for mixed effects modeling can be found in the analysis subfolders.
