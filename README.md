# A Reinforcement Learning and Sequential Sampling Model Constrained by Gaze Data

* *model_functions.R* contains functions for simulating and fitting models 1-7 in the main text.
* *model_functions_v2.R* contains functions for simulating and fitting the models with trial-dependent decision threshold.
* *ExpDesignFigure.R* generates the reward distributions in Figure 1A.
* *Fig1_Qvalues.R* generates the Q-value plot in Figure 1B.
* *drift_heatmap.R* generates Figures S1 and S2 (plus an additional figure for RT effects).

**Exp1_TwoContexts and Exp2_FourContexts contain data and code for Experiments 1 and 2 in the main text.**

* **data**: contains the trial-level data.
* **modeling**: contains scripts for loading and preparing the individual data sets (*load_data.R*), fitting models to individual data (*fit_model_Exp\*.R*), computing accumulative one-step-ahead prediction error (*APE_Exp\*.R*), simulating models (*simulate_models.R*), and testing parameter recovery (*param_recovery_Exp\*.R*). Note that model fitting, APE computation, and parameter recovery were carried out on a high-performance computing cluster and hence have corresponding shell scripts (.sh files). To reduce run times, APE computation was carried out in batches run in parallel.
  * **results** subfolder contains modeling output (.RDS files).
* **analysis**: contains R markdown files for all computational modeling analyses (*Exp\*_Analysis.Rmd*) and mixed-effects modeling (*Mixed Effect Models Experiment \*.Rmd*). The CSV files contain model fit results.

