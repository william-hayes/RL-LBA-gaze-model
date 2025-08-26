library(dplyr)

# read in the data 
dat <- read.csv('../data/full_modeling_data_Exp1.csv', header=T)

# categorize gaze proportions by correct / incorrect option
# calculate the gaze difference: correct option minus incorrect option
# divide into equally sized buckets
dat <- dat %>%
  group_by(subject) %>%
  mutate(gaze_cor = ifelse(EV_1 > EV_2, pre_fix_avail_1, pre_fix_avail_2),
         gaze_inc = ifelse(EV_1 > EV_2, pre_fix_avail_2, pre_fix_avail_1),
         gaze_diff = gaze_cor - gaze_inc,
         gaze_bucket = ntile(gaze_diff, 5)) %>%
  ungroup()

# individual datasets 
datasets <- list()
IDs <- unique(dat$subject)
for (i in 1:length(IDs)) {
  subject_data <- dat %>% filter(subject == IDs[i])
  datasets[[i]] <- list(subject = IDs[i],
                        EV_1 = subject_data$EV_1,
                        EV_2 = subject_data$EV_2,
                        choice = subject_data$choice_id,
                        correct = subject_data$correct,
                        RT = subject_data$RT,
                        options = as.matrix(subject_data[,c('option_1','option_2','option_3','option_4')]),
                        avail = as.matrix(subject_data[,c('avail_1','avail_2')]),
                        outcomes = as.matrix(subject_data[,c('outcome_1','outcome_2','outcome_3','outcome_4')]),
                        gaze_pre = as.matrix(subject_data[,c('pre_fix_avail_1','pre_fix_avail_2')]),
                        gaze_bucket = subject_data$gaze_bucket,
                        N = sum(subject_data$RT >= 250 & subject_data$RT <= 10000))
  rm(subject_data)
}