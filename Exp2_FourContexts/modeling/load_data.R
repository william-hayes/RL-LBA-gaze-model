library(dplyr)

# read in the data 
dat <- read.csv('../data/full_modeling_data_Exp2.csv', header=T)

# categorize gaze proportions by correct / incorrect option
# calculate the gaze difference: correct option minus incorrect option
# divide into equally sized buckets for the learning and transfer phases
dat <- dat %>%
  group_by(subject, block) %>%
  mutate(gaze_cor = ifelse(correct_resp=='left', pre_fix_left, pre_fix_right),
         gaze_inc = ifelse(correct_resp=='left', pre_fix_right, pre_fix_left),
         gaze_diff = gaze_cor - gaze_inc,
         gaze_bucket = ntile(gaze_diff, 5)) %>%
  ungroup()

#individual datasets
datasets <- list()
modeling_data <- list()
IDs <- unique(dat$subject)
for (i in 1:length(IDs)) {
  subject_data <- dat %>% filter(subject == IDs[i])
  
  datasets[[i]] <- list(subject = IDs[i],
                        correct_resp = subject_data$correct_resp,
                        choice = subject_data$choice_id - 1,
                        correct = subject_data$correct,
                        RT = subject_data$RT,
                        options = as.matrix(subject_data[,c('left_index','right_index')]) - 1,
                        avail = as.matrix(subject_data[,c('left_index','right_index')]) - 1,
                        outcomes = as.matrix(subject_data[,c('left_outcome','right_outcome')]),
                        gaze_pre = as.matrix(subject_data[,c('pre_fix_left','pre_fix_right')]),
                        gaze_bucket = subject_data$gaze_bucket,
                        N = sum(subject_data$RT >= 250 & subject_data$RT <= 10000))
  
  # model inputs only
  modeling_data[[i]] <- list(choice = subject_data$choice_id - 1,
                             RT = subject_data$RT,
                             options = as.matrix(subject_data[,c('left_index','right_index')]) - 1,
                             outcomes = as.matrix(subject_data[,c('left_outcome','right_outcome')]),
                             avail = as.matrix(subject_data[,c('left_index','right_index')]) - 1,
                             gaze = as.matrix(subject_data[,c('pre_fix_left','pre_fix_right')]),
                             n_opt = 8,
                             min_o = min(as.matrix(subject_data[,c('left_outcome','right_outcome')]), na.rm=T),
                             max_o = max(as.matrix(subject_data[,c('left_outcome','right_outcome')]), na.rm=T),
                             rt_lower=250,
                             rt_upper=10000,
                             Q_init=0.5)
  
  rm(subject_data)
}
