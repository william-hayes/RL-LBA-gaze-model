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
IDs <- unique(dat$subject)
for (i in 1:length(IDs)) {
  subject_data <- dat %>% filter(subject == IDs[i])
  datasets[[i]] <- list(subject = IDs[i],
                        correct_resp = subject_data$correct_resp,
                        choice = subject_data$choice_id,
                        correct = subject_data$correct,
                        RT = subject_data$RT,
                        options = as.matrix(subject_data[,c('left_index','right_index')]),
                        outcomes = as.matrix(subject_data[,c('left_outcome','right_outcome')]),
                        gaze_pre = as.matrix(subject_data[,c('pre_fix_left','pre_fix_right')]),
                        gaze_bucket = subject_data$gaze_bucket,
                        N = sum(subject_data$RT >= 250 & subject_data$RT <= 10000))
  rm(subject_data)
}
