###################
library(tidyverse)
library(caret)
###################

#areas
d <- df %>%
  filter(cohort == "filly2") %>%
  left_join(esl_area) %>%
  left_join(ga_area) %>%
  left_join(htr_area) %>%
  left_join(rpe_area) %>%
  select(va
         , 17:46
         , -ga_area_total, -esl_area_total, -htr_area_total, -rpe_area_total
         # , ga_area_total
         # , esl_area_total, htr_area_total, rpe_area_total
         # ,ga_area_1
         # ,ga_area_1, esl_area_1, htr_area_1, rpe_area_1
         ) %>% 
  filter(!is.na(ga_area_1))

# Probabilities
d <- df %>%
  filter(cohort == "FILLY2") %>%
  left_join(esl_prob) %>%
  left_join(ga_prob) %>%
  left_join(htr_prob) %>%
  left_join(rpe_prob) %>%
  select(va
         ,17:56
         , -ga_prob_total, -esl_prob_total, -htr_prob_total, -rpe_prob_total
         # , ga_prob_2, esl_prob_2, htr_prob_2, rpe_prob_2
         # , ga_prob_3, esl_prob_3, htr_prob_3, rpe_prob_3
         # , ga_prob_4, esl_prob_4, htr_prob_4, rpe_prob_4
         # , ga_prob_5, esl_prob_5, htr_prob_5, rpe_prob_5
         ) %>% 
  filter(!is.na(esl_prob_1))


########################################################
#### Cross-validation regression - BOOSTRAP RESAMPLING
# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
########################################################
# Define training control
set.seed(123)
train.control <- trainControl(method = "boot", number = 100)

# Train the model - 

model <- train(va ~ .,
               data = (d), 
               method = "lm",
               trControl = train.control)


# Summarize the results
summary(model)
print(model)




