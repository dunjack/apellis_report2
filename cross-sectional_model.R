###################
library(tidyverse)
library(caret)
###################

#areas
d <- df %>%
  # filter(cohort == "filly2") %>%
  left_join(esl_area) %>%
  left_join(ga_area) %>%
  left_join(htr_area) %>%
  left_join(rpe_area) 
  # select(va
         # , 17:46
         # , -ga_area_total, -esl_area_total, -htr_area_total, -rpe_area_total
         # , ga_area_total
         # , esl_area_total, htr_area_total, rpe_area_total
         # ,ga_area_1
         # ,ga_area_1, esl_area_1, htr_area_1, rpe_area_1
         # ) %>% 
  filter(!is.na(ga_area_1))



# Probabilities
d <- df %>%
  # filter(cohort == "FILLY2") %>%
  inner_join(esl_prob) %>%
  inner_join(ga_prob) %>%
  inner_join(htr_prob) %>%
  inner_join(rpe_prob) %>%
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

### VA ; BOTH MEH & filly ###
#GA only
va_all_ga_total <- train(va ~ ga_prob_total,
                     data = (d), 
                     method = "lm",
                     trControl = train.control)

va_all_ga_fovea <- train(va ~ ga_prob_1,
                         data = (d), 
                         method = "lm",
                         trControl = train.control)


va_all_ga_allregions <- train(va ~ ga_prob_1 + ga_prob_2 + ga_prob_3 + ga_prob_4 
                              + ga_prob_5 + ga_prob_6 + ga_prob_7 + ga_prob_8,
                         data = (d), 
                         method = "lm",
                         trControl = train.control)

#GA + other features only
va_all_allfeatures_total <- train(va ~ ga_prob_total + rpe_prob_total + esl_prob_total + htr_prob_total,
                                  data = (d), 
                                  method = "lm",
                                  trControl = train.control)

va_all_allfeatures_fovea <- train(va ~ ga_prob_1 + rpe_prob_1 + esl_prob_1 + htr_prob_1,
                         data = (d), 
                         method = "lm",
                         trControl = train.control)


va_all_allfeatures_allregions <- train(va ~ 
                                         ga_prob_1 + ga_prob_2 + ga_prob_3 + ga_prob_4 + ga_prob_5 + ga_prob_6 + ga_prob_7 + ga_prob_8
                                       + rpe_prob_1 + rpe_prob_2 + rpe_prob_3 + rpe_prob_4 + rpe_prob_5 + rpe_prob_6 + rpe_prob_7 + rpe_prob_8
                                       + esl_prob_1 + esl_prob_2 + esl_prob_3 + esl_prob_4 + esl_prob_5 + esl_prob_6 + esl_prob_7 + esl_prob_8
                                       + htr_prob_1 + htr_prob_2 + htr_prob_3 + htr_prob_4 + htr_prob_5 + htr_prob_6 + htr_prob_7 + htr_prob_8,
                              data = (d), 
                              method = "lm",
                              trControl = train.control)


x <- 
  data.frame(va_all_allfeatures_total[4]) %>%
    mutate(
      cohort = "all",
      feature = "ga",
      region = "total") %>%
  rbind(
    data.frame(va_all_allfeatures_fovea[4]) %>%
                 mutate(
                   cohort = "all",
                   feature = "ga",
                   region = "fovea"))



  


# Summarize the results
summary(model)
print(model)

x <- data.frame(model[4])

lm(llva ~ ga_area_total, d) %>%
  summary() %>%
  xtable() %>%
  kable()


library(knitr)
library(xtable)

train(va ~ .,
      data = (d), 
      method = "lm",
      trControl = train.control) %>%
  summary() %>%
  xtable() 
