################
# df
###############

#
rpe_area <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/feature_probability/RPE.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    rpe_area_total = total,
    rpe_area_1 = X1,
    rpe_area_2 = X2,
    rpe_area_3 = X3,
    rpe_area_4 = X4,
    rpe_area_5 = X5,
    rpe_area_6 = X6,
    rpe_area_7 = X7,
    rpe_area_8 = X8,
    rpe_area_9 = X9 ) 

# JOIN ON FEATURE AREAS
# AREA mm2
esl_area <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/area/ESL.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    esl_area_total = total,
    esl_area_1 = X1,
    esl_area_2 = X2,
    esl_area_3 = X3,
    esl_area_4 = X4,
    esl_area_5 = X5,
    esl_area_6 = X6,
    esl_area_7 = X7,
    esl_area_8 = X8,
    esl_area_9 = X9 ) 

rpe_area <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/area/RPE.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    rpe_area_total = total,
    rpe_area_1 = X1,
    rpe_area_2 = X2,
    rpe_area_3 = X3,
    rpe_area_4 = X4,
    rpe_area_5 = X5,
    rpe_area_6 = X6,
    rpe_area_7 = X7,
    rpe_area_8 = X8,
    rpe_area_9 = X9 ) 

htr_area <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/area/HTR.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    htr_area_total = total,
    htr_area_1 = X1,
    htr_area_2 = X2,
    htr_area_3 = X3,
    htr_area_4 = X4,
    htr_area_5 = X5,
    htr_area_6 = X6,
    htr_area_7 = X7,
    htr_area_8 = X8,
    htr_area_9 = X9 ) 

ga_area <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/area/GA.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    ga_area_total = total,
    ga_area_1 = X1,
    ga_area_2 = X2,
    ga_area_3 = X3,
    ga_area_4 = X4,
    ga_area_5 = X5,
    ga_area_6 = X6,
    ga_area_7 = X7,
    ga_area_8 = X8,
    ga_area_9 = X9 ) 

# Probability of feature
esl_prob <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/feature_probability/ESL.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    esl_prob_total = total,
    esl_prob_1 = X1,
    esl_prob_2 = X2,
    esl_prob_3 = X3,
    esl_prob_4 = X4,
    esl_prob_5 = X5,
    esl_prob_6 = X6,
    esl_prob_7 = X7,
    esl_prob_8 = X8,
    esl_prob_9 = X9 ) 

rpe_prob <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/feature_probability/RPE.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    rpe_prob_total = total,
    rpe_prob_1 = X1,
    rpe_prob_2 = X2,
    rpe_prob_3 = X3,
    rpe_prob_4 = X4,
    rpe_prob_5 = X5,
    rpe_prob_6 = X6,
    rpe_prob_7 = X7,
    rpe_prob_8 = X8,
    rpe_prob_9 = X9 ) 

htr_prob <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/feature_probability/HTR.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    htr_prob_total = total,
    htr_prob_1 = X1,
    htr_prob_2 = X2,
    htr_prob_3 = X3,
    htr_prob_4 = X4,
    htr_prob_5 = X5,
    htr_prob_6 = X6,
    htr_prob_7 = X7,
    htr_prob_8 = X8,
    htr_prob_9 = X9 ) 

ga_prob <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/feature_probability/GA.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pdb, 8, pad = "0")),".pat" )) %>%
  select(-pdb, -foveal,-perifoveal, -X) %>%
  rename(
    ga_prob_total = total,
    ga_prob_1 = X1,
    ga_prob_2 = X2,
    ga_prob_3 = X3,
    ga_prob_4 = X4,
    ga_prob_5 = X5,
    ga_prob_6 = X6,
    ga_prob_7 = X7,
    ga_prob_8 = X8,
    ga_prob_9 = X9 ) 


area_total <- df %>%
  inner_join(esl_area) %>%
  inner_join(ga_area) %>%
  inner_join(htr_area) %>%
  inner_join(rpe_area) %>%
  select( cohort, va, llva,
          esl_area_total, rpe_area_total, htr_area_total, ga_area_total, 
          esl_area_1, rpe_area_1, htr_area_1, ga_area_1) %>%
  mutate(
    esl_area_1 = signif(esl_area_1 / 0.79 * 100,2),
    rpe_area_1 = signif(rpe_area_1 / 0.79 * 100,2),
    htr_area_1 = signif(htr_area_1 / 0.79 * 100,2),
    ga_area_1 = signif(ga_area_1 / 0.79 * 100,2))


################
# Table
###############
library(table1)

### 
area_total$cohort    <- factor(area_total$cohort, levels=c("filly2", "meh", 2), labels=c("FILLY2", "MEH", "P-value"))

label(area_total$va)    <- "Visual acuity"
label(area_total$llva)    <- "Low luminescence Visual acuity"

label(area_total$rpe_area_total)    <- "RPE-loss"
label(area_total$esl_area_total)    <- "Photoreceptor degeneration"
label(area_total$htr_area_total)    <- "Hypertransmission"
label(area_total$ga_area_total)    <- "Geographic atrophy"

label(area_total$rpe_area_1)    <- "RPE-loss"
label(area_total$esl_area_1)    <- "Photoreceptor degeneration"
label(area_total$htr_area_1)    <- "Hypertransmission"
label(area_total$ga_area_1)    <- "Geographic atrophy"

units(area_total$va)      <- "ETDRS letters"

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- area_total[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ area_total$cohort)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(area_total$cohort)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}


table1(~ va + rpe_area_1 + esl_area_1 + htr_area_1 + ga_area_1|cohort ,
       data=area_total, droplevels=F,render=rndr, render.strat=rndr.strat, overall="Overall")


table1(~ va + rpe_area_total + esl_area_total + htr_area_total + ga_area_total|cohort ,
       data=area_total, droplevels=F,render=rndr, render.strat=rndr.strat, overall="Overall")


table1(~ va + llva |cohort ,
       data=area_total, droplevels=F,render=rndr, render.strat=rndr.strat, overall=F)





################
# distribution
###############

df_distr_va <- df %>%
  mutate(cohort = "overall") %>%
  rbind(df) %>%
  rbind(df %>% 
          filter(cohort == "filly2") %>%
          mutate(cohort = "filly2_2",
                 va = llva) 
          ) %>% # Plus Low Luminescence VA
  ggplot(aes(x = factor(cohort, 
                        levels=c("overall", "filly2", "meh","filly2_2"), 
                        labels=c("Overall","FILLY2", "MEH","FILLY2_2")), 
             y= va, 
             group = factor(cohort, 
                            levels=c("overall", "filly2", "meh","filly2_2"), 
                            labels=c("Overall","FILLY2", "MEH","FILLY2_2")))) +
  geom_violinhalf(width = 0.5, fill = NA, linetype = "dotted") +
  stat_boxplot(geom = "errorbar", width = 0.3, position = position_nudge(x = -0.3)) + 
  geom_boxplot(width= 0.2, fill = c("white","white","white","darkgrey"), position = position_nudge(x = -0.3), outlier.size = 0.3) +
  geom_dotplot(binaxis = "y", binwidth = 0.75, dotsize = 0.2) + 
  theme_bw() +
  labs(
    x = "",
    y = "ETDRS letters") +
  scale_y_continuous(breaks=seq(0, 120, 10))

ggsave(df_distr_va, file="df_distr_va.png", width = 6.5, height = 5.5)


df_distr_area <- area_total %>%
  gather("feature", "value", 4:7) %>%
  mutate(cohort = "overall") %>%
  rbind(  area_total %>%
            gather("feature", "value", 4:7) ) %>%
  ggplot(aes(x = factor(cohort, 
                        levels=c("overall", "filly2", "meh"), 
                        labels=c("Overall","FILLY2", "MEH")), 
             y= value,
             color = feature )) +
  geom_violinhalf(width = 0.5, fill = NA, linetype = "dotted") +
  stat_boxplot(geom = "errorbar", width = 0.2, position = position_nudge(x = -0.3)) + 
  geom_boxplot(width= 0.2, fill = "white", position = position_nudge(x = -0.3), outlier.size = 0.3) +
  geom_dotplot(binaxis = "y", binwidth = 1.0, dotsize = 0.05) + 
  theme_bw() +
  scale_y_continuous(limits=c(0, 30), breaks = seq(0,40,5))+
  labs(
    x = "",
    y =  expression(paste("Area (mm"^"2",")")),
    color = "") +
  ggtitle("") + 
  facet_wrap(~ factor(feature,
                      levels = c("rpe_area_total", "esl_area_total", "htr_area_total", "ga_area_total"),
                      labels = c("RPE-loss", "Photoreceptor degeneration", "Hypertransmission", "RORA")), nrow =2) +
  scale_colour_manual(values = c("#1380A1", "#588300","#990000", "#b57807" )) + 
  theme(legend.position='none')

ggsave(df_distr_area, file="df_distr_area.png", width = 12, height = 11)




