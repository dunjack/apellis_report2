################
# df
###############
df <- df


################
# Tables
###############
library(table1)


### 
df$cohort    <- factor(df$cohort, levels=c("filly2", "meh", 2), labels=c("FILLY2", "MEH", "P-value"))

df$ethnicity    <- factor(df$ethnicity,
                                 levels=c("afrocarribean", "black","se_asian", "caucasian","other","unknown", "hispanic or latino"), 
                                 labels=c( "Afro Caribbean",  "Afro Caribbean", "Asian",  "Caucasian", "Other","Unknown", "Hispanic or Latino"))

df$gender    <- factor(df$gender,
                              levels=c("f", "m"), 
                              labels=c( "Female", "Male"))


label(df$age)    <- "Age"
label(df$ethnicity)      <- "Ethnicity"
label(df$gender)    <- "Gender"
label(df$va)    <- "Visual acuity"

units(df$age)      <- "years"
units(df$va_base)      <- "ETDRS letters"

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- df[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ df$cohort)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(df$cohort)))$p.value
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


table1(~gender + age + ethnicity  | cohort,
       data=(df %>% distinct(patkey,.keep_all = T)), droplevels=F,render=rndr, render.strat=rndr.strat, overall="Overall")


