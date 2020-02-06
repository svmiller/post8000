#
# Chapter 1 of Mastering Metrics
# -----------------------------------------------------
# Steve Miller
# HT: Jeff Arnold, whose code I ganked in various places.
# Date: 5 February 2020

# Let's get started with updating/installing some packages -----
# update it again, just in case.
# Certainly, by time I wrote this, I added a few new data sets from the previous lab.

devtools::install_github("svmiller/post8000r")

library(tidyverse)
library(post8000r)

# Let's replicate Table 1.1 from Mastering Metrics

NHIS2009 %>%
  mutate(fml = ifelse(fml == 1, "Female", "Male")) %>%
  # Turn columns to long, but subset out: fml, hi, and perweight.
  # We actually don't want perweight for anything important here
  gather(variable, value, -fml, -hi, -perweight) %>%
  # group by female, health insurance, and the particular variable
  group_by(fml, hi, variable) %>%
  # Get means of the variables, by each 2x2 of the groups
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  # Turn the mean and sd into one "stat" variable, with values.
  gather(stat, value, -fml, -hi, -variable) %>%
  # Concatenate stat and hi into stat_hi
  unite(stat_hi, stat, hi) %>%
  # spread to wide
  spread(stat_hi, value) %>%
  # Reorder
  select(fml, variable, mean_1, mean_0, sd_1, sd_0) %>%
  # Create differences
  mutate(diff = mean_1 - mean_0)

# t-tests are ways to check for differences of means across two groups
# You could do it informally like this.
NHIS2009 %>%
  mutate(female = ifelse(fml == 0, "Male", "Female")) %>%
  group_by(female, hi) %>%
  summarize(mean = mean(hlth, na.rm=T),
            sd = sd(hlth),
            lwr = mean - (abs(qnorm(.025))*(sd/sqrt(n()))),
            upr = mean + (abs(qnorm(.025))*(sd/sqrt(n())))) %>%
  group_by(female) %>%
  # Notice this will tell you basically what you learned above.
  mutate(diff = mean - lag(mean, 1)) %>%
  # You can visualize this too.
  ggplot(.,aes(as.factor(hi), mean, ymin=lwr, ymax=upr)) +
  facet_wrap(~female) +
  geom_pointrange() + coord_flip() +
  scale_x_discrete(labels = c("No Health Insurance", "Some Health Insurance"))

# t-tests allow you to check for a difference of means across groups.
NHIS2009 %>%
  # create a more informative factor for female
  mutate(fml = as_factor(ifelse(fml == 0, "Male", "Female"))) %>%
  # group by female
  group_by(fml) %>%
  # Notice: for both men and women, we're comparing the mean of income for those without health insurance to those with it.
  # estimate1 will be those without. estimate2 will be those without.
  # statistic will be the t-statistic.
  # p-value will be the, well, p-value.
  # parameter should be the df for the statistic
  do(broom::tidy(t.test(inc ~ hi, data=.)))




data(Rand)

# I want to note a couple things here. Namely, Rand is a "list." of two data frames.
# Rand[["RAND Baseline"]] (alternatively: Rand[[1]]) will be the baseline stuff for a replication of Table 1.3 in Mastering Metrics.
# Rand[["RAND Outcomes"]] (alternatively: Rand[[2]]) will be the outcomes stuff for a partial replication of Table 1.4


Rand[["RAND Baseline"]] %>%
  # Take the first one/baseline group
  group_by(plantype) %>%
  tally()

base_vars <- c("age","blackhisp","cholest","diastol",
               "educper","female","ghindx",
               "hosp","income1cpi","mhi","systol")

# Let's start to replicate Table 1.3
Rand[["RAND Baseline"]] %>%
  # get just the Catastrophic type
  filter(plantype == "Catastrophic") %>%
  # select the plantype, and anything that ppears in the base_vars
  select(plantype, one_of(base_vars)) %>%
  # Make it "long", holding the plantype for context.
  gather(variable, value, -plantype, 2:ncol(.)) %>%
  # group by the variable
  group_by(variable) %>%
  # Create means and SDs
  # NOTE: this will be inadequate for proportions since those have a different calculation of a SD
  # So, don't read too much into those. I'm being lazy. :P
  summarize(mean = mean(value, na.rm=T),
            sd = sd(value, na.rm=T)) %>%
  # round to two decimal points
  mutate_if(is.numeric, ~round(., 2)) %>%
  # concatenate together
  mutate(`Catastrophic (Means and SDs)` = paste0(mean," (", sd, ")")) %>%
  # get just what we want
  select(-mean, -sd) -> CatBase

# Okay, pretty sure this is how to automate what AP do in MM
# Let's create a blank tibble
ModsBase = tibble()

# Now, for each one of the base_vars, let's:
# 1) regress the base_var on the factor of plantype,
# 2) tidy it up and create a new variable that identifies what it is.
# 3) Add it do the Mods tibble
for (i in base_vars){
  mod <- lm(as.formula(paste(i,"~ plantype")), data=Rand[["RAND Baseline"]])
  hold_this <- broom::tidy(mod) %>% mutate(variable = i)
  ModsBase <- bind_rows(ModsBase, hold_this)
}

# see?
ModsBase 

ModsBase %>%
  # We don't want intercepts. Those are the catastrophic plans
  filter(term != "(Intercept)") %>%
  # get rid of that plantype prefix
  mutate(term = stringr::str_remove(term, "plantype")) %>%
  # get rid of the t-stat and p.value
  select(-statistic, -p.value) %>%
  # round to two decimal points
  mutate_if(is.numeric, ~round(., 2)) %>%
  # concatenate together
  mutate(est = paste0(estimate, " (", std.error,")")) %>%
  # select just the three things we want
  select(term, variable, est) %>%
  # spread them out, because this'll merge real nice.
  spread(term, est) %>%
  # bind this together with CatBase
  bind_cols(CatBase, .) %>%
  # select just what we want, mindful that the spread made the column order alphabetical.
  select(variable, `Catastrophic (Means and SDs)`, Deductible, Coinsurance, Free) # voila

# Note: I think the differences in the S.E.s here are because of S.E. clustering, which I elected to not do for convenience.

# You could also do this as a series of t-tests if you'd like
# Basic takeaway here: random assignment made the differences between groups truly random.
# That's what it's supposed to do!
Rand[["RAND Baseline"]] %>%
  filter(plantype %in% c("Catastrophic", "Deductible")) %>%
  do(broom::tidy(t.test(cholest
                        ~plantype, data=.)))


# Now, let's do a partial replication of Table 1.4
Rand[["RAND Outcomes"]] %>%
  filter(plantype == "Catastrophic") %>%
  select(plantype:ncol(.)) %>%
  # All right, let's do it again. Same as above.
  gather(variable, value, -plantype, 2:ncol(.)) %>%
  # group by the variable
  group_by(variable) %>%
  # Create means and SDs
  # NOTE: this will be inadequate for proportions since those have a different calculation of a SD
  # So, don't read too much into those. I'm being lazy. :P
  summarize(mean = mean(value, na.rm=T),
            sd = sd(value, na.rm=T)) %>%
  # round to two decimal points
  mutate_if(is.numeric, ~round(., 2)) %>%
  # concatenate together
  mutate(`Catastrophic (Means and SDs)` = paste0(mean," (", sd, ")")) %>%
  # get just what we want
  select(-mean, -sd) -> CatOut

out_vars <- c("ftf", "inpdol_inf","out_inf","tot_inf","totadm")

# Let's create a blank tibble
ModsOut = tibble()

# Now, for each one of the out_vars, let's:
# 1) regress the base_var on the factor of plantype,
# 2) tidy it up and create a new variable that identifies what it is.
# 3) Add it do the Mods tibble
for (i in out_vars){
  mod <- lm(as.formula(paste(i,"~ plantype")), data=Rand[["RAND Outcomes"]])
  hold_this <- broom::tidy(mod) %>% mutate(variable = i)
  ModsOut <- bind_rows(ModsOut, hold_this)
}

# see?
ModsOut


# All right, let's do it again.
ModsOut %>%
  # We don't want intercepts. Those are the catastrophic plans
  filter(term != "(Intercept)") %>%
  # get rid of that plantype prefix
  mutate(term = stringr::str_remove(term, "plantype")) %>%
  # get rid of the t-stat and p.value
  select(-statistic, -p.value) %>%
  # round to two decimal points
  mutate_if(is.numeric, ~round(., 2)) %>%
  # concatenate together
  mutate(est = paste0(estimate, " (", std.error,")")) %>%
  # select just the three things we want
  select(term, variable, est) %>%
  # spread them out, because this'll merge real nice.
  spread(term, est) %>%
  # bind this together with CatOut
  bind_cols(CatOut, .) %>%
  # select just what we want, mindful that the spread made the column order alphabetical.
  select(variable, `Catastrophic (Means and SDs)`, Deductible, Coinsurance, Free) # voila

# Again, the SEs are off because I didn't cluster on what looks to be the person variable. Same basic takeaway emerges.
