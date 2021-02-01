#' ---
#' title: "Random Assignment and Experiments"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 4 February 2021
#' abstract: "This is a lab script for [POST 8000](http://post8000.svmiller.com), a graduate-level quantitative methods for public policy class that I teach at Clemson University. It will not be the most sophisticated R-related write-up of mine---check [my blog](http://svmiller.com/blog) for those---but it should be useful for discussion around the associated R script for the week's 'lab' session."
#' output:
#'    html_document:
#'      css: lab-script.css
#'      toc: TRUE
#'      toc_float:
#'        collapsed: false
#'        smooth_scroll: false
#'      highlight: zenburn
#' ---


#' # R Packages/Data for This Session
#' 
#' The data we'll be using were cobbled from the [data made available](https://www.masteringmetrics.com/resources/) by the authors of *Mastering 'Metrics*.
#' I also want to note that Jeff Arnold, helpfully, ganked these data and did a lot of pre-processing of them, which I mirrored on my end before
#' making these data available in my `{stevedata}` package. I did want to issue one caveat about these data, more a complaint about Mastering 'Metrics. That
#' is: these data are not as well-sourced as I'd like. There's a bit of a leap of faith in terms of the data made available and what the authors provide
#' in their textbook. The results are effectively able to be replicated, but not perfectly.
#' 
#' Anywho, here are the data and packages we'll be using.

library(tidyverse)
library(stevedata)

#' # The Idea of Random Assignment
#' 
#' The idea of random assignment is that it is ideal for the sake of causal inference that a "treatment" of interest is randomly assigned
#' within a controlled setting. For entry students, this sounds strange---shouldn't treatments be allocated on basis of need or something to that effect?
#' The answer from a causal inference perspective is "no, of course not." Strange as this may seem to a lay person, but the logic is that you want people
#' receiving the treatment to be equal in expectation to those who do not receive the treatment. There may be random differences between the group
#' that receives the treatment and the (control) group that does not *but you want the only systematic difference between the two groups to be the treatment
#' itself*. 
#' 
#' Let's think of a simple/hypothetical case here. Assume we recruited 685 people for an experiment. There is no special reason for the number 685; it
#' just happened to come off my fingertips. The subjects in this pool of 685 people either received the treatment (1) or did not
#' receive the treatment (0). Thereafter, we took some measurements from the pool on some outcome of interest (`y`). On average, the true effect of the treatment 
#' on the outcome of interest is that a unit change in the treatment (i.e. receiving it) increases the value of `y` by 1, even if the
#' errors randomly distributed around those effects are some what diffuse. The data would look something like this.
#' 
set.seed(8675309) # Jenny, I got your number

tibble(id = seq(1:685),
       treat = sample(c(0, 1), 685, replace=TRUE),
       y = 50 + treat + rnorm(685, 0, 2.5)) -> D

#' It looks like we isolated the treatment effect pretty well. That's the benefit of dealing with larger samples. If you want to give this a whirl for
#' yourself, try reducing the number of observations to something like 100.

broom::tidy(t.test(y ~ treat, data = D))

#' Because the number of observations is odd, there's going to be a slight discrepancy between who receives the treatment and who does not.
#' It's immaterial here that treatment and control are not equal in size. Importantly, we basically did go-go-gadget randomization a la the `sample()`
#' function in R.
D %>% group_by(treat) %>% tally()

#' Now, let's illustrate the logic of random sample, if with some kind of ex post R chicanery. Notice the treatment/control group sizes above? Let's use
#' that as the basis for assigning the sex of the respondent. The first 362 observations will be men. The next 323 observations will be women. Then, we're
#' going to use the `id` column (which is effectively just a unique identifier for the respondent) to create some more identifying information. We'll make
#' 10 roughly equal quantiles from the ID number that will serve as age bracket for 1) 18-29, 2) 30-39, 3) 40-49, 
#' 4) 50-59, and 5) 60-85. These ages will be drawn randomly from a uniform distribution with those appropriate minimums and maximums based on that information. 
#' The age stuff will lean on the `rowwise()` function.
#' 
#' Here's a way of thinking about this hypothetical experiment. Assume we ran our shop from 8 a.m. to 5 p.m. or something like that.
#'  In our weird case, the first 362 people that showed up were dudes. The women didn't start showing up until about halfway through the experiment.
#'  Further, the youngest people either showed up first thing in the morning or before the experiment closed shop. The older subjects only arrived midway through
#'  the day. In other words, there is clear variation between who arrives for the experiment *but the allocation of the treatment is still effectively random.*
set.seed(8675309)
D %>% 
  mutate(female = c(rep(0, 362), rep(1, 323)),
         quantile = ntile(id, 10)) %>% 
  rowwise() %>%
  mutate(age = case_when(
    quantile %in% c(1, 10) ~ runif(1, 18, 29),
    quantile %in% c(2, 9) ~ runif(1, 30, 39),
    quantile %in% c(3, 8) ~ runif(1, 40, 49),
    quantile %in% c(4, 7) ~ runif(1, 50, 59),
    quantile %in% c(5, 6) ~ runif(1, 60, 85)
    )) %>% ungroup() %>%
  mutate(age = round(age)) -> D

#' While I can't imagine many full-time experimentalists would advise running a controlled experiment where there are major correlates for who arrives at what time
#' for the experiment, the idea is that the allocation of the treatment should still be randomly assigned. In other words, no matter the systematic determinants 
#' of subject arrival, the allocation of treatment and control is unaffected and, thus, the difference in group compositions should still be random.
#' In our case, notice there is no systematic differences between treatment and control by age...

broom::tidy(t.test(age ~ treat, data = D))

#' ...or by gender. Here, I'll note how much I hate how clunky the `prop.test()` is in R and how little I do these in my day-to-day stuff to be aware of fancier
#' alternatives.

D %>%
  group_by(female, treat) %>%
  tally() %>% group_by(treat) %>% 
  mutate(treat_tot = sum(n)) %>%
  arrange(treat_tot)

# test for proportion of women by different treatment groups.
broom::tidy(prop.test(x = c(145, 178), n = c(323, 362)))

#' There are differences between treatment and control, but they are random differences. The only systematic difference that emerges is the treatment.
#' 
#' With that in mind, let's dig into the data made available from the authors of *Mastering 'Metrics* to replicate some of what they discuss.
#' 
#' # The 2009 National Health Insurance Survey (NHIS)
#' 
#' We'll start with the the 2009 NHIS example from Chapter 1. I have this in `{stevedata}` as `mm_nhis`.

?mm_nhis
mm_nhis

#' Let's go explore these data some. First, let's start with the health index, which is a numeric vector for a health index,
#' broadly understood, that ranges from 1-5. I have some misgivings about communicating this via mean. I'll belabor that reservation
#' more in a few weeks. However, let's go look at the health index as a function of having health insurance (yes or no, 1 or 0) by both 
#' men and women.
#' 
mm_nhis %>%
  group_by(fml, hi) %>%
  summarize(mean_hlth = mean(hlth),
            sd_hlth = sd(hlth)) %>%
  group_by(fml) %>%
  mutate(diff = mean_hlth - lag(mean_hlth))

#' Minus some rounding, or perhaps some weighting, we basically reproduced the means from the top row of Table 1.1. The difference in means
#' for having health insurance is about .278 for women and about .382 for men.
#' 
#' Notice that the presence of a standard error in parentheses around the difference in Table 1.1 implies they're communicating the difference in
#' means via *t*-test. You can also do that even as it takes some acclimation with the *t*-test. Basically, the default output in the `t.test()` function
#' in base R is from a Welch's two-sample test (i.e. the groups/treatments have unequal variances) and the test is two-tailed.

mm_nhis %>%
  group_by(fml) %>% 
  do(broom::tidy(t.test(hlth ~ hi, data = .))) %>%
  data.frame

#' Of note: the authors of *Mastering 'Metrics* report the standard error of the difference whereas the default output in a tidied *t*-test is the 
#' 95% confidence interval. If it were that important for you to extract the standard error in a simple application like this, I think you'd have to do 
#' it manually independent of the `tidy()` function. For example, here's what it'd be for men.
#' 
t_men <- t.test(hlth ~ hi, data = subset(mm_nhis, fml == 0))
as.vector(abs(diff(t_men$estimate)/t_men$statistic)) 

#' And here's what it'd be for women:
t_women <- t.test(hlth ~ hi, data = subset(mm_nhis, fml == 1))
as.vector(abs(diff(t_women$estimate)/t_women$statistic))

#' Knowing what you know about confidence intervals, standard errors, and the normal distribution, you could manually calculate the confidence interval.
#' It'll tell you the same thing: for men and women, those having at least some health insurance score higher in their health index than those that don't.
#' 
#' I don't know where you all are re: getting comfortable with *t*-test output, but a graphical rendering of what the *t*-test is effectively telling you
#' may help. Here, for both men and women, the 95% intervals around general health estimates for those with and without health insurance do not overlap.

mm_nhis %>%
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
  scale_x_discrete(labels = c("No Health Insurance", "Some Health Insurance")) +
  labs(y = "Mean Health Insurance (with 95% Intervals)")

#' Let's [Leeroy Jenkins this](https://www.youtube.com/watch?v=mLyOj_QD4a4) now and fully reproduce Table 1.1 in *Mastering 'Metrics*. Here's what we're
#' going to do. First, we're going to select just we want and "gather" it with the `gather()` function that comes in the tidyverse (by way of `{tidyr}`).
#' Then, we're going to recode the health insurance variable for readability purposes. Thereafter, we're going to group-split the data by dependent variable
#' and gender. We'll call this new object `Listie`, because I am bad at naming objects. Do as I say, not as I do on this front.


mm_nhis %>%
  select(-perweight) %>%
  gather(var, val, -fml, -hi) %>%
  # This is a little convoluted, but it'll help readability for comparing to Table 1.1
  mutate(hi = ifelse(hi == 0, 1, 0)) %>%
  group_split(var, fml) -> Listie # note: I can be really bad with object names.

#' Inspecting the intermediate output, I see that there are 14 data frames in the list of data frames. The sequence goes, for each DV, a data frame
#' for men and then a data frame for women. The group-splitting of the DVs is alphabetical by variable name. Since each DV appears twice in the list
#' of data frames (by gender), let's repeat them. Trust me, I know what I'm doing. I'm a doctor, after all.

mm_nhis %>%
  select(-perweight) %>%
  gather(var, val, -fml, -hi) %>% distinct(var) %>% arrange(var) %>% pull(var) %>% rep(., each=2) -> DVs

#' Then, for each data frame in `Listie`, we're going to do a *t*-test to get the important info we want. We're going to `do.call()` all those data into
#' a single data frame. Then, we're going to add in the identifying information (i.e. the DVs and whether the data were for men or women). Let's also do
#' some nip-and-tuck stuff for readability too (i.e. rename some columns and create 95% confint brackets).

Listie %>%
  map(~broom::tidy(t.test(val ~ hi, data = .))) %>%
  do.call("rbind", .) %>%
  mutate(dv = DVs,
         gender = rep(c("Male", "Female"), 7)) %>%
  rename(diff = estimate,
         mean_somehi = estimate1,
         mean_nohi = estimate2) %>%
  mutate(confint95 = paste0("[",round(conf.low, 2),", ", round(conf.high, 2),"]")) %>% 
  select(gender, dv, mean_somehi, mean_nohi, diff, confint95)

#' The pertinent info is there and, plus side, you learned a little bit about *t*-tests and how to do them in a pipe workflow (even grouped).
#' 
#' # The Rand Health Insurance Experiment
#' 
#' Next up, let's replicate the information summarized in Table 1.3 and Table 1.4 from the Rand health insurance experiment. I have these data as 
#' `mm_randhie` in `{stevedata}`.
?mm_randhie

#' Importantly: `mm_randhie` is a *list* of two data frames. The first is the baseline data. You can see that here. Note the double brackets for isolating
#' elements of a list in R.
#' 
mm_randhie[[1]]

#' The elements of the list are named too, so this will work as well.

mm_randhie[["RAND Baseline"]]

#' The second data frame in `mm_randhie` is the outcome data from the experiment. You can access this as follows, or as `mm_randhie[["RAND Outcomes"]]`.

mm_randhie[[2]] 

#' Let's start with the first data set on the baseline stuff in order to replicate some of what we see in Table 1.3. In this particular table, the 
#' authors are treating health insurance coverage as kind of a fixed effect. The catastrophic plan is the "baseline", if you will, against which they
#' are comparing the means for the deductible group, the coinsurance group, the free group and any ob the above against the catastrophic group.
#' 
#' First, let's see what we're dealing with here.

mm_randhie[[1]] %>%
  group_by(plantype) %>%
  tally()

#' Cool beans. Alrightie, we have to be a bit mindful with what the authors are doing. Basically, the mean of an interval variable is the arithmetic mean
#' while the mean of the proportion variable (e.g. whether the individual is not-white) is the proportion of 1s. More importantly, the standard deviation is
#' calculated differently (i.e. sqrt(p*(1-p))).
#' 
#' Let's keep it simple here to start. We'll compare the mean age between the catastrophic and the other plans. 
#' Recall, this is an experimental design with randomly assigned treatments. If there are significant differences by potential confounders among these
#' treatment groups, we have some reason to believe we (rather: the experimental people who did this project) messed up.

mm_randhie[[1]] %>%
  select(plantype, age) %>%
  filter(plantype %in% c("Catastrophic", "Deductible")) %>%
  # group_by(plantype) %>% 
  # mutate(plantype = as.character(plantype)) %>%
  ungroup() %>%
  do(broom::tidy(t.test(age ~ plantype, data = .)))

#' This test result tells us that the difference in mean age for the catastrophic group is about 32.4 (which checks out, given what we see in that row
#' of Table 1.3). The difference between that mean age in the catastrophic group and the deductible group is about .56 (checks out again, see column 2 in
#' Table 1.3). That difference seems to be a fluke draw because the *p*-value associated with that difference in means is .382. In other words,
#' there is no statistically significant difference in age for those groups. That's good! We don't want those in the context of random assignment.
#' 
#' Let's generalize this a little bit. First, I'm going to stick to just the interval variables here (i.e. things I wouldn't mind plugging into
#' a *t*-test). This code is going to be convoluted, so let me explain what it's doing. First, I'm going to create a vector of the interval
#' variables. Next, I'm going to create a vector variable of the various plans, *separating out* the catastrophic plan (which is a baseline to which I'll
#' return later). Then, I'm going to create a baseline tibble (`baseline_comparisons`). Then, I'm going to do a loop (I know...). R programmers will tell
#' you to avoid doing a loop. *I* on the other hand will tell you they're right in the same way that people say you shouldn't drink a soda and have a pack
#' of gummy worms before bedtime. I mean, yeah, they're not good for you and they're right to say "don't do that",
#' but they'll make you occasionally feel good and they'll cure what ails you. So... yeah.
#' 
#' Anywho,  for each of these interval variable and, within those, each other plan type, I'm going to filter the baseline data to just the catastrophic
#' folk (and the other plan type), select just the plantype and interval variable, and do a *t*-test. I'm going to tidy the results of that and put it
#' into one of my favorite data frame names (`hold_this`). Thereafter, I'll populate the `baseline_comparisons` tibble through a row-bind.

interval_vars <- c("age", "cholest", "educper", "income1cpi", "mhi", "systol")

mm_randhie[[1]] %>%
  distinct(plantype) %>%
  pull() %>% as.character() %>% .[2:4] -> other_plans


baseline_comparisons <- tibble()

for (i in interval_vars) {
  for (j in other_plans) {
    
    mm_randhie[[1]] %>%
      filter(plantype %in% c("Catastrophic", j)) %>%
      select(plantype, i) -> dat
    
    ttest <-do.call('t.test', args=list(formula=as.formula(paste0(i,'~plantype')), data=dat))
    hold_this <- broom::tidy(ttest) %>%  mutate(plantype = j, var = i) %>% select(plantype, var, estimate:p.value)
    baseline_comparisons <- bind_rows(baseline_comparisons, hold_this)
  }

}

#' Observe what this does.

baseline_comparisons

#' From this, it looks like there are some discernible differences for a few groups on a few dimensions. For example, there is
#' a discernible difference in income between the catastrophic and the deductible group. There is a similar difference in the systolic
#' blood pressure as well. There is also a difference in cholesterol level between the free group and the catastrophic group. These are
#' less than ideal even as the bulk of what we observe would be consistent with random assignment.
#' 
#' For what it's worth, [Jeff Arnold's write-up seems to suggest](https://jrnold.github.io/masteringmetrics/rand-health-insurance-experiment-hie.html) that the authors are doing
#' some kind of standard error correction in the context of a fixed effects linear model. That would explain the standard error differences. 
#' 
#' Next, let's look at the outcomes data. Recall from the discussion in *Mastering 'Metrics* that the results of the experiment seemed to be less sanguine
#' about the effect of allocating more health care coverage for improving health care outcomes. Rather than improve health care outcomes, the
#' different plans seemed to mostly increase the consumption of health care.
#' 
#' The version of the data I have permit just a comparison of the number of face-to-face visits, the total of out-patient expenses for the respondent,
#' the number of hospital admissions, and the total health expenses for the respondent. The process of evaluation will look similar to what we did above.

healthuse_vars <- c("ftf", "out_inf", "totadm", "inpdol_inf", "tot_inf")

outcome_comparisons <- tibble()

for (i in healthuse_vars) {
  for (j in other_plans) {
    
    mm_randhie[[2]] %>%
      filter(plantype %in% c("Catastrophic", j)) %>%
      select(plantype, i) -> dat
    
    ttest <-do.call('t.test', args=list(formula=as.formula(paste0(i,'~plantype')), data=dat))
    hold_this <- broom::tidy(ttest) %>%  mutate(plantype = j, var = i) %>% select(plantype, var, estimate:p.value)
    outcome_comparisons <- bind_rows(outcome_comparisons, hold_this)
  }
}

#' The evidence here is consistent with increased use with greater coverage. I'm eye-balling just three insignificant *t*-test
#' results of the 15 total.
