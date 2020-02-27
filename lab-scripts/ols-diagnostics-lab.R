#
# OLS Regression Diagnostics
# -----------------------------------------------------
# Steve Miller
# Date: 27 February 2020

# Let's get started with updating/installing some packages -----
devtools::install_github("svmiller/post8000r")

# Also install these. You'll thank me letter. Lots of good OLS diagnostic tools in these
# modelr is useful for other stuff too.
install.packages("lmtest", "sandwich", "robustbase", "modelr")


# load packages for today's lab session
library(tidyverse)
library(post8000r)
library(lmtest)
library(sandwich)
library(robustbase)
library(modelr)

# Let's start with a basic replication of some of what you see in Chapter 5 of the SAGE Handbook
# The authors here are using a sample data set on trust in the police in Belgium.
# I created that data here as ESSBE5

ESSBE5
?ESSBE5 # for the codebook

# Let's get that simple linear model.
summary(M1 <- lm(trstplc ~ agea + female + eduyrs +  hincfel + plcpvcr, data=ESSBE5))

# The results suggest that there are no gender differences, but:
# older respondents are less likely to trust the police, but that effect looks tiny even given the scale of the variable.
# more educated people are more likely to trust the police
# those who feel less secure in their income (given how that's defined) are less likely to trust the police
# The more confident the respondent is about the ability of the police to prevent crime, the higher the trust.

# One diagnostic the authors recommend doing with these data are comparing a LOESS smoother with a linear model for each variable.
# To be clear: the IVs here are mostly discrete. Gender is binary. The DV has 11 different values. This won't look pretty
# But, it's a place to start. Here's how you can do that in R.

ESSBE5 %>%
  # Select only the variables we need
  select(trstplc:plcpvcr) %>%
  # tidyr::gather from long to even longer.
  # This creates a handy three column data frame where each value of each IV is matched with the DV for all observations
  gather(var, value, 2:ncol(.)) %>%
  # Create a ggplot where the x axis is the value of a given x variable.
  ggplot(.,aes(value, trstplc)) +
  # Create your facet now since the var variable is the particular x variable.
  # Make sure to set scale="free_x" because these x variables are all on different scales
  facet_wrap(~var, scale="free_x") +
  # scatterplot
  geom_point() +
  # linear smoother
  geom_smooth(method="lm") +
  # loess smoother, with different color
  geom_smooth(method="loess", color="black")

# How you should interpret a plot like this: how much does the LOESS smoother and the LM smoother differ?
# My takeaway: not a whole lot. Be mindful the LOESS smoother will want to travel to the outlier (see that cornball obs where a respondent has 40+ years of education)
# Also helpful in determining outliers, it seems.

# If you see any type of "U" shape, this implies a non-linear effect of x on y
# Modeling this is fairly simple, all things considered: lm(y ~ x + I(x^2), data=Data)
# You don't really see that here, but you may encounter it.
# The canonical case here is life satisfaction/happiness and age.
# All things considered, the very young and the very old are happier than the miserable bastards like me in the 30s-late 40s period.

# This plot won't help you with identifying potential interactions.
# Interactions are when the effect of x1 on y depends on the value of x2.
# Modeling this is kind of simple: lm(y ~ x1*x2, data=Data)
# For example, here's a toy one for how age interacts with attitudes toward the police's ability to prevent crime.

summary(M2 <- lm(trstplc ~ agea*plcpvcr, data=ESSBE5))

# Here's how you should both interpret this, with a soapbox pitch for how you should approach interactions.
# The coefficient for age is the effect of increasing age with plcpvcr is ZERO (i.e. police, per the respondent, are completely unsuccessful in preventing crime)
# The coefficient for plcpvcr is the effect of increasing confidence in the crime prevention of the police when age is ZERO. You can already see a problem
# The coefficient for agea:plcpvcr is the estimated change in y when both age and the plcpvcr variable increase.

# Two things come to mind here:
# Always, always, always make sure there's a naturally occurring zero in your IVs, especially when you interact them.
# I will yell at you if you don't. Make your model legible across the board.
# For simple OLS models, this becomes less a problem but for more complicated models, it's REALLY important. 
# You may struggle to find interactions that are actually there because the coefficient can't find a zero that doesn't exist.
# You can do min/max. Pretty sure your readings for the week talked about a min/max transformation
# I prefer to center and scale, such that zero is naturally the mean.
# Second, you can tease this out with a margins plot for the marginal effect of the coefficient.

# I prefer to plot them to illustrate this.
# I'm not going to spoil some of what we'll do later in the semester on quantities of interest, so let's approach it this way.

ESSBE5 %>%
  mutate(agecat = cut(agea, 3)) %>%
  # Why are there 15 year olds in the sample? Anywho...
  # morbidly curious how many people are in each group
  group_by(agecat) %>%
  mutate(ngrp = n()) %>%
  group_by(agecat,plcpvcr) %>%
  summarize(meantrst = mean(trstplc, na.rm=T)) %>%
  ggplot(.,aes(plcpvcr, meantrst,color=agecat,linetype=agecat)) +
  scale_x_continuous(breaks = seq(0, 11)) + geom_line(size=1.1)

# I offer this as only illustrative of potential interactions, and not a whole here as substantive takeaways.
# BUT, if you wanted to do that, I think you can tell a story of what's happening here.
# You might tell a story here that different age groups interpret crime prevention differently
# Older folk see it as a vehicle to trust because it's good job performance.
# Younger folk see it as police overreach, and are less likely to trust the police.
# I won't offer this with a whole lot of conviction, so don't take it as gospel, but just how you could interpret this.

# Now, let's talk about serial correlation/autocorrelation in your errors.
# You can encounter these for any number of reasons, but I think you'll get better at detecting them once you know where they manifest.
# You are, in my experience, more likely to encounter serial correlation/autocorrelation when you have natural groupings in time or space and don't model it.

# For example, here's one of interest for you time series folk, from the post8000r package

TSD
?TSD

# This is a simple/fake time-series data set. You can think of this as analogous to some kind of economic trend like GDP, which is nigh-everywhere increasing.
# Thus, there's a fundamental time component to y here. In fact, I'm deliberately modeling that.
# y, in this data set, is a linear function of a base intercept of 20 + .25*year + .25*x1 + .5*x2 + error.
# So, you want to explain the systematic effects of x1 and x2 on y. They're certainly there, but time is naturally increasing y as well.
# However, time/year is not a real theoretical variable of interest. Time naturally increases. No shit. But there's a time component to y too.


# Here's what will happen if you naively model y with just x1 and x2.

summary(M3 <- lm(y ~ x1 + x2, data=TSD))

# Notice: we didn't at all capture the true effects of x1 on x2. How might we detect that?
# A Breusch-Godfrey Test will check for higher-order serial correlations that we may have missed in our analysis. Let's lookie.

?bgtest

bgtest(M3)

# A Durbin-Watson test will look for autocorrelation, which you typically see more in time-series problems.

?dwtest

dwtest(M3)

# The p-value is the probability we observed the test-statistic if there weas no serial/auto correlation. Notice it's effectively zero.
# In other words: there's serial/auto correlation in the data which, again, no shit. We know there is. Let's lookie:

TSD %>%
  mutate(resid = resid(M3),
         fitted = fitted(M3)) %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point() + geom_smooth()

# Residuals shouldn't look like this.

# There are a few things to do when you have a time-series model, short of fitting an explicit time-series model that falls a bit outside the domain of this class.

# One solution: lag your DV

summary(M4 <- lm(y ~ x1 + x2 + lag(y), data=TSD, na.action=na.exclude))

bgtest(M4)
dwtest(M4)

# The BG statistic is less sanguine about what we did than the DW test. But let's look at the residuals:

TSD %>%
  mutate(resid = resid(M4),
         fitted = fitted(M4)) %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point() + geom_smooth()

# Better. Much better, actually.
# A caution: social scientists are generally *loathe* to treat lagged DVs as a simple fix here, even as they are a simple fix here. Few reasons:
# One: a lag is not a theoretical variable. It communicates something kind of simplistic, all things considered.
# Notice how this manifests in the R2. It's almost a perfect fit!
# In some applications, researchers warn that lags can overwhelm other systematic effects in your time series.
# They don't here, but the potential for this is kind of dissatisfying. There are other systematic effects that influence y, but
# lagging a DV will just reduce the model to a kind of dumb one that says y at time point t is explained by y at time point t-1.
# In my own research, where do I get a time-varying component to something, I'm generally disinclined toward lags myself.

# Here's one other approach:

summary(M5 <- lm(I(y - lag(y)) ~ x1 + x2, data=TSD, na.action=na.exclude))

# Create a difference variable and explain that. Notice: we captured the systematic effects of x1 and x2 here.

bgtest(M5)
dwtest(M5)

# Here, again, the BG test is less sanguine about what we did than the DW test.
# The model is also a discernibly worse fit (i.e. in R-square), but we captured the true effects and didn't really have to model time.

# You could also do an ARIMA model, but that's a different topic for a different class.

# What happens when you have a cross-section component in addition to a time component? You have both spatial and temporal autocorrelation.

TSCSD
?TSCSD

# Here's what will happen if you naively model y with just x1 and x2.

summary(M6 <- lm(y ~ x1 + x2, data=TSCSD))

# Swing and a miss. Not even close.

# There are a few ways to fix this
# The first is fixed effects, which the econometricians love. Here, specify different coefficients for different spatial/temporal groupings.

summary(M7 <- lm(y ~ x1 + x2 + factor(country) + factor(year), data=TSCSD))

# Hot #take, I *despise* this approach in most applications. If you have some covariate that is a group-level effect that varies across groups, don't do this.
# Estimation-wise, it's not a problem at all if you covariates are just individual-level and all you really have are different intercepts.
# That's effectively what we have here.
# However, fixed effects can mislead you greatly if you have coefficients that vary by groups, or you have group-level covariates.

# That hot #take aside, a fixed effect approach takes all the sources of spatial/temporal heterogeneity and isolates them as variables.
# The coeffiicent that emerges is typically uninteresting by itself (another grievance of mine).
# It's just there to draw lines through the data. to soak up variation so you can get what you really want.
# Basically, the coefficient of country11 is the difference of country11 relative to a baseline of country1
# The coefficient for year1951 is the difference in the intercept relative to the baseline of the year 1920.
# Completely arbitrary, (mostly) wholly uninteresting.
# It does that here, though.

# The second is my preferred method: the mixed effects approach.
# This treats the sources of spatial/temporal heterogeneity as random effects and models them as just different intercepts.
# It has the added benefit of allowing slopes (here for x1 and x2 ) to vary if you wanted it.

# this is outside the bounds for this class, but here's how you'd do it
# install.packages("lme4")
# library(lme4)
# summary(M8 <- lmer(y ~ x1 + x2 + (1 | country) + (1 | year), data=TSCSD))
# ^ nailed it.

# Okay, let's talk about heteroskedasticity.
# Heteroskedasticity is a problem in which the residuals are non-constant and typically a function of something (whether OVB or model misspecification)
# You'll know it when you see it: the "cone of shame."

# Here's a way of mimicking this:

set.seed(8675309)
tibble(
  x = rnorm(1000, 5, 2),
  e = rnorm(1000, 0, 1 + .25*x),
  y = 1 + .25*x + e
) -> H

summary(M8 <- lm(y ~ x, data=H))

H %>%
  mutate(resid = resid(M8),
         fitted = fitted(M8))  %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point()

# Behold, your shame.
# One thing, though: notice heteroskedasticity doesn't necessarily bias your analysis/coefficients.
# We're still close to the treatment effect here. 
# But it could, or at least the confidence of it, and it's why you should treat any analysis with a grain of salt if it's there.

# How you can fis this?
# Well, there are a lot of tools and, as far as I learned them, they're more for comparison rather than fixing, per se.
# Toward that end, I'm going to give a canoical case of heteroskedasticity appearing in nature in the CP77 data set from post8000r

CP77
?CP77 # Note, some state abbs here look obviously wrong (e.g. DY, NB). Don't blame me. Blame someone else. :P I ganked it from the robustbase package.


summary(M9 <- lm(edexppc ~ region + urbanpop + incpc + pop, data=CP77))

CP77 %>%
  mutate(resid = resid(M9),
         fitted = fitted(M9))  %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point()

# Double whammy. heteroskedasticity + Hawaii outlier.
# ed. note: it's always Hawaii. Hawaii is an atypical state in a lot of ways, in a lot of applications.

# Let's test for heteroskedasticity using the Breusch-Pagan test from the lmtest package:

bptest(M9)

# Yup. See the p-value.
# Okay, what can we do?
# The good news here, if you can call it that, is heteroskedasticity targets what we can say about the SEs.
# We can use the vcovHC() function from the sandwich package to get, we hope, the "correct" SEs:


# By default vcovHC() estimates a heteroskedasticity consistent (HC) variance covariance matrix for the parameters. 
# There are several ways to estimate such a matrix, and by default vcovHC() estimates the “HC3” one.
# See Zeileis (2004) for more details.


M9 %>% 
  vcovHC() %>% 
  diag() %>% 
  sqrt() %>% 
  as.data.frame() %>% rownames_to_column() %>% as_tibble() %>%
  rename(std.error = 2) %>%
  select(-rowname) %>%
  bind_cols(broom::tidy(M9) %>% select(term, estimate), .) %>%
  mutate(Category = "Heteroskedasticity Consistent Standard Errors") -> M9hcses

M9hcses

# And holy shit, the standard errors are quite different! Here is a takeaway, for inference sake.
M9hcses %>%
  bind_rows(broom::tidy(M9), .) %>%
  mutate(Category = ifelse(is.na(Category), "Normal OLS SEs", Category),
         tstat = estimate/std.error,
         pval = 1.96*pt(-abs(tstat),df=43),
         lwr = estimate - 1.96*std.error,
         upr = estimate + 1.96*std.error) %>%
  filter(term != "(Intercept)") %>%
  ggplot(.,aes(Category, estimate, ymin=lwr, ymax=upr)) + 
  geom_pointrange(position = position_dodge(width = 1)) +
  facet_wrap(~term, scales="free_x") +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip()

# This tells us, unsurprisingly, we should not be so confident in some of our parameters.
# Btw, coeftest from lmtest will do this as well.

coeftest(M9, vcov = vcovHC(M9), type = "HC3")

# There's also robust linear models from the robustbase package.

summary(M10 <- lmrob(edexppc ~ region + urbanpop + incpc + pop, data=CP77))

# IIRC, these estimates will differ slightly because this package is targeting both the heteroskedasticity and the outliers.

# You may also encounter someone who says they're "bootstrapping" their standard errors.
# This always mystified me when I was in grad school, until I figured out it's a lot simpler than it lets on.

# So here's what "bootstrapping" is in this context.
# Let's resample the original CP77 data set, of original length 50, 1000 times
# When we say "resample", this means we're going to sample our original data, WITH REPLACEMENT, of its original length.
# This does mean we could hypothetically get a sample that has Hawaii 50 times, but this is obviously unlikely.
# More likely, though: you might get a state to appear a few times in a given sample.

set.seed(8675309) # Jenny, I got your number...
CP77 %>% 
  modelr::bootstrap(1000) -> bootCP77

# Here's the first one, or at least a summary of it. Notice how Kansas appears three times in this first one.
bootCP77 %>% slice(1) %>% pull(strap) %>% as.data.frame() %>% group_by(state) %>% summarize(n =n())

# Now, for each one, let's do 1,000 regressions on each of those, and store it in the bootCP77 object.

bootCP77 %>% 
    mutate(lm = 
             map(strap, 
                 ~lm(edexppc ~ region + urbanpop + incpc + pop, 
                     data = .))) -> bootCP77


# Let's "tidy" them now.

bootCP77 %>% mutate(tidy =  map(lm, broom::tidy)) -> bootCP77

# here's the first one, for example
bootCP77$tidy[[1]]

# here's the 1000th, one
bootCP77$tidy[[1000]]


# A little more work to do.
# This will create a full damn tidied data.frame.

bootCP77 %>%
  pull(tidy) %>%
  map2_df(., 
          seq(1, resamples), 
          ~mutate(.x, resample = .y)) -> tidiedbootCP77

# And, to summarize this further, we get the sd of the *estimates* as a robust standard error

tidiedbootCP77 %>%
  group_by(term) %>%
  summarize(bse = sd(estimate)) %>% 
  left_join(broom::tidy(M9), .) %>%
  select(term:std.error, bse, everything())

# Superlatives of this approach: super flexible. Bootstrapping SEs, once you get proficient with pipe-oriented programming, can do a lot of cool things.
# Downside: possibly time-consuming, depending on your data.

# No matter, different approaches to heteroskedasticity gives very different SEs.
# Nothing is really a "fix", per se. But compare what you observe and determine how great the problem is.
# I advise use them all as robustness tests and discuss the differences.
