#
# Logistic Regression
# -----------------------------------------------------
# Steve Miller
# Date: 26 March 2020

# Let's get started with updating/installing some packages -----
devtools::install_github("svmiller/post8000r")

# This lab script will lean on an analysis I did here in 2017:
# http://svmiller.com/blog/2017/04/age-income-racism-partisanship-trump-vote-2016/

library(tidyverse)
library(post8000r)
library(lmtest)

?TV16
TV16

# Let's see if we can explain the Trump vote among white people in Pennsylvania.
TV16 %>%
  filter(state == "Pennsylvania" & racef == "White") -> Penn

# Let's try a linear model for this, wherein we are estimating a white person's Trump vote as a function of:
# the person's age, gender, education (if they have a college diploma), household income,
# partisanship (D to R), ideology (L to R), whether they're a born-again Christian,
# and the cognitive racism and emapthetic racism variables
# Read the blog post for more description about what these are.
M1 <- lm(votetrump ~ age + female + collegeed + famincr + 
           pid7na + ideo + bornagain + lcograc + lemprac,
         data = Penn, na.action=na.exclude)

# Let's get a summary
summary(M1)

# I will caution that these effects are not interpretable; "linear probability model" be damned.
# One interpretation here, looking at the ideo variable: a one-unit increase in ideology increases
# the expected value(?) of voting for Trump by .058. I mean, okay...

# Remember: OLS is BLUE iff (sic) its underlying assumptions are met.
# Let's check if the assumptions are met.
# Is there serial correlation in the data?
bgtest(M1)
dwtest(M1)

# Probably not, which makes sense. By selecting on just Pennsylvania at one point in time, we don't seem
# to have problems of serial correlation, spatially or temporally.
# Let's test for heteroskedasticity using the Breusch-Pagan test from the lmtest package:

bptest(M1)

# yeah, no shit you have heteroskedastic errors. You only have two possible responses!
# Our DV isn't normal, something a Shapiro-Wilk test will also show:

shapiro.test(Penn$votetrump)

# Do see this for a discussion of the limits of normality tests, however:
# https://stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless

# Put it this way: one of the assumptions of OLS is your DV is drawn from a normal distribution with set mean and sd/variance.
# This would be a normal DV with similar parameters.

set.seed(8675309) # Jenny, I got your number...
tibble(x=rnorm(nrow(Penn), mean(Penn$votetrump, na.rm=T), sd(Penn$votetrump, na.rm=T))) %>%
  ggplot(.,aes(x)) + geom_density() +
  scale_x_continuous(breaks = seq(-2, 2, by =.25))

# Instead, you gave it something like this:
Penn %>% na.omit %>%
  ggplot(.,aes(as.factor(votetrump))) + geom_bar()

# tsk, tsk tsk. No, no, and no.

# There are people who contend that it's okay to use OLS on a binary outcome because the estimate reveals the probability of 1.
# This is called, euphemstically, the "linear probability model."
# If you try this in your paper, I will throw something at you.
# For one, the errors will be heteroskedastic, as we mentioned.
# You might object that you can do H-W, bootstrapped or some other form of heteroskedasticity-corrected SEs to fix that.
# My retort: *no you may not*. In fact, those types of SE corrections reveal more than they fix.
# See: https://gking.harvard.edu/files/gking/files/robust_0.pdf

# Further, you will invariably return fitted values of y that break the rules of probability.
# Observe:

Penn %>%
  mutate(yhat = predict(M1)) %>%
  ggplot(.,aes(yhat, votetrump)) +
  geom_point(alpha=0.2, color="black") +
  geom_vline(xintercept = 1, linetype="dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text", label="Impossible predictions\n(yhat > 1)",
           x = 1.05, y=.5, hjust="left") + 
  annotate("text", label="Impossible predictions\n(yhat < 0)",
           x = -.05, y=.5, hjust="right")

# I'm willing to be flexible and open to interpretation about linear models on things like four-item DVs (definitely don't)
# five-item Likerts (enh, also don't do that...) or seven-item ordinal variables (I'm listening...), 
# or ordinal items on a larger scale like 10 points (about where I'd be).
# Under those circumstances, check your y-hats.
# Anything outside the bounds of the scale will tell you not to use an OLS under that circumstance.
# When all y-hats are within that scale, you might spin a ball of yarn to me about how the discrete nature of the DV
# are our only measurements of some underlying continuum. I'm willing to hear those out, under those circumstances.
# However, the answer is *always* NO when it comes to linear models on binary DVs.
# Use a logistic regression instead.

M2 <- glm(votetrump ~ age + female + collegeed + famincr + 
           pid7na + ideo + bornagain + lcograc + lemprac,
         data = Penn, na.action=na.exclude,
         family = binomial(link="logit"))

summary(M2)

# Just because everything that was significant in M1 is significant in M2 is beside the point.
# That's not a justification for using an LM on a binary DV.

# Recall: these are logits (i.e. natural logged odds). How might you make these more flexible/accessible?
# PRACTICALLY: wait until we get to the week on simulation, especially for more complicated models with lots of covariates.

# 1) Inverse the logit to return a probability.
# Intuition here is simple: inverse the logit to return a probability.
# My only misgiving: for full models, RESIST THE URGE TO DO THIS MANUALLY. You will make errors.
# Wait until we get to the week on making the most of regression to start doing serious quantities of interest.

# Nevertheless, let's do a simple exercise here to illustrate the intuition.
# Here is a simple model where we have just one covariate: collegeed.
# FWIW, I select just this one covariate because, by itself and with no other IVs, the coefficient is almost the same as the full model.
# Plus, it's binary, so the calculation here will be simple.

M3 <- glm(votetrump ~ collegeed, data=Penn, na.action = na.exclude,
          family = binomial(link="logit"))
summary(M3)

# Now, let's write a simple custom function that inverts the logit
invlogit <- function(x) {1/(1 + exp(-x))}

# Then, let's tidy up our model
broom::tidy(M3) -> tidyM3
interceptM3 <- tidyM3[1, 2] %>% pull()
coefM3 <- tidyM3[2, 2] %>% pull()

# ^ Btw, this is why I'm telling you to resist the urge to do this manually for full models.
# ESPECIALLY when you you don't have naturally occuring zeroes in most of the parameters.

# And, let's calculate this manually:
invlogit((interceptM3 + coefM3)) - invlogit(interceptM3)

# Interpretation: having a college education decreases the probability of voting for Trump among Pennsylvania whites by about 20 percentage points.
# Some caveats:
# - Again, don't do this manually for full models. Get some quantities of interest. We'll talk about this in a few weeks.
# - You *can't* just isolate the coefficients to do this. Gotta include the intercept.
#    (i.e. p(y = 1 | x = 1) - p(y = 1 | x = 0) must include the intercept).
# See:
invlogit(coefM3)
# Consider this a tentative estimate too. Truly, esp. for GLMs, quantities of interest are what you need to more fully communicate the effects you want.
# We'll get to that.

# 2) The "divide by 4" rule
# Gelman and Hill (2006, 82) have one nice rule of thumb. They call it the "divide by 4" rule.
# tl;dr: the logistic curve is steepest at the center. The slope of the curve (i.e. the derivative of the logistic function) is maximized
# at the point where Be^0/(1 + e^0)^2 = B/4. B/4 is the *maximum* difference in p(y = 1) for a one-unit change in x.
# Thus, the "divide by 4" rule gets you an *upper bound* of the predictive difference. (Note: does not apply to intercept/constant, obviously)

# Indeed, it's not bad as a rule of thumb:
invlogit((interceptM3 + coefM3)) - invlogit(interceptM3)
coefM3/4

# ^ not bad. Not perfect, but not bad.
# Let's look at somethings from M2 now

broom::tidy(M2) %>%
  select(1:2) %>%
  mutate(db4 = estimate/4) %>%
  slice(-1) # does not apply to constant/intercept

# 3) Odds ratios
# Odds ratios may also be useful ways of summarizing a logistic regression.
# I encourage you to get flexible with logits, but odds ratios have an advantage over probabilities because they can scale up infinitely.
# Probabilities are left-bound and right-bound. Odds and odds ratios are only left-bound. Odds/odds ratios less than 1 are negative effects.
# FWIW, I wince every time I see logistic regression models communicated with odds ratios.
# Namely, if you're arguing for a negative effect, the odds ratio kind of obscures that because they're left-bound at 0.
# I prefer to communicate them on their original scale and encourage you to get comfortable with the same.
# Yet, here's how you could do it. Just exponentiate what the logistic regression model gives you.
# Let's also add confidence intervals for funsies.

broom::tidy(M2) %>%
  bind_cols(., as_tibble(confint(M2))) %>%
  mutate_at(vars(2:ncol(.)), exp)

# Model Checking ------

# 1) Binned residuals
# With OLS, you compare the fitted values and residuals with a simple scatterplot.
# With logistic regression, you get something similar, but recall the residuals are discrete because the DV is discrete.

Penn %>%
  mutate(fitted = predict(M2, type="response"),
         resid = residuals(M2, type = "response")) -> Penn

# Gelman and Hill have a rule of thumb for determining the size of the bins.
# For larger data sets (n >= 100, which we have here), the number of bins are determined as the (rounded down, i.e. "floored") 
# square root of number of obs in the model
nbins <- floor(sqrt(nobs(M2)))

# Then, across these number of bins, we're going to look for the corresponding obs location in the fitted values.
breaks.index <- floor(length(M2$fitted.values) * (1:(nbins - 1)) / nbins)
# The breaks that emerge will coincide with those fitted values
breaks <- unique(c(-Inf, sort(M2$fitted.values)[breaks.index], Inf))

# ^ Interest of full disclosure: I need to think about how I want to make this more flexible.
# This is a Steve problem for future classes, but this will converge on what arm::binnedplot will give you.
# Also need to think about how to allow you to do this across x-vals as well.
# You could install the performance package if you'd like
# install.packages(c("parameters","performance","easystats","see"))
# I think that'd allow you to do it, but my goal in this class is to get you to install as few packages as possible.


Penn %>% select(fitted, resid) %>% na.omit %>%
  mutate(bin = as.numeric(cut(M2$fitted.values, breaks))) %>% 
  group_by(bin) %>% 
  summarize(min = min(fitted, na.rm=T),
            max = max(fitted, na.rm=T),
            n = n(),
            meanfit = mean(fitted, na.rm=T),
            meanresid = mean(resid, na.rm=T),
            sdresid = sd(resid, na.rm=T),
            se = 1.96*sdresid/sqrt(n),
            inbounds = ifelse(between(meanresid, -se, se), 1, 0)) -> Bins


# Let's check to see where we're out of bounds:
# I've always been told the ideal interpretation here is that you want 95% of your bins "in bounds."
# We won't have that here, it seems. Let's see where:
Bins %>%
  filter(inbounds == 0)

# Does look like our model isn't a great fit at 1) the tail ends of the distribution where the probabilities are touching 0 or 1.
# Also, we have that one odd-ball outlier in the .741-.784 bin.
# FWIW, don't fret the outliers that close to the bounds of 0 and 1. Not ideal, but not terribly problematic.

# You could importantly plot them as well.

Bins %>%
  ggplot(.,aes(meanfit, meanresid)) + geom_point() +
  geom_smooth() +
  geom_ribbon(aes(ymin = -Inf, ymax = -se), alpha = .1 , fill = "red") +
  geom_ribbon(aes(ymin = se, ymax = Inf), alpha = .1 , fill = "red") +
  geom_line(aes(y = se), colour = "grey70") +
  geom_line(aes(y = -se), colour = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed")

# When you see a plot like this, check for a pattern. There really isn't much one to speak of. At least I don't think.
# However, if you're worried your model might not be the best fit given what you see from this plot, consider the following:
# 1) You might have some hidden interactions you should find. Think esp. of those large residuals as illustrative of that.
# 2) You may have some outliers, a non-linear pattern, or one of your covariates may have a skew.
#   Under these conditions, consider some type of transformation (logarithmic, square-root) 
#   or allowing an effect to be non-linear (e.g squaring it).

# 2) How Well are you predicting the 1s?
# Another form of model "checking" is more model "validation."
# Notice how the fitted values are predicted probabilities of 1.
# So, let's create two summaries to see how well our model is understanding the Trump vote.
# In the first case, `meandv` is just the proportion of 1s. It's a null model.
# Basically, if we just predicted 1 because the mean was 1, the proportion of times we were right would be the mean.
# The proportion of times we were wrong would be 1 - mean.
# In the second case, `errorrate` is the proportion of cases where we predicted a Trump vote, given the model, but were wrong 
# or the times we predicted the Pennsylvania white wouldn't vote for Trump, but did.

Penn %>%
  select(votetrump, fitted) %>%
  summarize(meandv = 1 - mean(votetrump, na.rm=T),
            errorrate = mean((fitted > .5 & votetrump == 0) | fitted <.5 & votetrump == 1, na.rm=T))

# As you evaluate this kind of output, keep the following in mind:
# You should want your error rate to be smaller than .5, much smaller even.
# If your error rate were greater than .5, you could simply set all betas to 0 and get a better fitting model.
# Here, I think we're doing pretty damn good, all things considered. We're correctly predicting about 87% of the cases.

# 3) Deviance comparisons
# Recall: deviance is a measure of error and replaces R2 as a measure of model fit in a GLM like a logistic regression.
# It's analogous to the residual standard deviation.
# Lower deviance is better: it means your fitted values better "fit" to the observed values.
# Much like R2, though, it decrases the more ingredients you throw in the proverbial broth. You could overfit a model, which you shouldn't.
# One way of thinking about this: if you added a variable that was 100% random noise, the deviance would decrease by 1, on average.
# If you added a variable that was informative/useful, the deviance would decrease by more than 1.
# This is additive: adding k predictors to a model should decrease the deviance by k (or more).

# Let's illustrate this. First, let's add a completely uninformative variable.

set.seed(8675309) # Jenny, I got your number...
Penn %>%
  mutate(noise = rnorm(n())) -> Penn

M4 <- update(M2,. ~ . + noise)
summary(M4)
# ^ Notice: the noise variable adds nothing of value to the model.

# How much did we reduce the deviance in the model
M2$deviance - M4$deviance
# ^ less than 1, even. Almost zero reduction in the deviance.
# Unsurpising: the variable is noise.

# Let's do something a bit more substantive, and a bit more timely.
# Pundits, newsies, and various other talking heads have been notoriously reticent to chalk up the Trump vote to racism.
# It's uncomfortable for these people to characterize or describe racism in their potential consumer base.
# I'm not sure how many business classes encourage characterizing their consumers as racist. You're getting the picture.
# So let's set this question up as an empirical one:
# 1) How much worse off would our error rate be if we didn't model racism?
# 2) How bigger would the deviance be in the absence of these two racism variables?
# Let's see: 

# Simple update of the model to omit the two racism variables.
M5 <- update(M2,. ~ . - lcograc - lemprac)
summary(M5)

M5$deviance - M2$deviance
# Quite the change in deviance. How might an ANOVA summarize this?

# What will an ANOVA tell us?
anova(M5, M2, test="Chisq")
# It's suggesting what your eyes and ears have been telling you since 2016.
# You can learn a lot about the Trump vote (at least among Pennsylvania whites) as a function of racism (here: cognitive and empathetic).
# The full model (M2) produces statistically significant results for both of them, each with relatively large magnitude effects.
# Makes sense M2 would be the better fit than M5, no? That's what the ANOVA is telling you here.

# A likelihood ratio test will tell you the same thing, btw, with the exact same statistic
# from lmtest
lrtest(M5, M2)
# ^ one note above: this is a likelihood ratio test. I've previously been discussing deviance,
# but, incidentally, the log likelihood of a GLM * - 2 returns the deviance.
# If you ever see, anywhere in your travels, a statistic of "-2LL", it's the deviance.

# FWIW, few people (in my experience) ask to see ANOVA. You can pitch model comparison here as an error rate stat.

Penn %>%
  rename(fittedM2 = fitted) %>%
  mutate(fittedM5 = fitted(M5, type="response")) %>%
  select(votetrump, fittedM2, fittedM5 )  %>%
  summarize(meandv = 1 - mean(votetrump, na.rm=T),
            errorrateM2 = mean((fittedM2 > .5 & votetrump == 0) | fittedM2 <.5 & votetrump == 1, na.rm=T),
            errorrateM5 = mean((fittedM5 > .5 & votetrump == 0) | fittedM5 <.5 & votetrump == 1, na.rm=T))

# Implications: M2 is a better fit than M5 deviance-wise. 
# Error rate wise, M5 is a bit worse than M2. Not terribly so, though. 
# But I'm confident the difference in the error rate is a "significant" one, if you will.
