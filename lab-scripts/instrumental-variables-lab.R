#
# Instrumental Variable Regression
# -----------------------------------------------------
# Steve Miller
# Date: 5 March 2020

# Let's get started with updating/installing some packages -----
devtools::install_github("svmiller/post8000r")

# Also install this. estimatr is a nifty package that has some flexible stuff. I probably should've asked you to install this last week too.
install.packages("estimatr")

# load packages for today's lab session
library(tidyverse)
library(post8000r)
library(estimatr)

# This part will be derived from a blog post:
# http://svmiller.com/blog/2019/09/instrumental-variables-2sls/

# You can either install and load stevemisc, or call this function randomly. Your pick.
cor2data <- function(cor, n, seed){
  # number of observations to simulate
  nobs = n

  # Cholesky decomposition
  U = t(chol(cor))
  nvars = dim(U)[1]

  if(missing(seed)) {

  } else {
    set.seed(seed)
  }

  # Random variables that follow the correlation matrix
  rdata = matrix(rnorm(nvars*nobs,0,1), nrow=nvars, ncol=nobs)
  X = U %*% rdata
  # Transpose, convert to data
  # require(tidyverse)
  Data = t(X)
  Data = as.data.frame(Data)
  return(Data)
}

# Here's what's happening here:
# the control variable is 100% unproblematic, uncorrelated with anything.
# treat is correlated with the instrument at .85, but correlated (-.5) with e. So: endogeneity
# instr is an instrument for the treatment variable and is uncorrelated with e.
# ^ in other words, it satisfies all three conditions for an instrument:
# relevance: strong correlation with the treatment
# exclusion: doesn't affect y, only through treatment
# exogeneity: not correlated with the error term e

vars = c("control", "treat", "instr", "e")
Cor <- matrix(cbind(1, 0.001, 0.001, 0.001,
                             0.001, 1, 0.85, -0.5,
                             0.001, 0.85, 1, 0.001,
                             0.001, -0.5, 0.001, 1),nrow=4)
rownames(Cor) <- colnames(Cor) <- vars

# Now let's make some data.
A <- cor2data(Cor, 1000, 8675309) %>% tbl_df() # Jenny I got your number...

# And let's make a y variable. Again: must keep in mind the exclusion restriction
A$y <- with(A, 5 + .5*control + .5*treat + e)


# What happens if we pretend there's no endogeneity problem?
summary(M1 <-lm(y ~ control + treat, data=A))
# Notice: control is 100% unaffected
# treat is way off. Not only is the true effect not even close, but we even get a type 2 error from a NHT standpoint


# Couldn't we just include the instrument here? After all, it's correlated with the treatment.
summary(M2 <-lm(y ~ control + treat + instr, data=A))
# No you can't. This isn't a simple omitted variable problem and, remember, instr has no effect on y by design.

# This is where the IV approach is useful since we've satisfied all three assumptions for it.
# The process looks like this:
# regress your contaminated treatment on the instrument.
# extract the fitted values of it
# regress y on the fitted values of the previous regression.
# this is why we call it two-stage least squares (2SLS) regression

# First-stage model...
summary(FSM <- lm(treat ~ control + instr, data=A))
# ^ notice how the regression estimate basically is the correlation?
# Further, using the control variable here is optional

# Extract fitted values.
A %>%
  mutate(treat_hat = fitted(FSM)) -> A

# Second-stage model...
summary(SSM <- lm(y  ~ control + treat_hat, data=A))
# ^ voila: isolated the true causal effect.

# One caveat: people who specialize in 2SLS/IV regression will caution that what I just did here will isolate the true causal effect
# However, the standard errors will be wrong and need to be adjusted accordingly.
#  the iv_robust command from estimatr will do this for you.
# When using iv_robust, the second stage goes left of | while the first-stage goes right of it.
# Do note there are a few different forms you can do here in the "se_type" option. I'm just going default (which is HC2)
summary(IVM <- iv_robust(y ~ control + treat | instr + control, data=A))

# How different/similar are these results?

broom::tidy(IVM)
broom::tidy(SSM)

# Robust standard error estimation in this context will be less sanguine about your standard errors.
# The main takeaway is case of six-of-one here, though, but be sure to explore how much your standard errors might be affected by this process.
# You could also bootstrap too. Bootstrapping is kind of cool.

Dee04

# Note: Dee is using OLS on binary DVs in parts of his analysis.
# Don't ever do this. Ever. Not even if there's a fire.
summary(D1 <- lm(register ~ college, data=Dee04))

# Okay, we're in orbit of what he reports in Table 1 (first column: registered to vote), but let's back up
# What Dee is trying to unpack is a classic case of a tricky endogeneity problem for a question of supreme importance
# (i.e. the causal effect of higher ed in the U.S.)
# However, there's unobserved determinants of both. Certainly, repsondents in the HS&B data had some civic values prior to college.
# Clearly, college entrance can't explain something that happened before it.

# Dee proposes an intrument that Card (1995) had actually introduced before.
# That is: the geographic availability of colleges closer to the respondent.
# Intuitively, more colleges closer to the respondent (esp. students from disadvantaged backgrounds) should explain college attendance,
# but those should not have any effect on adult outcomes.

# So, Dee grabbed a "distance" variable, which is the distance in miles between the respondent's high school and the nearest college in the county at that time.
# There's a lot to like about this instrument. Let's start intuitively.
# 1) it should satisfy the exogeneity condition. These colleges exist well prior to the child appearing in the HS&B data.
# 2) you could argue it satisfies the relevance condition. The closer a college, the cheaper it is to attend, and the more opportuity to attend.
# 3) it should satisfy the exclusion restriction. Distance affects college, which affects register.
# BUT: distance should not be directly correlated with register, "only through" college.

# Let's check to see how well relevance and exclusion are satisfied here:
Dee04 %>%
  summarize(relevance = cor(distance, college),
            exclusion = cor(distance, register))

# Enh? Seems like a weak instrument on the relevance frontier, but, if you wanted, a cor.test will vindicate it. Observe:
cor.test(Dee04$distance, Dee04$college)

# In other words, it's not a strong correlation, but it's one that doesn't include zero.
# How about the exclusion restriction?
cor.test(Dee04$distance, Dee04$register)

# ^ includes zero. So that'll satisfy the exclusion restriction (I think).
# Alrightie, let's get to it then.

summary(D2 <- iv_robust(register ~ college + black + hispanic + female | distance + black + hispanic + female, data=Dee04))
# ^ my takeaway: I think we're off from Table 2 because of the nature of abbreviated data I was able to get.

# What if we do this manually?

FSM <- glm(college ~ distance, data=Dee04)

Dee04 %>%
  mutate(college_hat = fitted(FSM)) -> Dee04

# Again, don't do this
summary(SSM <- lm(register ~ college_hat + black + hispanic + female, data=Dee04))

# Same thing.
