#
# Measurement Error
# -----------------------------------------------------
# Steve Miller
# Date: 12 February 2020
# See also: http://svmiller.com/blog/2020/02/random-measurement-error/
# And: http://svmiller.com/blog/2019/09/instrumental-variables-2sls/

# Let's get started with updating/installing some packages -----

# optional: install my stevemisc package
# my stevemisc package has the cor2data function
# you can also elect to not install it by loading the function manually (see below)
devtools::install_github("svmiller/stevemisc")

library(tidyverse)


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


# Let's create a few data sets from the cor2data function
# However, this function (as you can see above) requires a correlation matrix.
# Let's create a few.
# First, we'll create one where two x variables are uncorrelated with each other.

vars = c("x1", "x2",  "e")
Cor <- matrix(cbind(1, 0.001, 0.001,
                    0.001, 1, 0.001,
                    0.001, 0.001, 1),nrow=3)
rownames(Cor) <- colnames(Cor) <- vars

# Notice below: x1, x2, and the error term are obviously perfectly correlated with itself (the diagonals)
# However, there are negligible correlations with anything else in the matrix (the .001s).
Cor

# We'll create a data frame (A) from these data. The data frame will have 1000 observations

A <- cor2data(Cor, 1000, 8675309) %>% tbl_df() # Jenny I got your number...

# Next, we'll create a a correlation matrix (with just x1, x2, and e) where x1 and x2 are modestly correlated (.35)
Cor <- matrix(cbind(1, .35, 0.001,
                    .35, 1, 0.001,
                    0.001, 0.001, 1),nrow=3)
rownames(Cor) <- colnames(Cor) <- vars

# Then, a data frame B that simulates data from this correlation matrix.

B <- cor2data(Cor, 1000, 8675309) %>% tbl_df() # Jenny I got your number...

# Next up: a correlation matrix where x2 is correlated with the error term  at .35
Cor <- matrix(cbind(1, .001, 0.001,
                    .001, 1, .35,
                    0.001, .35, 1),nrow=3)
rownames(Cor) <- colnames(Cor) <- vars

# Then, a data frame B that simulates data from this correlation matrix.

C <- cor2data(Cor, 1000, 8675309) %>% tbl_df() # Jenny I got your number...


# For simplicity's sake, let's create an outcome y for data frames A, B, and C.
# Simply: the outcome y is a function of a slope-intercept of 1 + x1 + x2 + e.
# In regression parlance: the coefficients for x1 and x2 are 1 and the estimated value of y = 1 when x1 and x2 are 0 (plus e, though)
# For simplicity's sake, I'm giving the error term e equal footing with x1 and x2. 
# I.e. there is still a lot left unexplained about y in the error term e.

A$y <- with(A, 1 + x1 + x2 + e)
B$y <- with(B, 1 + x1 + x2 + e)
C$y <- with(C, 1 + x1 + x2 + e)

# Let's start with the ideal case of no measurement error.
# In this case, we'll be starting with data frame A and including all known effects on y (minus the unobserved e, obviously.)

summary(MA1 <- lm(y ~ x1 + x2, data=A))

# Barring the effect of x1, which is randomly more than two standard errors away from the known effect of x1 on y,
# our OLS regression is well within orbit of what the true effects are for x1 and x2 on y.
# NOTE: if you were to expand the number of observations, you'd get that x1 to include 1. This is estimation uncertainty.


# Now, let's assume some omitted variable bias in the A data frame.
# We know y is in part a function of x1. We have x1. We're not going to include x1, though.

summary(MA2 <- lm(y ~  x2, data=A))

# Notice x2's effect on y is materially unaffected by the exclusion of x1.
# This is because x1 and x2 are uncorrelated with each other.
# Omitted variable bias is no bias at all when some other covariate is uncorrelated with the other right-hand side stuff.


# What happens when x1 and x2 are moderately correlated, as in data frame B? Recall:
with(B, cor(x1, x2))

# Let's see:
summary(MB1 <- lm(y ~ x1 + x2, data=B))

# Look familiar? Looks very much like MA1 using the A data frame where both x1 and x2 are uncorrelated.
# Here, in data frame B, they're clearly correlated. But OLS separates that out and returns partial effects as regression coefficients.

# Now, let's exclude x1.

summary(MB2 <- lm(y ~  x2, data=B))

# See what happens? The exclusion of x1 biases the coefficient for x2 up because of the positive correlation between x1 and x2.
# In other words: x2 partly includes x1's effect on y as well. This is omitted variable bias "biasing" the direction/strength of the relationship.
# The true relationship between x2 and y is 1. Our estimate is about 1.3. Muy malo.

# What about when one of the variables is correlated with the error term, which we don't directly observe in our data?
# In other words, there's something in the ether that is not being captured by us as researchers that is correlated with a predictor of interest (x2).
with(C, cor(x2, e))

summary(MC1 <- lm(y ~ x1 + x2, data=C))

# Yikes. x2's effect is greatly overstated because it's positively correlated with an unobserved error term that we don't directly model in the regression.
# Worse, again: error terms include everything not formally modeled. I.e. we know in simulation we could fix this, but, in the real world, you don't have the data available (yet).
# This will motivate a lot of the instrumental variables stuff we'll talk about later in the semester.

# What about random measurement error?
# This will dovetail a lot with what I do here:
# http://svmiller.com/blog/2020/02/random-measurement-error/
# tl;dr: let's introduce some random error at every 10th value of either y or x2 to see what happens.

new_vals <- c(-500, -100, -10, -5, -3, -2, -1, 0,
              1, 2, 3, 5, 10, 100, 500)

new_vals <- c(seq(1:4), seq(5, 50, 5), 75, seq(100, 500, 100))

new_vals <- c(new_vals*-1, 0, new_vals)

# We're going to be doing this a few times, so let's make a function of it to reduce redundant code.
# To make this tractable, let's focus on just x2

loopranderrx2 <- function(dat, nv){
  output <- tibble()
  for (i in nv) {
    # Looping through nv
    # For every 10th value for x2, recode it to whatever the ith value is in nv
    dat %>%
      mutate(x2re = ifelse(row_number() %in% c(seq(0, 1000, by =10)), i, x2)) -> dat
    # regress y  on this particular new x2nv variable
    mod <- lm(y ~ x1 + x2re, data=dat)
    # grab r-square
    r2 = broom::glance(mod)[1,1] %>% pull()
    # create a broom table that codes whatever the ith value of new_vals is, and the adjr2 as well
    broomed = broom::tidy(mod) %>% mutate(mod = i, r2 = r2)
    # bind it to otput
    output = bind_rows(output, broomed)
  }
  return(output)
}

# Also, graphing can be code-heavy. So:
# (trust me, I'm a political scientist.)

ggloopranderrx2 <-  function (data, comparison) {
  data %>%
    mutate(term = ifelse(grepl("x2", term), "x2", term)) %>%
    mutate(lwr = estimate - abs(qnorm(.025))*std.error,
           upr = estimate + abs(qnorm(.025))*std.error) %>%
    ggplot(., aes(as.factor(mod), estimate, ymin=lwr, ymax=upr)) +
    #theme_steve_web() +
    # post_bg() +
    facet_wrap(~term) +
    scale_y_continuous(breaks = seq(0, 1, by = .2)) +
    geom_pointrange() +
    geom_rect(data=comparison, aes(x = NULL,y = NULL,xmin=-Inf, xmax=Inf, 
                              ymin=lwr, ymax=upr), 
              alpha=0.1,fill="red") +
    coord_flip() 
}


# Let's start with x2 for A
AX2 <- loopranderrx2(A, new_vals)

# grab/broom up MA1, for context
MA1df <- broom::tidy(MA1) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error)  %>%
  mutate(r2 = broom::glance(MA1) %>% pull(1))

# Now, let's compare....

ggloopranderrx2(AX2, MA1df) +
  labs(x = "Every 10th Observation of x2 is Recoded to This Value",
       y = "Coefficient (with 95% Intervals)",
       title = "The Effect of Random Measurement Error in x2",
       subtitle = "Random measurement error in x2 plummets the estimated relationship of x2 on y to zero, making it hard to discern a signal that objectively exists.",
       caption = "Shaded area communicates 95% Intervals from OLS with no measurement error.")

# Implication: huge random error in x2 pushes the effect of x2 on y to zero.
# If you were to compare the r-squareds (see my blog post), you'd see the effect is to collapse r2 to a stepwise model where you just didn't estimate x2 at all.

# What about when x1 and x2 are a little correlated?

BX2 <- loopranderrx2(B, new_vals)

# grab/broom up MB1, for context
MB1df <- broom::tidy(MB1) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error)  %>%
  mutate(r2 = broom::glance(MB1) %>% pull(1))

# Now, let's compare....

ggloopranderrx2(BX2, MB1df) +
  labs(x = "Every 10th Observation of x2 is Recoded to This Value",
       y = "Coefficient (with 95% Intervals)",
       title = "The Effect of Random Measurement Error in x2",
       subtitle = "When x2 and x1 are correlated, the introduction of random error to x2 is equivalent to omitted variable bias.",
       caption = "Shaded area communicates 95% Intervals from OLS with no measurement error.")

# Now, when x2 is correlated with the error term.
CX2 <- loopranderrx2(C, new_vals)


ggloopranderrx2(CX2, MB1df)  + # I know this is MB1df, but why do you want biased x2 here? You already know it's 1.
  labs(x = "Every 10th Observation of x2 is Recoded to This Value",
       y = "Coefficient (with 95% Intervals)",
       title = "The Effect of Random Measurement Error in x2",
       subtitle = "When x2 is correlated with the error term, the introduction of random error to x2 has similar effects to when there is no correlation with the error term in an ideal application.",
       caption = "Shaded area communicates 95% Intervals from OLS with no measurement error.")

# Now, let's look at random error in y

loopranderry <- function(dat, nv){
  output <- tibble()
  for (i in nv) {
    # Looping through nv
    # For every 10th value for y, recode it to whatever the ith value is in nv
    dat %>%
      mutate(yre = ifelse(row_number() %in% c(seq(0, 1000, by =10)), i, y)) -> dat
    # regress y  on this particular new x2nv variable
    mod <- lm(yre ~ x1 + x2, data=dat)
    # grab r-square
    r2 = broom::glance(mod)[1,1] %>% pull()
    # create a broom table that codes whatever the ith value of new_vals is, and the adjr2 as well
    broomed = broom::tidy(mod) %>% mutate(mod = i, r2 = r2)
    # bind it to otput
    output = bind_rows(output, broomed)
  }
  return(output)
}

ggloopranderry <-  function (data, comparison) {
  data %>%
    mutate(lwr = estimate - abs(qnorm(.025))*std.error,
           upr = estimate + abs(qnorm(.025))*std.error) %>%
    ggplot(., aes(as.factor(mod), estimate, ymin=lwr, ymax=upr)) +
    #theme_steve_web() +
    # post_bg() +
    facet_wrap(~term) +
    # scale_y_continuous(breaks = seq(0, 1, by = .2)) +
    geom_pointrange() +
    geom_rect(data=comparison, aes(x = NULL,y = NULL,xmin=-Inf, xmax=Inf, 
                                   ymin=lwr, ymax=upr), 
              alpha=0.1,fill="red") +
    coord_flip() 
}

# Now, what happens when y is contaminated by random error?

AY <- loopranderry(A, new_vals)

ggloopranderry(AY, MA1df)

# General takeaway: random error in y will mess with the intercept, but it won't bias x1 and x2. 
# What you don't really see, though, is how wide those s.es are getting for x1 and x2
# In other words, the noisier y is, the more fruitless it is try to systematically model it. It's noise.

# What about in B when x1 and x2 are a little correlated?
BY <- loopranderry(B, new_vals)

ggloopranderry(BY, MB1df)

# Same thing. You're trying to fit a slope-intercept equation to noise.
# It's a bold strategy, Cotton.

# What about when x2 is endogenous to e?

CY <- loopranderry(C, new_vals)
ggloopranderry(CY, MB1df)

# same thing. Not that regressing y on a known endogenous parameter is a good idea, but alas. Same takeaway.
