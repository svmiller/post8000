#
# OLS Regression
# -----------------------------------------------------
# Steve Miller
# Date: 19 February 2020

# Let's get started with updating/installing some packages -----
devtools::install_github("svmiller/post8000r")


# optional: install my stevemisc package
# my stevemisc package has the cor2data function
# you can also elect to not install it by loading the function manually (see below)
devtools::install_github("svmiller/stevemisc")

library(tidyverse)
library(post8000r)



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

# Let's start with just one variable
vars = c("x1",  "e")
Cor <- matrix(cbind(1, 0.001,
                    0.001, 1), nrow=2)
rownames(Cor) <- colnames(Cor) <- vars

A <- cor2data(Cor, 1000, 8675309) %>% tbl_df() # Jenny I got your number...

A$y <- with(A, 1 + x1 + .5*e)

summary(M1 <- lm(y ~ x1, data=A))

# Btw, if you just wanted to do a simple lm with one y on one x, ggplot could do this for you.
A %>%
  ggplot(.,aes(x1,y)) +
  geom_point() +
  geom_smooth(method ="lm")

# One of the properties of the SRF with one x is the regression line will travel through the mean of y and x. Observe:
a_meany = mean(A$y)
a_meanx1 = mean(A$x1)

A %>%
  ggplot(.,aes(x1,y)) +
  geom_point() +
  geom_smooth(method ="lm") +
  geom_vline(xintercept = a_meanx1) +
  geom_hline(yintercept = a_meany) 

# One of the assumptions is the mean of residuals will be zero and x is uncorrelated with the residuals.
# observe:
A %>%
  mutate(resid = resid(M1)) %>%
  summarize(corx1 = cor(resid, x1),
            meanresid = mean(resid))


# Always, always, always do some diagnostics on your OLS model.
# This will, in practice, mean looking at your residuals and extracting your fitted variables.
# Let's do that:

A %>%
  mutate(resid = resid(M1),
         fitted = fitted(M1)) -> A

# 1) Compare the fitted versus the residual:
# Fitted vs. residual plots should more or less look random.
# The line through it should be flat/straight through 0 on the y-intercept

A %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept = 0, linetype="dashed")

# 2) The second plot is a normal Q-Q plot of the residuals
# If the the line is straight, it suggests the errors are normally distributed

A %>%
  ggplot(.,aes(sample=resid)) + geom_qq()

# 3) The third plot is a scale-location plot
# Basically: plot the fitted values against the square root of the absolute values of the residuals
# This too should effectively be random

A %>% 
  mutate(absqresid = sqrt(abs(resid))) %>%
  ggplot(.,aes(fitted, absqresid)) +
  geom_point() + geom_smooth(method = "lm")

# 4) The fourth plot is a residuals vs. leverage plot
# This will help you find any influential cases in your data.
# These residuals vs. leverage plots are typically communicated with standardized residuals
# Some definitions:
# -- outlier: an observation with a large residual
# -- leverage point: an observation with an x value far from the mean.
# -- influence observation: an observation that changes the slope of the line
# One way around this is to compare the regression line with and without those observations.

A %>%
  mutate(stdresid = rstandard(M1), # standardized residuals, via handy built-in function
         lev = hatvalues(M1)) %>% # leverages. Trust me, just do it this way. matrix algebra is a pain.
  ggplot(.,aes(lev, stdresid)) +
  geom_point() +
  geom_smooth(method ="loess")

# One thing happening underneath the proverbial hood of these diagnostic plots is a Cook's distance score.
# Cook's distance is a measure used to calculate influence in OLS. Higher values of Cook's D = more influence.
# So what defines "high" and "influencer?" Some studies recommend a threshold of 4/n, or 4/(n-k-1).
# Alternatively: check if it's above 1 if your data are sufficiently large.
# Values above that are problematic. Even then, so many rules of thumb.
# My hot #take: this is a "use your head" problem, following a graph of Cook's D scores.
# And you'll need to eyeball this, as I've already done:

tibble(obs = 1:1000,
       cooksd = cooks.distance(M1)) %>%
  bind_cols(A, .) -> A

A %>%
  ggplot(.,aes(obs, cooksd)) + geom_bar(stat="identity") +
  geom_hline(yintercept = .01, linetype="dashed") +
  geom_text(data=subset(A, cooksd>.01), aes(y=cooksd, label=obs), size=4)

# Traditional logic (and this is still valid): if you have problematic observations/influencers, punt them out,
# re-run the model, and compare the results.
# My hot #take: if you do that, the Cook's D scores for the new regression will change as well. Cook's D is kind of relative.
# Basically, take a look, find influencers/outliers, but don't try to wed yourself to Cook's D as a diagnostic.

# Base R has a lot of plots that could do this manually for you.
# It won't be as pretty, and you shouldn't present them "as is", but they're a little convenient.
# FYI: I hate built-in functions that make me hit return to see the next plot.
# It's why I did this the ggplot way.

plot(M1)

# This will return the four biggies that I described above, but you could grab an individual one, if you'd like
# See:

plot(M1, which=1)

# Want the Cook's D histo? That's no. 4.

plot(M1, which=4)

# There is one other too:
plot(M1, which=6)
# It plots the leverage points against Cook's D. Never have been a big fan of this plot.


# Let's replicate Newhouse (1977) now

Newhouse77

# Here's Table 2
summary(M1 <- lm(medexppc ~ gdppc , data=Newhouse77))
summary(M2 <- lm(medexppc ~ gdppc, data=subset(Newhouse77, country !="Greece")))

# Is Greece that much an outlier?

Newhouse77 %>%
  mutate(cooksd = cooks.distance(M1))

# I mean, sure, but it seems like Sweden and the U.S. are bigger influencers.
# Let's kick them out, see what happens.

summary(lm(medexppc ~ gdppc, data=subset(Newhouse77, country != "Sweden")))
summary(lm(medexppc ~ gdppc, data=subset(Newhouse77, country != "United States")))

# Hmm, how about this

countries <- Newhouse77$country

loopcountries <- function(dat){
  output <- tibble()
  for(i in countries) {
    dat %>%
      filter(country != i) -> dat1
    mod <- lm(medexppc ~ gdppc, data = dat1)
    broomed <- broom::tidy(mod) %>% mutate(omitted = i)
    output <- bind_rows(output, broomed)
  }
  return(output)
}

loopcountries(Newhouse77) %>%
  bind_rows(., broom::tidy(M1) %>% mutate(omitted = "FULL MODEL")) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  # Let's get rid of the intercept here just to make this clearer
  filter(term != "(Intercept)") %>%
  ggplot(.,aes(as.factor(omitted), estimate, ymin=lwr, ymax=upr)) +
  facet_wrap(~term) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip()

# Seems like the U.S. is more the influencer than Sweden or Greece, not that it materially changes much.
# Let's get back on track with some diagnostics of this new M1

plot(M1, which=1) # residuals vs. fitted mostly flat, but you're asking a lot to get variation from 13 units.
plot(M1, which=2) # likewise, normal QQ has a jump in the middle there. Not too dmaning, but, again, asking a lot of 13 units
plot(M1, which=3) # scale-location plot has some weirdness happening. Again: 13 units
plot(M1, which=5) # oof, this really wants to call out the U.S. and Sweden as outliers, along with Norway. Well...

summary(lm(medexppc ~ gdppc, data=subset(Newhouse77, country != "United States" & country != "Sweden" & country != "Norway")))

# I mean, sure, effect shifts up with them, but the estimate that still emerges is with a std.error of the full model.

# Calculate elasticities from Table 3 and, goddamn, it's been a hot minute since I've seen regression elasticities.
tibble(gdppc = c(mean(Newhouse77$gdppc), 3416, 4000, 5000, 6000)) -> newdat # i.e. I smell an error since I know 3416 is not the actual mean.

# small little error. Basically: the first elasticity for each is correct but the mean is incorrectly labeled.
newdat %>%
  mutate(m1coef = M1$coefficients["gdppc"],
         m2coef = M2$coefficients["gdppc"],
         pred1 = predict(M1, newdat = .),
         pred2 = predict(M2, newdat = .),
         el1 = m1coef*(gdppc)/pred1,
         el2 = m2coef*(gdppc)/pred2)


# Alternative estimations from Table 4

summary(M3 <- lm(medsharegdp ~ I(1/gdppc), Newhouse77))
summary(M4 <- lm(medsharegdp ~ gdppc, Newhouse77))
summary(M5 <- lm(log(medsharegdp) ~ log(gdppc), Newhouse77))
summary(M6 <- lm(medsharegdp ~ I(1/gdppc), data=subset(Newhouse77, country != "Greece")))
