#' ---
#' title: "OLS Regression"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 18 February 2021
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
#' 
#' # R Packages/Data for This Session
#' 
#' We'll be using `{tidyverse}` for all things workflow and `{stevedata}` for toy data. You may also want to install `{stevemisc}` from Github,
#' though we'll load that `cor2data()` function here so you don't have to do this.

library(tidyverse)
library(stevedata)


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

#' # OLS with Fake Data
#' 
#' Let's create a few data sets from the `cor2data()` function. This should look very familiar to what we did the previous week with measurement error.
#' We'll keep it simple here and have just the one predictor (`x1`) uncontaminated by any endogeneity concerns.

vars = c("x1",  "e")
Cor <- matrix(cbind(1, 0.001,
                    0.001, 1), nrow=2)
rownames(Cor) <- colnames(Cor) <- vars

A <- cor2data(Cor, 1000, 8675309) %>% tbl_df() # Jenny I got your number...

#' We'll set up a simple situation where the outcome `y` is a function of this formula. The expected value of `y` is 1 when `x1` is 0. The errors are kind of "skinny" too,
#' at least relative to the effect of `x1`.
 
A$y <- with(A, 1 + x1 + .5*e)

#' Okay then, let's do a regression to show what we just did.

summary(M1 <- lm(y ~ x1, data=A))

#' ^ Nailed it.
#' 
#' Btw, if you just wanted to do a simple lm with one y on one x, ggplot could do this for you.

A %>%
  ggplot(.,aes(x1,y)) +
  geom_point() +
  geom_smooth(method ="lm")

#' One of the properties of the SRF with one `x` is the regression line will travel through the mean of y and x. Observe:
a_meany = mean(A$y)
a_meanx1 = mean(A$x1)

A %>%
  ggplot(.,aes(x1,y)) +
  geom_point() +
  geom_smooth(method ="lm") +
  geom_vline(xintercept = a_meanx1) +
  geom_hline(yintercept = a_meany) 

#' One of the assumptions is the mean of residuals will be zero and `x` is uncorrelated with the residuals.
A %>%
  mutate(resid = resid(M1)) %>%
  summarize(corx1 = cor(resid, x1),
            meanresid = mean(resid))


#' ## The Basic Diagnostic Checks for Beginners.
#' 
#' Always, always, always do some diagnostics on your OLS model.  This will, in practice, mean looking at your residuals and extracting your fitted variables.
#' Let's do that:

A %>%
  mutate(resid = resid(M1),
         fitted = fitted(M1)) -> A

#' 1) Compare the fitted versus the residual. Fitted vs. residual plots should more or less look random. The line through it should be flat/straight through 0 on the y-intercept

A %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept = 0, linetype="dashed")

#' 2) The second plot is a normal Q-Q plot of the residuals. If the the line is straight, it suggests the errors are normally distributed

A %>%
  ggplot(.,aes(sample=resid)) + geom_qq()

#' 3) The third plot is a scale-location plot. Basically: plot the fitted values against the square root of the absolute values of the residuals. This too should effectively be random

A %>% 
  mutate(absqresid = sqrt(abs(resid))) %>%
  ggplot(.,aes(fitted, absqresid)) +
  geom_point() + geom_smooth(method = "lm")

#' 4) The fourth plot is a residuals vs. leverage plot. This will help you find any influential cases in your data. 
#' These residuals vs. leverage plots are typically communicated with standardized residuals
#' 
#' Some definitions:
#' 
#'  - outlier: an observation with a large residual
#'  - leverage point: an observation with an x value far from the mean.
#'  - influence observation: an observation that changes the slope of the line
#'  
#' One way around this is to compare the regression line with and without those observations.

A %>%
  mutate(stdresid = rstandard(M1), # standardized residuals, via handy built-in function
         lev = hatvalues(M1)) %>% # leverages. Trust me, just do it this way. matrix algebra is a pain.
  ggplot(.,aes(lev, stdresid)) +
  geom_point() +
  geom_smooth(method ="loess")

#' One thing happening underneath the proverbial hood of these diagnostic plots is a Cook's distance score.
#' Cook's distance is a measure used to calculate influence in OLS. Higher values of Cook's D = more influence.
#' So what defines "high" and "influencer?" Some studies recommend a threshold of 4/n, or 4/(n-k-1).
#' Alternatively: check if it's above 1 if your data are sufficiently large.
#' Values above that are problematic. Even then, so many rules of thumb.
#' My hot #take: this is a "use your head" problem, following a graph of Cook's D scores.
#' And you'll need to eyeball this, as I've already done:

tibble(obs = 1:1000,
       cooksd = cooks.distance(M1)) %>%
  bind_cols(A, .) -> A

A %>%
  ggplot(.,aes(obs, cooksd)) + geom_bar(stat="identity") +
  geom_hline(yintercept = .01, linetype="dashed") +
  geom_text(data=subset(A, cooksd>.01), aes(y=cooksd, label=obs), size=4)

#' Traditional logic (and this is still valid): if you have problematic observations/influencers, punt them out, re-run the model, and compare the results.
#' My hot #take: if you do that, the Cook's D scores for the new regression will change as well. Cook's D is kind of relative.
#' Basically, take a look, find influencers/outliers, but don't try to wed yourself to Cook's D as a diagnostic.

#' Base R has a lot of plots that could do this manually for you.
#' It won't be as pretty, and you shouldn't present them "as is", but they're a little convenient.
#' FYI: I hate built-in functions that make me hit return to see the next plot.
#' It's why I did this the ggplot way.

plot(M1)

#' This will return the four biggies that I described above, but you could grab an individual one, if you'd like. See:

plot(M1, which=1)

#' Want the Cook's D histogram? That's no. 4.

plot(M1, which=4)

#' There is one other too:
plot(M1, which=6)
#' It plots the leverage points against Cook's D. Never have been a big fan of this plot.
#' 
#' # A Replication of Newhouse (1977)
#' 
#' Let's replicate Newhouse (1977) now. I like this regression for this purpose not because it's good, but because you won't find many political science articles
#' where the full damn data for the regression is in the article itself. That's pretty cool and the topic is still accessible even if the analysis is clearly dated.
#' 
#' The data are in `{stevedata}` as follows.

Newhouse77
?Newhouse77 # for background info

#' Okie dokie. Let's reproduce Table 2 now. Here, Newhouse is regressing medical care expenditures per capita as a function of GDP per capita.
#' In his analysis, he does go-go-gadget OLS on the full data but then does another analysis that excludes Greece. In his analysis, Greece is apparently
#' an outlier than can monkey with inferences.

summary(M1 <- lm(medexppc ~ gdppc , data=Newhouse77))
summary(M2 <- lm(medexppc ~ gdppc, data=subset(Newhouse77, country !="Greece")))

#' Basic takeaway maps to what Newhouse reports. Higher GDP per capita > higher per capita medical care expenditures. This holds with or without Greece.
#' 
#' Question, for my own curiosity: is Greece that much an outlier? It doesn't really look like it. It's an outlier in that it scores low on both the DV
#' and the IV and not necessarily that it's removed from what would be expected, given the data. If Greece is a problem observation, shouldn't the U.S. be
#' a problem observation as well? Greece scores as low as the U.S. scores high on both the IV and DV. Let's plot the data (which you should always do for simple cases like this).
#' Greece is pretty easy to identify here. But, again, it's very much in line with what would be expected.

Newhouse77 %>% 
  ggplot(.,aes(gdppc, medexppc)) + 
  geom_point() + geom_smooth(method = "lm")

#' So, to answer this question, what would the Cook's D say?

Newhouse77 %>%
  mutate(cooksd = cooks.distance(M1))

#' I guess Greece is kind of an outlier, but the biggest influencers here seem to be Sweden and the United States (with Norway picking up bronze, if you will). 
#' Let's kick them out, see what happens.

summary(lm(medexppc ~ gdppc, data=subset(Newhouse77, country != "Norway")))
summary(lm(medexppc ~ gdppc, data=subset(Newhouse77, country != "United States")))
summary(lm(medexppc ~ gdppc, data=subset(Newhouse77, country != "Sweden")))

#' Nothing materially seems to change. So, how about this to assess the model fit. We'll do what's called a "leave one out" approach here, or at least we'll do it informally.
#' We'll write a simple script that just punts one observation out, repeat the regression, and we'll present what happens to the effect of GDP per capita on per capita
#' medical expenditures when that observation is removed.

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

#' Seems like the U.S. is more the influencer than Sweden or Greece, not that it materially changes much.
#' Let's get back on track with some diagnostics of this new M1 (from above). This is the full data, including Greece.
#' 
#' First, a residuals-fitted plot.
plot(M1, which=1) 

#' This is mostly flat, but you're asking a lot to get variation from 13 units. Oddly enough, the plot is complaining about Norway (makes sense), Sweden (makes sense),
#' and the United Kingdom (which is odd, given the Cook's D).
#' 
#' Here' a Q-Q plot.

plot(M1, which=2) 

#' The Q-Q plot has a jump in the middle there. Not too damning, but, again, asking a lot of 13 units.
#' 
#' Scale-location plot time:

plot(M1, which=3) 

#' This scale-location plot has some weirdness happening. Again: 13 units.
#' 
#' Finally, the residuals-leverage plot.
plot(M1, which=5) 

#' oof, this really wants to call out the U.S. and Sweden as outliers, along with Norway. Well, fine. Let's remove them and see what happens.

summary(lm(medexppc ~ gdppc, data=subset(Newhouse77, country != "United States" & country != "Sweden" & country != "Norway")))

#' I mean, sure, effect shifts up with them removed, but the estimate that still emerges is with a std.error of the full model.
#' 
#' 
#' Finally, let's calculate elasticities from Table 3 and, goddamn, it's been a hot minute since I've seen regression elasticities. I don't think
#' you'll see this much in your travels. Alas, let's do it for max reproducibility.

tibble(gdppc = c(mean(Newhouse77$gdppc), 3416, 4000, 5000, 6000)) -> newdat # i.e. I smell an error since I know 3416 is not the actual mean.

# small little error. Basically: the first elasticity for each is correct but the mean is incorrectly labeled.
newdat %>%
  mutate(m1coef = M1$coefficients["gdppc"],
         m2coef = M2$coefficients["gdppc"],
         pred1 = predict(M1, newdat = .),
         pred2 = predict(M2, newdat = .),
         el1 = m1coef*(gdppc)/pred1,
         el2 = m2coef*(gdppc)/pred2)


#' Finally, here are the alternative estimations from Table 4. Of note: the I() wrapper in a regression is great for embedding on-the-fly transformations of the data.
#' You don't need it for on-the-fly log transformations. I recommend just creating additional columns.

summary(M3 <- lm(medsharegdp ~ I(1/gdppc), Newhouse77))
summary(M4 <- lm(medsharegdp ~ gdppc, Newhouse77))
summary(M5 <- lm(log(medsharegdp) ~ log(gdppc), Newhouse77))
summary(M6 <- lm(medsharegdp ~ I(1/gdppc), data=subset(Newhouse77, country != "Greece")))
