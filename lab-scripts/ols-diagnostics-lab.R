#' ---
#' title: "OLS Regression Diagnostics"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 25 February 2021
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

#' # Elsewhere on My Website
#' 
#' Some of what I write here is taken from my website. Please read these:
#' 
#' - [Bootstrap Your Standard Errors in R, the Tidy Way](http://svmiller.com/blog/2020/03/bootstrap-standard-errors-in-r/)
#' 
#' Be advised I might plagiarize myself here.
#' 
#' # R Packages/Data for This Session
#' 
#' We'll be using `{stevedata}` for some data and `{tidyverse}` for most things. I'm assuming you have yet to install `{modelr}`, `{fixest}`, and `{lmtest}`. We'll
#' need those for this session.

packs <- c("lmtest", "fixest", "modelr")
new_pack <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new_pack)) install.packages(new_pack)

#' Run that, and then let's load them.

library(stevedata)
library(tidyverse)
library(lmtest)
library(fixest)
library(modelr)
options(warn=-1)


#' We're going to tailor this lab script to what you should do when you violate one of a few core assumptions of OLS. We'll focus on four here: linearity, normality,
#'  independence of errors, and constant error variances. In each case, we'll talk about what the assumption means, what diagnostics you have at your disposal to see
#'  whether your data are violating the model assumptions, and what you can do about them.
#'
#' # Linearity
#' 
#' One of the assumptions of the OLS model is that the outcome *y* is a linear function of the explanatory variable(s). Models that violate the linearity assumption amount to a 
#' specification error of the kind described by Gujarati. In a situation like this, the linear model is not appropriate to describe the relationship between *y* and the
#' explanatory variable(s). This amounts to a regression model that will be biased.
#' 
#' First, let's do this with the data/application used by Meuleman et al. (2019). In their case, they want to regress trust in the police in Belgium on an individual 
#' respondent's age (in years), gender (dummy for women), education (in years of full-time education), income (four-categories), and an assessment of how successful
#' the police are at preventing criminality (on an 11-point scale). Here's the naive linear model.
#' 
summary(M1 <- lm(trstplc ~ agea + female + eduyrs +  hincfel + plcpvcr, data=ESSBE5,
                 na.action=na.exclude))

#' Pro tip: if you have NAs and you don't care to drop those observations because you're lazy and you want `lm()` to drop them for you, specify `na.action=na.exclude` in the
#' formula. This will allow for some more convenience with helper functions.
#' 
#' ## Checking the Linearity Assumption
#' 
#' The authors describe an F-test for lack of fit. Truthfully, I've never liked this test. The intuition is straightforward but I've never seen an application of it
#' that I liked in the R programming language. I think you can assess the linearity diagnostic graphically. You can do it with the fitted-residual plot.
#'

plot(M1, which=1)

#' Concerns about the somewhat discrete nature of the DV aside, I do see a slight, upward bow there for the higher fitted values.
#' 
#' I also really like the authors' suggestion to compare a LOESS smoother to the the linear line in a faceted scatter plot. Here's how you'd do that.

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

#' Here's how you should interpret a plot like this: how much does the LOESS smoother and the LM smoother differ?
#' My takeaway: not a whole lot. Be mindful the LOESS smoother will want to travel to the outlier. The cool thing about this approach is that
#' a scatter plot like this will point you to some odd observations that you may want to punt out. For example, this is [the second time I've seen a European
#' respondent say they have 40+ years of full-time education](http://svmiller.com/blog/2020/03/what-explains-british-attitudes-toward-immigration-a-pedagogical-example/).
#' Which, c'mon, fam.
#' 
#' ## Potential Fixes for Non-Linearity
#' 
#' If you see any type of "U" shape, this implies a non-linear effect of *x* on *y*. The solution here is some kind of polynomial. The analog here with fake/hypothetical
#' data would be something like `lm(y ~ x + I(x^2), data=Data)`. More complicated shapes imply the need for some higher-order polynomial.
#' My own hot #take here is you're kind of losing the plot a little bit with the higher-order polynomials. In your own way, you're fitting
#' the model to the data (which is fine even as political scientists don't like that practice much) and you're still putting yourself
#' in a position where you have to explain the results in a simple and coherent way.
#' 
#' Depending on your particular application, you may also want to identify the particular *x* variable that is offending a linear relationship and break into some kind of
#' category variable and treat it as a fixed effect. Here is a simple example (and one that could also be modeled by a polynomial). There is [a well-known "U" effect of age
#' on happiness](https://www.weforum.org/agenda/2017/08/youll-probably-have-a-midlife-happiness-crisis-heres-why). Age may not be a "treatment", but there 
#' are still a lot of studies that suggest the 20-year-olds are relatively happy people and the retirees are relatively happy. It's the people in their 30s and 40s who, like me,
#' are miserable, depressed bastards. If you look at that "shape of happiness" chart, maybe you can do something like create an age bracket for the 16-35 group, another for the 
#' 36-60 group, and another for the 61 and above group. Treat that as a fixed effect, possibly with the 36-60 people as the baseline.
#' 
#' There's a mixed effects model comment I want to add here, but that really is more of an advanced topic.
#' 
#' # Normality
#' 
#' OLS assumes the *errors* are normally distributed. This is often conflated with an assumption that the outcome variable is normally distributed. That's not quite what it is.
#' It does imply that the conditional distribution of the dependent variable is normal but that is not equivalent to assuming the marginal distribution of the dependent variable
#' is normal. At the end of the day, the assumption of normality is more about the errors than the dependent variable even as the assumption
#' about the former does strongly imply an assumption about the latter.
#' 
#' Violating the assumption of a normal distribution of the errors is not as severe as a violation of some of the other assumptions. The normality assumption is not necessary
#' for point estimates to be unbiased. In one prominent textbook on statistical methods, Gelman and Hill (2007, p. 46) say the normality assumption is not important at all 
#' because it has no strong implication for the regression line. I think this follows because Gelman and Hill (2007)---later Gelman, Hill, and Vehtari (2020)---are nowhere near
#' as interested in null hypothesis testing as your typical social scientist likely is. No matter, violating the assumption of a normal distribution of errors has some implication
#' for drawing a line that reasonably approximates individual data points (if not the line itself, per se). Thus, you may want to check it, certainly if you have a small data set.
#' 
#' ## Checking the Normality Assumption
#' 
#' There are any number of diagnostics here for testing whether the residuals are normally distributed. Basically: gank your model's residuals and check if they are consistent
#' with an assumption that they are drawn from a normal distribution. You can do this with a Shapiro-Wilk test or a Kolmogorov-Smirnov test.

shapiro.test(resid(M1))
ks.test(resid(M1), y=pnorm)

#' In both applications, the assumption of a normal distribution of errors is violated. One caveat here might be that normality tests are sensitive to deviations from normality
#' when sample sizes are large. They're also sensitive to really any kind of discreteness in the dependent variable and any kind of heaping at extremes under those conditions.
#' 
#' You could also use our ol' friend the Q-Q plot to show the same thing. 

plot(M1, which=2)

#' There's added information from the Q-Q plot since it's saying that the negative residuals are more extreme than expected. Alas, compare that plot with
#' an actual density plot of the residuals juxtaposed a normal density function with a mean of 0 and a standard deviation equal to the standard deviation of
#' the residuals.


sd_resid <- sd(as.numeric(resid(M1)), na.rm=T)


ESSBE5 %>%
  mutate(resid = resid(M1)) %>% 
  ggplot(.,aes(resid)) + 
  geom_density() +
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = sd_resid),
                linetype="dashed", size=1.5)

#' Sure, it's not normally distributed, but I also don't think it wrong to say the normality assumption is approximated well even in a case
#' where the DV has just 10 values.
#' 
#' ## Potential Fixes for Non-Normality of the Errors
#' 
#' This one is your call. In most applications where this is a severe issue, it's because you're trying to impose OLS on a binary DV or a five-item Likert. I will
#' yell at you and throw things at you for doing that, but not necessarily for reasons related to this violation of OLS. There are rules of thumb for when you can
#' take a technically ordinal/discrete DV and treat it as something you can model through OLS. In this case, I think this is fine and I agree with the authors doing it.
#' In the case of a five-item Likert or a binary DV, it's not fine and there's better/more information to be gathered from a more appropriate model.
#' 
#' Further, it's tough not to divorce the normality assumption from the homoskedasticity assumption even if they are separate. Both travel together in my applications even
#' as they are clearly separate assumptions of OLS. Do you have outliers in your dependent variable? If so, consider some kind of transformation. Do you have cornball outliers in
#' your independent variables (e.g. that one Belgian who said s/he had 40+ years of full-time education)? If so, consider some kind of transformation or even punting that observation
#' out the model.
#' 
#' # Independence (i.e. No Autocorrelation)
#' 
#' This one is a biggie. OLS assumes that observations are sampled independently and that any pair of errors are unrelated to each other. In formal terms, this
#' assumption seems kind of Greek to students. In practice, think of it this way. A lot of the theoretical intuition behind OLS is assuming something akin to a simple random
#' sample of the population. You probably learned central limit theorem by this point and 
#' [how you can simulate that](http://svmiller.com/blog/2020/03/normal-distribution-central-limit-theorem-inference/). However, you may have a data set that does not 
#' look like this.
#' 
#' In other words, this assumption is going to be violated like mad in any design that has a time-series, spatial, or multilevel component.
#' For students new to this stuff, you should be able to anticipate ahead of time when your errors are no longer independent of
#' each other.
#' 
#' The implications of non-independent errors---aka "serial correlation" or "autocorrelation"---are 
#' generally more about the variance of the estimator than the estimator itself. That said, there is reason to believe
#' the standard errors are wrong and this can have important implications for statistical inference you'd like to do.
#' 
#' ## Checking the Independence (i.e. No Autocorrelation) Assumption
#' 
#' Honestly? Once you know under what conditions that autocorrelation is going to happen, you can probably skip this 
#' and go right to an appropriate model designed to deal with the nature of your data problem. The most common tests
#' are the Breusch-Godfrey test and the Durbin-Watson test. Of the two, practitioners I've read seem to favor the former over
#' the latter. There is also an issue as to what kind of higher-order autocorrelation you're trying to detect. Both come in
#' the `{lmtest}` package.

bgtest(M1)
dwtest(M1)

#' In our case, since the Belgium data are a simple sample of a population, there doesn't seem to be any concern
#' for serial correlation. The *p*-values above a particular threshold (i.e. .05) mean the model is consistent with
#' a null hypothesis of no autocorrelation.
#' 
#' If you're curious what it would look like to flunk one of these tests, let's use some fake data I created for
#' precisely this purpose.

fakeTSD
?fakeTSD

#' These data were created to mimic a time-series type phenomenon where the outcome *y* is a 
#' linear function of `20 + (.25*year) + .25*x1 + 1*x2 + e`. `x1` is a continuous variable and `x2` is a dummy variable. 
#' Of note: year determines the data-generating process, but "year" isn't really a variable of interest. I.e. it's a time-series.
#' 
#' Here is a plot of *y* by increasing year.


fakeTSD %>%
  ggplot(.,aes(year, y)) + geom_line() +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10))

#' Looks like a time-series, because it is a time-series.
#' 
#' Let's see if we can go-go-gadget this through OLS.

summary(M2 <- lm(y ~ x1 + x2, data=fakeTSD))

#' Nooope. Let's get our L from the Breusch-Godfrey test and the Durbin-Watson test.

bgtest(M2)
dwtest(M2)

#' Oh yeah, we earned that L.
#' 
#' ## Potential Fixes for Autocorrelation
#' 
#' You need a new model. In our fake time-series data, you can still reasonably approximate the true effects
#' by including a lagged dependent variable as a predictor or by calculating `y - lag(y)` and regressing
#' that difference on `x1` and `x2`. No matter, the diagnostics for autocorrelation will still complain and 
#' practitioners are generally not sanguine about lagged DVs for a variety of reasons.
#' 
#' To reiterate the point: you need a new model. This class doesn't have the opportunity to teach about some advanced topics,
#' like time-series modeling, mixed effects models, and the like. No matter, you'll want a model that
#' explicitly accounts for what would be the source of autocorrelation. If I could teach an entire class about the cool shit
#' you can do with a mixed effects model, I would.
#' 
#' # Homoskedasticity (Constant Error Variances)
#' 
#' OLS assumes that the variance of the errors is the same regardless of the particular value of *x*. This is often called "homoskedasticity", mostly
#' in juxtaposition to the violation of this assumption: "heteroskedasticity." The change of prefix should be clear to what is implied here. Heteroskedasticity
#' often means the errors are typically a function of something else in the model.
#' 
#' The implications for violating the assumption of homoskedasticity are more nuisance than real problem. Unequal/non-constant variance does not affect the 
#' XB part of the regression model, but it does have implications for predicting individual data points. Here, the implication is similar to the implication
#' of violating the assumption of normally distributed errors. Indeed, those two typically travel together in my experiences.
#' 
#' ## Checking the Homoskedasticity Assumption
#' 
#' Our ol' pal, the fitted-residual plot, should be illustrative for most things. First, let's show what it looks like in an ideal case.

set.seed(8675309)
tibble(
  x = rnorm(1000, 5, 2),
  e = rnorm(1000, 0, 1),
  y = 1 + .25*x + e
) -> C # for constant

summary(M3 <- lm(y ~ x, data=C))

#' ^ The model is well identified.

C %>%
  mutate(resid = resid(M3),
         fitted = fitted(M3)) %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point()

#' ^ The fitted-residual plot looks fine.
#' 
#' Now, let's deliberately violate this assumption. Here, we'll go with the most obvious manifestation of heteroskedasticity: the "cone of shame." Notice
#' that the errors are an increasing function of the *x* variable.

tibble(
  x = rnorm(1000, 5, 2),
  e = rnorm(1000, 0, 1 + .25*x), # uh-oh...
  y = 1 + .25*x + e
) -> H # for heteroskedasticity

summary(M4 <- lm(y ~ x, data=H))

#' The model still looks well identified, but notice the standard errors went up.

H %>%
  mutate(resid = resid(M4),
         fitted = fitted(M4)) %>%
  ggplot(.,aes(fitted, resid)) +
  geom_point()

#' Yeah, I see the cone of shame here. This is a pretty tell-tale case of heteroskedasticity.
#' 
#' There is a formal test for this, though: the Breusch-Pagan test, which comes as `bptest()` in the `{lmtest}` package. The null hypothesis in the test
#' is homoskedasticity. If you can reject that (via low-enough *p*-value), you have evidence of heteroskedasticity. Observe:

bptest(M3)
bptest(M4)

#' For giggles, observe that these data sets also imply a non-normal distribution of the errors. In this case, the Shapiro-Wilk test didn't see a problem
#' but the Kolmogorov-Smirnov does. To be clear, they are different assumptions about OLS. But, in my experience, if I have one, I have the other too.

shapiro.test(resid(M3))
shapiro.test(resid(M4))
ks.test(resid(M3), y=pnorm)
ks.test(resid(M4), y=pnorm)


#' ## Potential Fixes for Heteroskedasticity
#' 
#' There is no shortage of fixes for heteroskedasticity. First, I want to bring in a new data set that I love using for this purpose because it's super illustrative
#' of what heteroskedasticity will look like in a simple case. `af_crime93` comes via Table 9.1 of the 3rd edition of Agresti and Finlay's 
#' *Statistical Methods for the Social Sciences*. The data come from the *Statistical Abstract of the United States* and most variables were measured in 1993.
#' 
#' I have this in the proto version of my `{stevedata}` package right now, but you can load it off the course website.

af_crime93 <- readRDS(url("http://post8000.svmiller.com/lab-scripts/af_crime93.rds"))
af_crime93

#' In this application, Agresti and Finlay try to model the state's violent crime rate (per 100,000 people in the population) as a function of 1) the percent of the state
#' with income below the poverty level (`poverty`), 2) the percent of families in the state headed by a single parent (`single`), 3) the percent of the population living
#' in metropolitan areas (`metro`), 4) the percentage of the state that is white (`white`), and 5) the percent of the state that graduated from high school (`highschool`).
#' 
#' Let's run a naive model and see what happens.
#' 

summary(M5 <- lm(violent ~ poverty + single + metro + white + highschool, data=af_crime93))

#' It looks like the poverty, single-parent family, and metro variables are statistically significant. The white variable looks like it's almost significant at the .10 level too.
#' 
#' Cool, let's do the Breusch-Pagan test.

bptest(M5)

#' ...and we flunked it. All right, let's take some inventory as to what happened here with the fitted-residual plot.

af_crime93 %>%
  mutate(fitted = fitted(M5),
         resid = resid(M5)) %>%
  ggplot(.,aes(fitted, resid)) + geom_point()

#' Oh wow. Fitted-residual plots shouldn't look like this. The post on my website offers a bit more information as to the offending cases, but let's go identify what seem
#' to be the biggest offenders here.


af_crime93 %>%
  mutate(resid = resid(M5)) %>%
  filter(abs(resid) >= 300)

#' So, what can we do here? Well, for one, I'm a little convinced just from eyeballing these data that DC is a *weird* state (i.e. not technically a state, but should be a state).
#' In a lot of ways, DC looks nothing like the other 50 because of the nature of its territorial endowment and population. It's a city-state.
#' 
#' First, let me verify that intuition.


af_crime93 %>%
  select(-murder) %>%
  gather(var, val, 3:ncol(.)) %>%
  ggplot(.,aes(val, violent)) +
  geom_point() + facet_wrap(~var, scale="free_x")

af_crime93 %>%
  filter(state == "DC")

#' Yeah, that's DC.
#' 
#' One thing I can do here is just punt out DC and see what happens.

summary(M6 <- lm(violent ~ poverty + single + metro + white + highschool, data=subset(af_crime93, state != "DC")))

#' One takeaway I'm seeing already (and wrote about on my blog a bit). The discernibility of that `white` coefficient is almost entirely a function of DC.
#' DC is a majority non-white (should-be-a) state and has a high violent crime rate in these data. There really is no discernible effect of the 
#' `white` coefficient without it.
#' 
#' But, let's see what the Breusch-Pagan test says.

bptest(M6)

#' ...and we passed. DC alone will flunk a Breusch-Pagan test in this application. If you have a simple case like this where it's just one observation that is messing with an
#' overall picture, you can punt it out.
#' 
#' The textbook solution is a weighted least squares (WLS) estimation. The process here is a little convoluted and there is a simple wrapper for it 
#' somewhere (I'm sure), but here's what the process looks like. 1) Run the model (which we already did). 2) Gank the residuals and fitted values. 3) 
#' Regress the absolute value of the residuals on the fitted values of the original model. 4) Extract *those* fitted values. 5) Square them and 6)
#' divide 1 over those values. 7) Finally, apply those as weights in the linear model once more for a re-estimation.


af_crime93 %>%
  mutate(wts =  1/fitted(lm(abs(residuals(M5))~fitted(M5)) )^2) -> af_crime93


summary(M7 <- lm(violent ~ poverty + single + metro + white + highschool, data=af_crime93, weights=wts))

#' We can compare the unweighted OLS with the WLS estimates.

broom::tidy(M5) %>% mutate(cat = "OLS") %>%
  bind_rows(., broom::tidy(M7) %>% mutate(cat = "WLS")) %>%
  arrange(term)

#' You could also do some on-the-fly heteroskedasticity corrections to your standard errors. There are a *lot* to choose from here, and some are more tedious than others.
#' I'll use the `{fixest}` package here because it's the most convenient. You may also want to look into the `{sandwich}` package as well.

summary(M8 <- feols(violent ~ poverty + single + metro + white + highschool, data=af_crime93, se = "hetero"))

#' Let's compare these heteroskedasticity corrected standard errors with the naive OLS standard errors.

broom::tidy(M5) %>% mutate(cat = "OLS") %>%
  bind_rows(., broom::tidy(M8) %>% mutate(cat = "HCSE")) %>%
  arrange(term)

#' The biggest inferential implications are for the `white` coefficient, but do note the *t*-statistics for `single` and `metro` dropped considerably as well.
#' 
#' You could also go back to some basics and acknowledge that outliers (i.e. DC) and right-skewed data are responsible for flunking the assumption of homoskedastic
#' error variances. We could transform the data to impose some kind of normality here. First, let's check if we have any 0s here.

af_crime93 %>% summary

#' We don't. So, let's log-transform the DV and regress that on the *x* variables.

summary(M9 <- lm(log(violent) ~ poverty + single + metro + white + highschool, data=af_crime93))
bptest(M9) 

#' And we passed. Barely at the .10 level, but I'll count that. Here, log-transforming the DV helped considerably.
#' The log-transformed DV does have the effect of suggesting there is no poverty effect.
#' 
#' Finally, we can do what I think is one of the greatest parlor tricks in statistics: bootstrapping. I'm going to have to punt on the theory and exact principles
#' of bootstrapping to classic texts, but the intuition here is, in the absence of a true "population", to treat the sample as a population of sorts and sample, *with replacement*
#' from that. There's *a lot* more to say about this that I don't have the space here. If you have some complicated data structures, you'll need to adjust your bootstrapping
#' to account for that.
#' 
#' The `{modelr}` package will make the bootstrapping simpler for us. First, let's sample, with replacement, from the `af_crime93` data 1,000 times. The data we have
#' are small, so we can't get too crazy. Efron I think recommends 250-500, but 1,000 is a nice number that is often a nice target in the sampling framework.

set.seed(8675309) # Jenny, I got your number...

af_crime93 %>%
  bootstrap(1000) -> bootCrime

#' Notice we have 1,000 resamples of the original data.
bootCrime


#' Here's the first one. Interestingly, Colorado appears in this *five* times. That seems like it's oversampling Colorado, which it would be if this were our only
#' resample. However, there will be cases where Colorado doesn't appear at all. That's the idea here.

bootCrime %>% 
  slice(1) %>% # grab the first row in this special tbl
  pull(strap) %>% # focus on the resample, momentarily a full list
  as.data.frame() %>% # cough up the full data
  as_tibble() %>%
  arrange(state) %>% # arrange by state, for ease of reading
  select(-murder, -wts)

#' Now, let's use `{purrr}` magic and run our regression on each of these resamples. Then, we're going to tidy them up, pull the results, and create a nice tidy data frame of those.

bootCrime %>% 
  mutate(lm = map(strap, ~lm(violent ~ poverty + single + metro + white + highschool, 
                             data = .)),
         tidy = map(lm, broom::tidy)) %>%
  pull(tidy) %>%
  map2_df(., # map to return a data frame
          seq(1, 1000), # make sure to get this seq right. We did this 1000 times.
          ~mutate(.x, resample = .y)) -> tidybootCrime

#' Now, in a case like this, your bootstrapped regression parameter (i.e. coefficient or intercept) is the mean of the estimate and your bootstrapped standard error is the
#' *standard deviation of the estimate*. That part may not be obvious. It's not the mean of the standard errors, it's the standard deviation of the coefficient.

tidybootCrime %>%
  group_by(term) %>%
  summarize(boot_est = mean(estimate),
            boot_se = sd(estimate)) -> bootsums

bootsums

#' You can compare the results of this bootstrapped summary to your "main" model. FWIW, you can bootstrap the coefficient as well but most practical interest is in what bootstrapping
#' does in comparison to the standard error of your OLS model. So, it might make more sense to just compare the standard errors.

broom::tidy(M5) %>%
  mutate(cat = "OLS") %>%
  bind_rows(., bootsums %>% rename(estimate = boot_est, `std.error` = boot_se) %>% mutate(cat = "Bootstrapped")) %>%
  arrange(term) %>%
  mutate(statistic = ifelse(is.na(statistic), estimate/std.error, statistic)) %>%
  select(-p.value)


#' In this case, bootstrapping leads to much more diffuse standard errors for the `white` variable and, to a lesser extent, the single-parent household variable. The latter
#' is not affected from a null hypothesis test standpoint.
#' 
#' The superlative of this approach is that it's super-flexible and you can do a lot of cool things. Downsides: it can be a bit time-consuming with larger data sets. Data sets that
#' would violate the independence of errors would require additional considerations.
#' 
#' No matter, nothing here is necessarily a "fix." When you have heteroskedasticity, refit the model a few ways and see what happens to your standard errors. Be prepared
#' to report this in an appendix.
