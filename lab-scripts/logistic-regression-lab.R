#' ---
#' title: "Logistic Regression"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 25 March 2021
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
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

#' 
#' # Elsewhere on My Website
#' 
#' Some of what I write here is taken from my website. Please read these:
#' 
#' - [Age, Income, Partisanship, Racial Attitudes and the Trump Vote in 2016](http://svmiller.com/blog/2017/04/age-income-racism-partisanship-trump-vote-2016/)
#' 
#' Be advised I might plagiarize myself here.
#' 
#' # R Packages/Data for This Session
#' 
#' Packages will be fairly minimal for this lab session. `{tidyverse}` will be doing most things. `{stevedata}` 
#' has our data. `{lmtest}` has some diagnostics for us. Let's load those now. The `{knitr}`
#' chunk above will make the code output less verbose.

library(tidyverse)
library(stevedata)
library(lmtest)

#' `TV16` comes from the 2016 Cooperative Congressional Election Study (CCES) data. It allows us to model
#' the individual-level correlates of the self-reported Trump vote in 2016 using data from all 50 states,
#' stratified on state population and other demographics. Importantly, there are more Californians than
#' South Carolinians so data of this sort should (and here: does) reflect that.

?TV16
TV16

#' We'll start simple. Let's start by looking at just white voters in what was a crucial swing state in that election: Pennsylvania.

TV16 %>%
  filter(state == "Pennsylvania" & racef == "White") -> Penn

#' Let's try a simple linear model for this. Herein, let's estimate a white person's vote for Trump (i.e. 1 = they did, 0 = they did not) 
#' as a function of age, gender, education (if they have a college diploma), household income, partisanship (D to R),
#' ideology (L to R), and whether they're a born-again Christian. Let's get a summary too.

M1 <- lm(votetrump ~ age + female + collegeed + famincr + 
           pid7na + ideo + bornagain,
         data = Penn, na.action=na.exclude)

summary(M1)

#' Your interpretation of this summary model will suggest that there are "statistically significant" effects
#' of age (+), college education (-), D to R partisanship (+, which, no shit), and L to C ideology (+, which, again,
#' no shit).
#' 
#' What you (well: I) did here is euphemistically called the "linear probability model." Its treatment often comes
#' wrapped in lots of exposition and justification but the model itself reduces to the imposition of an ordinary
#' least squares (OLS) model that regresses a binary dependent variable on a set of covariates. The interpretation
#' here is that the regression coefficients can be interpreted as the change in the probability that `y` = 1 holding constant
#' the other regressors. Thus, looking at the ideology coefficient, a one-unit change in the ideology variable increases
#' the probability of an individual-level Trump vote among these white Pennsylvanians by about .102.
#' 
#' You might have been able to gather from the lecture by this point that I hate this intepretation for reasons I hope to
#' elaborate throughout this lab script. No matter: remember that we can assess the veracity of the OLS model
#' through [various diagnostic procedures](http://post8000.svmiller.com/lab-scripts/ols-diagnostics-lab.html).
#' 
#' Let's check the linearity assumption.

plot(M1, which=1)

#' Okay, fitted-residual plots should not look like this. We're already off to a not-good start.
#' 
#' Let's test for potential auto-correlation.
#' 
bgtest(M1)
dwtest(M1)

#' We're golden here, pony boy. We did select on just white people in Pennyslvania. The moment we start lumping 
#' in people from other states or people from other races is when we're probably going to get some important
#' spatial heterogeneity here. No matter, we've satisfied the independence assumption with this small, focused
#' sample.
#' 
#' Okay, let's check for heteroskedasticity with the Breusch-Pagan test.

bptest(M1)

#' Yeah, no shit you have heteroskedastic errors. You only have two possible responses! Under these conditions,
#' you should expect that your errors are no longer normally distributed. The Shapiro-Wilk test, the 
#' Kolmogorov-Smirnov test, and the Q-Q plot will all tell you this.

shapiro.test(resid(M1))
ks.test(resid(M1), y=pnorm)
plot(M1, which=2)

#' To be clear, the normality assumption is about the *errors* and not the dependent variable, even as the normality
#' assumption of the errors does kidn of imply that the conditional distribution of the dependent variable is normal.
#' 
#' No matter, I want you to take stock of what happened here: the fitted-residual plot is a mess, the errors are
#' not normally distributed, and the errors are heteroskedastic. That's on you (well: me) for running an OLS
#' model on a binary dependent variable and asking it to make sense for me.
#' 
#' An econometrician may object that this is still fine. After all, the normality assumption is a strong one
#' and the implication for violating it is not as severe as the other ones. Further, heteroskedasticity
#' is more about the variance of the estimator and not the estimate itself. Further, the logistic curve is 
#' kind of "normal"/linear in the middle. Under those conditions, you can still think of the coefficients
#' as communicating changes in probability.
#' 
#' My retort to that is that it's not clear to me you necessarily should when the fitted values from the model
#' will break the bounds of probability. Observe:
#' 
Penn %>%
  mutate(yhat = predict(M1)) %>%
  ggplot(.,aes(yhat, votetrump)) +
  geom_point(alpha=0.2, color="black") +
  geom_vline(xintercept = 1, linetype="dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text", label="Impossible\npredictions\n(yhat > 1)",
           x = 1.05, y=.5, hjust="left") + 
  annotate("text", label="Impossible\npredictions\n(yhat < 0)",
           x = -.05, y=.5, hjust="right")

#' I'm willing to be flexible and open to interpretation about linear models on things like four-item DVs (definitely don't), 
#' five-item Likerts (enh, also don't do that...) or seven-item ordinal variables (I'm listening...),  or ordinal items on a 
#' larger scale like 10 points (about where I'd be). Under those circumstances, check your `y`-hats.
#' 
#' Anything outside the bounds of the scale should caution you against using OLS with the nature of the dependent
#' variable you have. When all `y`-hats are within that scale, you might spin a ball of yarn to me about 
#' how the discrete nature of the DV are our only measurements of some underlying continuum. 
#' I'm willing to hear those out, under those circumstances. However, the answer, for me, is *always* NO when 
#' it comes to linear models on binary DVs. Instead, use a model designed for the distribution of the dependent
#' variable. You can use a probit model, though I prefer the logistic regression and will discuss it here.
#' 
#' You can fit a logistic regression with the `glm()` function. Specify the family you want, which will be
#' `binomial(link="logit")`.

M2 <- glm(votetrump ~ age + female + collegeed + famincr + 
            pid7na + ideo + bornagain,
         data = Penn, na.action=na.exclude,
         family = binomial(link="logit"))

summary(M2)

#' Let's compare the two models. We'll suppress the intercepts to focus on the important stuff.

broom::tidy(M1) %>%
  mutate(model = "LPM") %>%
  bind_rows(., broom::tidy(M2) %>% mutate(model = "Logit")) %>% 
  arrange(term) %>% filter(term != "(Intercept)")

#' Just because almost everything that was significant in `M1` is significant in `M2` is beside the point. That
#' also glosses over an interesting change in the discernibility of the `bornagain` variable against a counterclaim
#' of zero effect. Peeking ahead, the "divide by 4" rule for logistic
#' regression suggests the linear probability model may be understating the actual effect of some variables like 
#' ideology and partisanship. Ultimately: hunting for statistical significance is no justification for taking the 
#' linear model over one better suited for modeling a binary dependent variable.
#' 
#' # Understanding the Logistic Regression
#' 
#' I think the biggest hangup for beginners and some practitioners is that OLS communicates what you want to know
#' in a more straightforward way: a coefficient communicates an estimated change in the value of `y` for a one-unit
#' change in `x`. However, the logistic regression returns coefficients as something called "logits" (i.e. natural
#' logged odds). How might you get more usable information from this model?
#' 
#' I'll save the "divide by 4" rule for second in this list, but I want to unpack the logistic regression model on its own 
#' terms here. Let's start with the first thing you can do.
#'
#' ## Inverse the Logit to Return a Probability
#' 
#' The intuition here is simple: you have a logit. You want a probability. Inverse the logit as an odds ratio to
#' get that probability you want. My only misgiving with this is that it's not so simple as monkeying with the coefficient.
#' You'll need the intercept as well, along with anything else in your model. Thus, you should *resist the urge to 
#' do this manually with more complicated models that have multiple covariates.* You will make errors. I'll show you
#' how to do this in the "making most of regression" lecture.
#' 
#' Nevertheless, let's do a simple exercise here with just one covariate: the college education variable. FWIW,
#' I selected just this one covariate because it too is binary so the calculation is simple. Further, the coefficient
#' is not too far removed from what it is in the fully specified model.
#' 

M3 <- glm(votetrump ~ collegeed, data=Penn, na.action = na.exclude,
          family = binomial(link="logit"))
summary(M3)

#' Let's tidy up our model now, extracting the information we want.

broom::tidy(M3) -> tidyM3
interceptM3 <- tidyM3[1, 2] %>% pull()
coefM3 <- tidyM3[2, 2] %>% pull()

#' ^ Btw, this is why I'm telling you to resist the urge to do this manually for full models, ESPECIALLY when you you don't have 
#' naturally occurring zeroes in most of the parameters.
#' 
#' Now, let's calculate it manually. What is the estimated difference in probability of voting for Trump for a white Pennsylvanian
#' with a college education versus a white Pennsylvanian without a college education? The `plogis()` function comes to the rescue here.

plogis((interceptM3 + coefM3)) - plogis(interceptM3)

#' Interpretation: having a college education decreases the probability of voting for Trump among
#' Pennsylvania whites by about 20 percentage points. Some caveats:
#' 
#' - Again, don't do this manually for full models. Get some quantities of interest. We'll talk about this in a few weeks.
#' - You *can't* just isolate the coefficients to do this. Gotta include the intercept. 
#' i.e. p(y = 1 | x = 1) - p(y = 1 | x = 0) must include the intercept. See:

plogis(coefM3)

#' Consider this a tentative estimate too. Truly, esp. for GLMs, quantities of interest 
#' are what you need to more fully communicate the effects you want. We'll get to that.
#' 
#' ## The "Divide by 4" Rule
#' 
#' I love parlor tricks in statistics and I think this is a fantastic one. To the best of my knowledge, we have
#' Gelman and Hill (2006, 82) to thank for this. Understanding it requires know a little bit about the logistic curve.
#' You've seen this before; you just might have called it the "S" curve.

scurve <- function(x){
  y <- exp(x) / (1 + exp(x))
  return(y)
}

tibble(x = seq(-6, 6)) %>%
  ggplot(.,aes(x)) +
  stat_function(fun = scurve) 

#' Observe that the logistic curve is steepest in the middle/center. That means the slope of the curve (i.e. the
#' derivative of the logistic function) is maximized at the point where ` Be^0/(1 + e^0)^2`. That, incidentally,
#' resolves to `B/4`. Thus, `B/4` (or: your regression coefficient, divided by 4) is the estimated *maximum* difference
#' in `p(y = 1)` for a one-unit change in `x`. It's an upper bound of the predictive difference, albeit one inapplicable
#' to the constant.
#' 
#' For what it's worth, I think this an amazing rule of thumb.

plogis((interceptM3 + coefM3)) - plogis(interceptM3)
coefM3/4

#' It's not perfect, but it's really good. We can apply this to the full model (`M2`) as well.

broom::tidy(M2) %>%
  select(1:3) %>%
  mutate(db4 = estimate/4) %>%
  slice(-1) # does not apply to constant/intercept

#' This is obviously not a place to stop in terms of communicating your logistic regression coefficients. However, I think it's a great place
#' to start. Truthfully, when I evaluate my own logistic regression models, this is typically the second thing I'm looking at (after evaluating 
#' statistical significance).
#' 
#' 
#' ## Odds ratios
#'
#' Odds ratios may also be useful ways of summarizing a logistic regression.
#' I encourage you to get flexible with logits, but odds ratios have an advantage over probabilities because they can scale up infinitely.
#' Probabilities are left-bound and right-bound. Odds and odds ratios are only left-bound. Odds/odds ratios less than 1 are negative effects.
#'  
#'  FWIW, I wince every time I see logistic regression models communicated with odds ratios.
#'  Namely, if you're arguing for a negative effect, the odds ratio kind of obscures that because they're left-bound at 0.
#'  I prefer to communicate them on their original scale and encourage you to get comfortable with the same.
#'  
#'  Yet, here's how you could do it. Just exponentiate what the logistic regression model gives you.
#'  Let's also add confidence intervals for funsies.

broom::tidy(M2) %>%
  bind_cols(., as_tibble(confint(M2))) %>%
  mutate_at(vars(2:ncol(.)), exp)

#' # Model Checking for Logistic Regression
#' 
#' There are various tools for model checking the logistic regression. The application differs from OLS but the approaches should
#' seem familiar.
#' 
#' ## Binned Residual Plots
#' 
#' First, you can get a binned residual plot. With OLS, you compare the fitted values and residuals with a scatterplot. With
#' logistic regression, you get something similar, but recall the residuals are discete because the dependent variable
#' is discrete.
#' 
#' First, let's get the fitted values and residuals.

Penn %>%
  mutate(fitted = predict(M2, type="response"),
         resid = residuals(M2, type = "response")) -> Penn

#' Gelman and Hill have a rule of thumb for determining the size of the bins. For larger data sets (n >= 100, which we have here), 
#' the number of bins are determined as the (rounded down, i.e. "floored") square root of number of obs in the model.
#' 
nbins <- floor(sqrt(nobs(M2)))

#' Then, across these number of bins, we're going to look for the corresponding obs location in the fitted values.
breaks.index <- floor(length(M2$fitted.values) * (1:(nbins - 1)) / nbins)

#' The breaks that emerge will coincide with those fitted values
breaks <- unique(c(-Inf, sort(M2$fitted.values)[breaks.index], Inf))

#' In the interest of full disclosure: I need to think about how I want to make this more flexible. This is a Steve problem 
#' for future classes, but this will converge on what `arm::binnedplot()` will give you. There's also `parameters::binned_residuals()`,
#' though that has additional package requirements (i.e. `{parameters}`, `{performance}`, `{easystats}`, and `{see}`, among, I'm sure,
#' others). My goal here is to get you to install as few packages as possible, even if the code that emerges can be a bit convoluted.
#' 
#' Okay then, let's create some bins.

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


#' Let's check to see where we're out of bounds. I've always been told the ideal interpretation here is 
#' that you want 95% of your bins "in bounds." We won't have that here, it seems. Let's see where:
Bins %>%
  filter(inbounds == 0)

#' It does look like our model isn't a great fit at 1) the tail ends of the distribution where the probabilities are touching 0 or 1.
#' We have a few bins in the middle as well. FWIW, don't fret the outliers that close to the bounds of 0 and 1. Not ideal, but not terribly problematic.
#' 
#' You could importantly plot them as well. This will be about what `arm::binnedplot()` is doing.

Bins %>%
  ggplot(.,aes(meanfit, meanresid)) + geom_point() +
  geom_smooth() +
  geom_ribbon(aes(ymin = -Inf, ymax = -se), alpha = .1 , fill = "red") +
  geom_ribbon(aes(ymin = se, ymax = Inf), alpha = .1 , fill = "red") +
  geom_line(aes(y = se), colour = "grey70") +
  geom_line(aes(y = -se), colour = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed")

#' When you see a plot like this, check for a pattern. There really isn't much of one, though there does seem to be some
#' bowing in the middle. I don't see a huge problem here, though I do see a model that could use some further explanation
#' and consideration. Basically, if you're worried your model might not be the best fit given what you see from this
#' plot, consider the following points.
#' 
#' 1. You might have some hidden interactions you should fine. Think especially of those large residuals as illustrative of that.
#' 2. You may have some outliers, a non-linear pattern, or one of your covariates may have a skew. Under those conditions,
#' think of some kind of transformation (logarithmic, square root) or allowing an effect to be non-linear.
#' 
#' Basically: think more about your data here. I did not. You should.
#' 
#' ## Predictive Accuracy
#' 
#' You can evaluate a logistic regression model by checking for predctive accuracy. You will see this done a lot
#' in "machine learning" applications. Herein, the model is "checked" not for fit, per se, but for "validation."
#' 
#' First, notice how our fitted values are predicted probabilities of observing a 1? Let's create two summaries to see
#' how well our model is understanding the Trump vote. In the first case, `naive` is just a naive error rate that comes
#' from knowing the mode/median. We know there are more 1s than 0s in the data. Indeed, the mean is about .52 (which, 
#' for binary variables, communicates the proportion of 1s). If we just picked 1 for all respondents, knowing nothing else,
#' we'd be right about 52% of the time. We'd be wrong about 48% of the time. 
#' 
#' In the second case, `errorrate` is the proportion of cases where we predicted a Trump vote, 
#' given the model, but were wrong (or the times we predicted the Pennsylvania white wouldn't vote for Trump, but did).
#' Do note: our cutoff for making a guess from the model output here is .5. Machine learning people I know say you can (and
#' perhaps should) adjust that threshold to do validation of a model. Here, we'll just do .5.

Penn %>%
  select(votetrump, fitted) %>%
  summarize(naive = 1 - mean(votetrump, na.rm=T),
            errorrate = mean((fitted > .5 & votetrump == 0) | fitted <.5 & votetrump == 1, na.rm=T))

#' As you evaluate this kind of output, keep the following in mind. You should want your error rate to be smaller than .5, 
#' much smaller even. If your error rate were greater than .5, you could simply set all betas to 0 and get a better fitting model.
#' Here, I think we're doing pretty damn good, all things considered. We're correctly predicting about 85% of the cases.
#' 
#' ## Model Deviance
#' 
#' 
#' Recall: deviance is a measure of error and replaces R2 as a measure of model fit in a GLM like a logistic regression.
#' It's analogous to the residual standard deviation. Lower deviance is better: it means your fitted values better "fit" to the observed values.
#' Much like R-square, though, it decreases the more ingredients you throw in the proverbial broth. You could overfit a model, which you shouldn't.
#' 
#' One way of thinking about this: if you added a variable that was 100% random noise, the deviance would decrease by 1, on average.
#' If you added a variable that was informative/useful, the deviance would decrease by more than 1.
#' This is additive: adding `k` predictors to a model should decrease the deviance by `k` (or more).
#' 
#' Let's illustrate this. First, let's add a completely uninformative variable.

set.seed(8675309) # Jenny, I got your number...
Penn %>%
  mutate(noise = rnorm(n())) -> Penn

M4 <- update(M2,. ~ . + noise)
summary(M4)

#' ^ Notice: the noise variable adds nothing of value to the model.

#' How much did we reduce the deviance in the model by doing this?
M2$deviance - M4$deviance

#' The difference is less than 1. It's an almost zero reduction in the deviance. This is unsurprising: the variable is noise.
#' 
#' Let's do something a bit more substantive. What if we elected to not model partisanship? We know
#' from the literature that partisan identity is the largest driver of a partisan vote in the United States.
#' Self-identified Republicans vote for Republican candidates. The stronger the attachment, the greater the likelihood of a 
#' partisan vote.
#' 
#' So let's set this question up as an empirical one:
#' 
#' 1) How much worse off would our error rate be if we didn't model partisanship?
#' 2) How bigger would the deviance be in the absence of partisanship?
#' 
#' Let's see.  For one, because I've been playing fast and loose with passing over missing data, let's standardize the data sets
#' to drop NAs. The ANOVA and likelihood ratio test will complain if I don't do this.


Penn %>% select(votetrump, age, female, collegeed, famincr, ideo, pid7na, bornagain) %>%
  na.omit -> Penn

# Re-run M2
M2 <- glm(votetrump ~ age + female + collegeed + famincr + 
            pid7na + ideo + bornagain,
          data = Penn, na.action=na.exclude,
          family = binomial(link="logit"))

#' Now, here's a simple update of the model to take out partisanship.
#' 
M5 <- update(M2,. ~ . -pid7na)
summary(M5)

#' Let's compare the deviances.
 
M5$deviance - M2$deviance

#' This is quite the change in deviance! How might an ANOVA summarize this?

anova(M5, M2, test="Chisq")

#' It's suggesting what you should know by now. Partisanship matters a whole lot in understanding partisan votes in elections,
#' especially for president. Your model will perform worse if you don't include a partisanship measure. That's what the ANOVA is telling you here.
#' 
#' A likelihood ratio test will tell you the same thing, btw, with the exact same statistic. This comes by way of the `{lmtest}` package.
lrtest(M5, M2)

#' The major thing to note here is that this a a likelihood ratio test. I had previously been discussing the deviance. Incidentally,
#' the log likelihood of a GLM * -2 returns the deviance. If you ever see, anywhere in your travels, a statistic with a shorthand of 
#' "-2LL", that's what that is. It's also communicating the deviance.
#' 
#' FWIW, few people (in my experience) ask to see ANOVA. You can pitch model comparison here as an error rate stat.

Penn %>%
  mutate(fittedM2 = fitted(M2, type="response")) %>%
  mutate(fittedM5 = fitted(M5, type="response")) %>%
  select(votetrump, fittedM2, fittedM5 )  %>%
  summarize(meandv = 1 - mean(votetrump, na.rm=T),
            errorrateM2 = mean((fittedM2 > .5 & votetrump == 0) | fittedM2 <.5 & votetrump == 1, na.rm=T),
            errorrateM5 = mean((fittedM5 > .5 & votetrump == 0) | fittedM5 <.5 & votetrump == 1, na.rm=T))

#' Implications: `M2` is a better fit than `M5` deviance-wise, which we established in the likelihood ratio test and the ANOVA.
#' In terms of predictive accuracy, `M2` is preferable to `M5` as well. The error rate in the full model + with partisanship is .15.
#' The error rate without partisanship increases to .221.
