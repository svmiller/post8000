#' ---
#' title: "The Basics of Bayesian Inference"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 15 April 2021
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
#' - [How Should You Think About Your Priors for a Bayesian Analysis?](http://svmiller.com/blog/2021/02/thinking-about-your-priors-bayesian-analysis/)
#' - [What Explains Union Density? A Replication of an Old Article with the {brms} Package](http://svmiller.com/blog/2019/08/what-explains-union-density-brms-replication/)
#' 
#' # R Packages/Data for This Session
#' 
#' Here are the R packages we'll be using today. `{tidyverse}` is for all things workflow and `{stevedata}` has toy data we'll use. `{brms}` is the only
#' new addition to this list. It's my favorite wrapper for estimating Bayesian models. It's a wrapper for the Stan programming language, which it should install for you.

library(tidyverse)
library(brms)
library(stevedata)

#' # The Intuition Behind Bayesian Inference, and Why You Should Consider It
#' 
#' We'll start with a basic illustration of Bayesian inference as it's implemented computationally and join it with a pitch 
#' of why you should think about this approach, no matter its limitations (i.e. computational run-time). The biggest benefit 
#' is that Bayesian inference is actually providing an answer to the question you're asking. In a lot of applications, you want
#' to know the probability of some hypothesis, given the data you obtained analyzed. The alternative/standard approach---the so-called
#' "frequentist" approach---is giving you the probability of the data, given some alternative hypothesis. Thus, the frequentist
#' answer to the question you're asking is basically indirect. The answer you get from the frequentist approach amounts to 
#' "here are some values I can't rule out." The Bayesian answer is a bit more direct: the probability of theta, given the data.
#' 
#' However, this approach requires the introduction of prior beliefs that effectively weight the likelihood function of the data to
#' produce the posterior estimates that you want. These priors amount to reasonable expectations of the data, which you can approach any
#' number of ways. Here's one way of thinking about it, with respect to a simple example that I've seen around the block a few times and
#' incidentally involves a university that has a special place in my heart. Suspend disbelief about adequately sourcing the information presented
#' here and take it as a matter of fact. But, the introduction of physical training during World War II apparently made for quite the fit male
#' student body at the University of Illinois at this time. A simple random sample, purportedly of 100 (male) students, resulted in [a mean mile run time of 7.11 minutes](https://brainly.com/question/13018058)
#'  with a standard deviation of .74 minutes. That would be an amazing level of fitness; I'd think. 
#'  
#' Let's mimic that here, standardizing those times to seconds, and recreating a random sample of 100 observations that would roughly correspond to that.

mean_time <- 7.11*60
sd_time <- .74*60

set.seed(8675309)
illinois_times <- tibble(y = rnorm(100, mean_time, sd_time))

mean_time
sd_time
mean(illinois_times$y)
sd(illinois_times$y)

#' Good enough. 
#' 
#' Here's what we want to know, though. We observe these mile run times where the mean is `r round(mean(illinois_times$y), 2)` seconds and
#' the standard deviation is  `r round(sd(illinois_times$y), 2)` seconds. What should we think is the mean mile time (and error around the mean)
#' given the data we observed?
#' 
#' Here, we reiterate a few things at stake: for one, the probability of A given B is not equal to the probability of B given A. Frequentist
#' approaches aren't going to give you a direct answer to this question. Further, we have 100 observations from which to infer. In the broad
#' scheme of things, that's not a whole lot. This means if we have strong beliefs about what mile times should be, that should exert some influence
#' on how we interpret the data we observe.
#' 
#' We could approach this *a lot* of different ways, but let's do two simple approaches to thinking about prior expectations. First, though, let's
#' use the `{brms}` to remind us what parameters are at stake here.

mile_form <- bf(y ~ 1)
get_prior(mile_form, data = illinois_times)

#' In this simple model, we're calculating just two things: an intercept that will amount to the mean and a residual standard deviation that will
#' amount to the standard deviation of the mean. This approach will also show you what the defaults are. My blog post on how to think about
#' priors for a Bayesian analysis will explain where `{brms}` is getting these priors. If you were curious, this is where `{brms}` would be
#' starting for these two parameters.

set.seed(8675309)
tibble(int_prior = rstudent_t(10000, 3, 429, 42.9),
       sig_prior = rstudent_t(10000, 3, 0, 42.9)) %>%
  gather(var, value) %>%
  ggplot(.,aes(value)) +
  facet_wrap(~var, scales = "free") +
  geom_density()

#' When curious about priors, embrace simulating values from them to see what they look like. Here, the intercept prior seems okay even as the 
#' left tail of it cluster around a five-minute mile (and I think we can probably rule that out as a plausible value since that would be an
#' elite time). The sigma amounts to a "half-t" because standard deviations cannot be negative. These aren't awful priors, but I think we can do
#' a tiny bit better while encouraging you to think about your data a little bit more.
#' 
#' This seems daunting, but this is a clear "use your head" issue here and, thing is, you don't even have to use all your head! Just use a little of it.
#' Here are two examples to get you started. For one, here's a news source that suggests the average mile time for young men in and around that age
#' bracket is ["about 9 to 10 minutes."](https://www.healthline.com/health/average-mile-time). Let's take the upper bound of that as the mean and use that
#' minute window in the "about 9 to 10 minutes" to suggest a standard deviation of about a minute. We'll make this a skinny prior too. In other words,
#' we're reasonably sure about these as prior estimates for the mean mile time and the standard deviation mile time.
#' 

ten_minute_prior <- c(set_prior("normal(600, 10)", class="Intercept"),
                      set_prior("normal(60, 10)", class = "sigma"))

#' Here's another approach that requires only a little of your head. We know that a four-minute mile is near impossible to crack. People have, but that is
#' an *elite* time. So, we don't think the *mean* could possibly be less than four. We also know the average person *walks* at a pace of around
#' [3 to 4 miles per hour](https://www.healthline.com/health/exercise-fitness/average-walking-speed). That would mean a person could complete a mile just
#' by walking in around 15 minutes. It could not possibly be less than 4 minutes and, in all likelihood, it could not be more than 15. However, any time
#' between those extremes is equally likely. If we standardize that to seconds, we have a minimum of 240 and a maximum of 900.
#' In other words, the distribution here could follow a uniform distribution between both extremes for the mean.
#' We'll set a similar one for the standard deviation of the mean too. Let's say: 0 to 60. I want to note that thinking of an upper bound for
#' the residual standard deviation is *really* dumb and `{brms}`/Stan is going to bark at you for this. It'll still run, though. It'll just tell you
#' that it think it's stupid to impose an upper bound on a standard deviation like this.

unif_form <- bf(y ~ 0 + Intercept) # see what I did there
get_prior(unif_form, data=illinois_times)

uniform_prior <-  c(prior("uniform(240, 900)", lb=240, ub=900, class="b"),
                    prior("uniform(0, 60)",  class="sigma"))

#' I'll only add the caveat here that Bayesian modelers are generally loathe to encourage people, especially beginners, to hide behind the uniform distribution.
#' It truly is the laziest distribution. I'm sure it's why `{brms}` is going to make it just a tiny bit more difficult to do this.
#' 
#' No matter, let's see what our priors mean for the inferences we want to make. Let's start with the default priors given by `{brms}`. I want to note, in this application,
#' I'm going to care mostly about the intercept (i.e. the mean mile time).

M1 <- brm(mile_form, 
          family = gaussian(), # the normal distribution 
          data=illinois_times,
          seed = 8675309, # reproducible seed
          sample_prior = TRUE, # give us samples from the prior distribution too
          refresh = 0, # by default, Stan spams the console with sampling updates. Disable that.
          )

#' Now, let's use some `{brms}` functions to extract the output of interest to us. `posterior_samples()` returns the posterior
#' estimates (`b_Intercept` and `sigma`) along with simulations from the prior distribution as well (i.e. `prior_Intercept` and
#' `prior_sigma`). Some "tidy"-friendly verbs will make the following plot.

posterior_samples(M1)  %>% as_tibble() %>%
  select(b_Intercept, prior_Intercept) %>%
  gather(var, val) %>%
  bind_rows(., tibble(var = "actual", val = illinois_times$y)) %>%
  ggplot(.,aes(val, fill=var)) + geom_density(alpha=0.8) +
  theme_bw() + theme(legend.position = "bottom")

#' I want you to notice what happened. The `{brms}` default priors lean on the available data. As a result, the posterior estimate of the mean
#' basically converges on the actual mean. `{brms}` default approach, in the absence of further guidance, is to devise priors that exert minimal
#' weight on the posterior estimates.
#' 
#' Let's see what a bit more informative prior looks like. Here, we suspect the mean is about 10 minutes, give or take 10 seconds. We're comparably 
#' sure about the residual standard deviation as well, suggesting a mean of 60 seconds with a standard deviation of 10 seconds.


M2 <- brm(mile_form, 
          family = gaussian(),
          data=illinois_times,
          seed = 8675309,
          sample_prior = TRUE,
          refresh = 0,
          prior = ten_minute_prior # specify our priors
)

posterior_samples(M2) %>% as_tibble() %>%
  select(b_Intercept, prior_Intercept) %>%
  gather(var, val) %>%
  bind_rows(., tibble(var = "actual", val = illinois_times$y)) %>%
  ggplot(.,aes(val, fill=var)) + geom_density(alpha=0.8) +
  theme_bw() + theme(legend.position = "bottom")
 
#' Here, observe that the prior had a fairly strong effect on the posterior estimates. We were so sure the mean was 10 minutes. The data we observed, which
#' had a mean of just over 7 minutes, lead us to update our assessments of what the mean could be. We thought it was 10 minutes. We observe a mean of just over 7
#' minutes. However, the somewhat paltry nature of the data (100 observations) and the strength of the prior mean we update our belief that the mean is about
#' `r round(mean(posterior_samples(M2)$Intercept)/60, 2)` minutes.
#' 
#' Now, the stupid uniform distribution is going to produce a take-away similar to the default `{brms}` approach. Because the uniform distribution is flat with no natural
#' peak, the posterior estimates are going to converge on the observed data.

M3 <- brm(unif_form,
          family = gaussian(),
          data=illinois_times,
          seed = 8675309,
          sample_prior = TRUE,
          refresh = 0,
          prior = uniform_prior)

posterior_samples(M3) %>% as_tibble() %>%
  select(b_Intercept, prior_b) %>%
  gather(var, val) %>%
  bind_rows(., tibble(var = "actual", val = illinois_times$y)) %>%
  ggplot(.,aes(val, fill=var)) + geom_density(alpha=0.8) +
  theme_bw() + theme(legend.position = "bottom")

#' If you have weak data and some prior expectations about it, you should use a Bayesian approach. If you want a bit more direct answer to your question about the state of the world,
#' given some observed data, you should also use a Bayesian approach.
#' 
#' # How Should You Use Bayes For Your Own Purposes?
#' 
#' The "why" for Bayes is multiple and the rationale above reduces to 1) it's actually answering the question you're asking and 2) it allows prior beliefs
#' about the data to inform our probabilistic assessments about the nature of the world given the question you're asking. If you don't have a lot
#' of data to bear, the prior expectations of it can exert a fair bit of influence on the posterior estimates. The "how" may not be 100% obvious, though.
#' So, here are some tips for how to use Bayesian approaches to statistical inference for practical purposes.
#' 
#' ## Competitive Hypothesis Tests
#' 
#' My first introduction to Bayesian inference was in the form of a competitive hypothesis test pitting two arguments about union density. This will clearly be aped
#' from my blog post on this from 2019. Here's the tl;dr, though. The outcome of interest is union density for 20 rich countries around the end of the 1970s. One side
#' says the primary driver of union density is logged labor force size (intuition: higher labor force -> greater difficulty in achieving collective action). Another
#' side says the primary driver of union density is industrialization (intuition: industrialization played an outsized role in the labor movement and addresses
#' the collective action problem through concentration of the population into urban centers). The problem, however, is that both industrialization and logged labor force size
#' are almost perfectly correlated.
#' 
#' The data are available in `{stevedata}` as `uniondensity`.

uniondensity
?uniondensity

#' Here's the correlation of the two variables.

uniondensity %>%
  summarize(cor = cor(concen, size))

#' This means any regression model is going to struggle to find the partial effect of either one for evaluating this hypothesis. No matter, Bayes is going to provide a path
#' forward through the use of prior beliefs about the very paltry nature of the data. First, let's specify the regression model we'll be using. Note, only, there is a 
#' control variable for an index of left-wing governments over time in these 20 countries.

union_form <- bf(union ~ left + size + concen)

#' Let's see what parameters in the model would need prior beliefs slapped on them.

get_prior(union_form, data=uniondensity)

#' Now, let's create two sets of priors. One is "Team Logged Labor Force Size" an the other is "Team Industrial Concentration." Please read the underlying article/texts for
#' the justification for these particular values.

labor_priors <- c(set_prior("normal(3,1.5)", class = "b", coef= "left"),
                  # team labor force size
                 set_prior("normal(-5,2.5)", class = "b", coef="size"), 
                 # doesn't think industrial concentration has anything to do with it
                 set_prior("normal(0,10^6)", class="b", coef="concen"), 
                 set_prior("normal(0,10^6)", class="Intercept"))

industry_priors <- c(set_prior("normal(3,1.5)", class = "b", coef= "left"),
                     # doesn't think labor force size has anything to do with it
                     set_prior("normal(0,10^6)", class = "b", coef="size"), 
                     # team industrial concentration
                     set_prior("normal(10,5)", class="b", coef="concen"), 
                     set_prior("normal(0,10^6)", class="Intercept"))

#' Now, let's run two models comparing what these priors beliefs do to the nature of our data, which have so few observations to bear on this debate.

union_mods <- list()

union_mods$"labor" <- brm(union_form,
                          data = uniondensity,
                          seed = 8675309,
                          sample_prior = TRUE,
                          refresh = 0,
                          prior = labor_priors)

union_mods$"industry" <- brm(union_form,
                          data = uniondensity,
                          seed = 8675309,
                          sample_prior = TRUE,
                          refresh = 0,
                          prior = industry_priors)

#' Let's compare the results.

summary(union_mods[[1]])
summary(union_mods[[2]])

#' The takeaway, as it was in Western and Jackman (1994) and in the replication on my blog: if you were on "Team Logged Labor Force Size",
#' the data observed suggest the effect is there whether you built in strong prior beliefs it should be there OR you built in ignorance priors
#' and were agnostic to the effect of that variable. If you were on "Team Industrial Concentration", the effect is there *only* if you built in strong
#' prior beliefs that it should be there. Thus, given the nature of these analyses, you're probably going to give the day to "Team Logged Labor Force Size" 
#' and tell "Team Industrial Concentration" that you'd have to build in strong prior beliefs about the effect to result in a posterior distribution of results
#' consistent with the prior beliefs.
#' 
#' Dedicated Bayesian researchers will say that this is just "a" thing you can do with Bayes that you really can't do with standard models. They'll probably
#' also discourage you from thinking too much about this or that it's all you can do with Bayes, but I think it as a nice place to start thinking about Bayes.
#' 
#' ## Related: Leverage the Prior over Weak Data
#' 
#' You could also use the prior distribution as a sort of "leverage" or "hedge" over weak data. Consider one of my go-to linear models, which explains immigration sentiment
#' as a function of various things in the United Kingdom. Here would be the data, the codebook, and the descriptive statistics.
#' 
ESS9GB
?ESS9GB

ESS9GB %>% select(immigsent:lrscale) %>%
  mutate(n = n()) %>%
  summary

#' I want you to note that unemployment variable. There is a lot of scholarship in the political economy of immigration that suggests worsening economic prospects lead
#' to negative evaluation of immigrants, the extent to which the native-born population sees them as competition for employment or potential drains on the social safety
#' net. So, we'd expect a negative effect to come out in the linear model.
#' 

immig_mods <- list()

immig_mods$"LM" <- lm(immigsent ~ agea + female + eduyrs + uempla + hinctnta + lrscale,
                      data = ESS9GB)

summary(immig_mods$"LM")

#' Instead, we get this result in which the standard error for the estimate swallows the coefficient. Curious as to why that happened, I look at the data and see how many
#' unemployed people I have in the model.
#' 
ESS9GB %>% filter(uempla == 1) %>%
  select(immigsent:lrscale) %>%
  na.omit

#' I have just 30 unemployed people in this sample. I have no real power to make this kind of assessment based on the data alone. Even as the 1,454 respondents
#' result in a sufficiently powered analysis overall, this particular coefficient is going to struggle.
#' 
#' Let's think of a hypothetical way to approach this. The data here are from 2018. Let's say I ran a similar analysis in 2016 and found that the effect of
#' being unemployed lowers the immigration sentiment variable by about 2.12 points and that the standard deviation on that coefficient is 1.26. This, by the
#' way, is what a naive *t*-test would suggest.

ttest_uempl <- t.test(immigsent ~ uempla, data=ESS9GB)

broom::tidy(ttest_uempl) %>%
  select(estimate:p.value)

round(as.vector(abs(diff(ttest_uempl$estimate)/ttest_uempl$statistic)), 2)

#' We can think of that as a kind of "conventional wisdom" about the effect of being unemployed on immigration sentiment and that the problem of the 2018 data is that
#' we just don't have too many unemployed people in the proverbial pool. Here, let's specify that as a prior.

umempl_prior <- c(set_prior("normal(-2.12,1.26)", class="b", coef="uempla"))

immig_mods$"uempl_prior" <- brm(immigsent ~ agea + female + eduyrs + uempla + hinctnta + lrscale,
                                data = ESS9GB,
                                seed = 8675309,
                                prior = umempl_prior,
                                refresh = 0,
                                family = gaussian())

summary(immig_mods$"uempl_prior")

#' Notice the effect the prior had compared to the standard linear model. The only downside to doing this is that, if this were a coefficient of supreme interest,
#' for a given project, expect a reviewer to ask you to perform a "sensitivity analysis." Basically, play around with a few priors and see if the results are
#' sensitive to the use of a strong prior.
#' 
#' ## Other Uses of Bayes
#' 
#' I think I lack the space and time to really say how else you can use Bayesian analysis. Other things you can do with Bayes include:
#' 
#' - Imposing "reasonable ignorance" priors on your data. I talk a bit about this in [a blog post from earlier this year](http://svmiller.com/blog/2021/02/thinking-about-your-priors-bayesian-analysis/).
#' Basically, one major plea from Bayesians is that you should really think more about your data and your model than you probably do. You don't need to do your regression by way of
#' matrix algebra, but you should at least know what parameters are of interest in your model, how they could conceivably be distributed, and what values are (im)possible.
#' - Estimating a variety of models. Bayesian models in the Stan programming language can do a lot of different kinds of models that you would need to spread across multiple libraries/packages. In particular, I think
#' `{brms}`/`{stan}` do multinomial models and hurdle models better than any other frequentist package I've seen.
#' - Meta analysis. [The "eight schools" example](https://statmodeling.stat.columbia.edu/2014/01/21/everything-need-know-bayesian-statistics-learned-eight-schools/) is really illustrative to how cool Bayesian approaches are to things.  
#' 
#' # Diagnostics of Your Bayesian Model
#' 
#' One thing I like about Bayesian models, the more I use them, is how verbose they are about potential problems. Standard modeling approaches will either leave you
#' to figure out a potential problem on your own (e.g. weak data in the above case of unemployment and immigration sentiment), or fail completely (e.g. separation
#' problems in a logistic regression). Bayesian models in the Stan programming language are, by contrast, much more verbose about potential issues and things for you to explore.
#' Here are the things you should do to assess your model output.
#' 
#' First, plot the chains/samplers. If everything has gone well, you should see something like this. Informally---super informally---you want to see
#' "fuzzy caterpillars." If you see "fuzzy caterpillars" with no weird spikes, the sampler had no issues.

plot(immig_mods$"uempl_prior")

#' Next, look at the summary of the model.

summary(immig_mods$uempl_prior)

#' Take inventory of the following things: `Rhat`, `Bulk_ESS`, and `Tail_ESS`. `Rhat` is the R-hat convergence diagnostic that compares between- and within-chain estimates
#' for the parameters of interest. If things have not mixed well, `Rhat` will be larger than 1. If there is a problem, believe me, `{stan}` will tell you.
#' 
#' `Bulk_ESS` is the estimated bulk effective sample size using rank normalized draws. It's useful for measuring sampling efficiency `Tail_ESS` is the estimated
#' tail effective sample size. It's useful for measuring the sampling efficiency of the tails of the distribution, which, well, are at the tails and are things you want to
#' sample well even as they are the tails. You want *both* to be at least 100 per chain you estimate. If they are, it suggests the posterior estimates are reliable. If you fail
#' to get this from your model, trust me, Stan will let you know.
#' 
#' ## What Should I Do if Something is Wrong in My Model?
#' 
#' Stan is really good at pointing at problems in your data and model---and, from my experience, frustratingly good. As it points to problems, it also points to potential solutions.
#' I'll only reiterate a few things you may encounter, which are mostly going to be applicable to more complicated models than your standard linear models (and what have you).
#' 
#' If your chains have not mixed well or your effective sample size is really low, Stan think you should use stronger priors on your data. You will typically encounter this
#' in cases where you're electing to not think too much about your data (speaking from experience) and you're hoping the programming language does that for you. For simple models,
#' that works well enough. For more complicated models that have things like random intercepts, slopes, and built-in covariance structures, you should think a little bit more
#' about your data.
#' 
#' You may get a warning about "divergent transitions." From experience, this means you're probably using a very complicated model and the data for it have some features that
#' are hard to explore. You have one of two options here for this. You can explore model reparameterization, which I almost never do, or you can increase the `adapt_delta` as
#' a control function. In `{brms}`, something like this should work in the `brm()` wrapper: `control = list(adapt_delta = .9)`. Of note: I believe the default is .8, I kick it up
#' to .9 if I have a problem and then keep increasing it if I still have this problem. Should you fix this problem, you'll address the concern of bias in the sampler. However,
#' sampling time does go up.
#' 
#' You may also get a warning that says something to the effect of "maximum treedepth exceeded." This is more an efficiency concern. I think of this as an easy problem to
#' fix. Add `control = list(max_treedepth = 15)` to your `brm()` wrapper. The default maximum treedepth is 10, btw.