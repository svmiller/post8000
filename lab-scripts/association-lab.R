#' ---
#' title: "Associational and Inferential Techniques"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 28 January 2021
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

#' # Elsewhere on My Website
#' 
#' Some of what I write here is taken from my website. Please read these:
#' 
#' - [Illustrate Some Correlation Limitations and Fallacies in R](http://svmiller.com/blog/2020/01/illustrate-correlation-fallacies-limitations-in-r/)
#' - [The Normal Distribution, Central Limit Theorem, and Inference from a Sample](http://svmiller.com/blog/2020/03/normal-distribution-central-limit-theorem-inference/)
#' 
#' Be advised I might plagiarize myself here.
#' 
#' # R Packages/Data for This Session
#' 
#' You should've already installed two of the following R packages for this lab session. `{tidyverse}` will be for all things workflow and `{stevedata}` will be for the toy data sets.
#' Let's load those libraries now. The order doesn't really matter here. I think I had mistaken that `{broom}` was in `{tidyverse}` and it does not appear to be. No matter
#' It's a wonderful package for a lot of things---probably one of the best value-added packages in all of R. Install it if you have not.
#' 
#' The following thing below is something I wrote for the IPSA-FLACSO program in Mexico City in 2019---and, sidebar, I miss the hell out of that amazing city and FLACSO should
#' invite me back for things. Basically, this thing looks for whether you have downloaded/installed a package from CRAN and, if you don't have it, it installs it.

pack <- c("broom")
new_pack <- pack[!(pack %in% installed.packages()[,"Package"])]
if(length(new_pack)) install.packages(new_pack)

#' Let's load what we'll be using here.

library(tidyverse)
library(stevedata)
library(broom)

#' # Correlation
#' 
#' Let's start with an illustration of basic correlation. There are any number of ways of doing a rudimentary correlation in R, but 
#' it's worth noting that the `cor()` function comes standard in R. However, like the `mean()`, `median()` and `sd()` functions, you'll
#' need to be mindful of NAs in your data. If you have them, you'll want to add use = "complete.obs" as an extra parameter in `cor()`.
#' 
#' Let's do something simple. Let's correlate the `sumnatsoc` measure (which increases as the respondent believes we're spending too little
#' on various social programs) with the respondent's ideology. As always, check `?gss_spending` for more clarity about the measure.

cor(gss_spending$polviews, gss_spending$sumnatsoc, use="complete.obs")

#' This will tell you the Pearon's *r* (correlation coefficient) between both is -.35, which is a decently strong negative correlation.
#' Be mindful of a few things with correlation. While there is such a thing as a correlation test for significance (which I'll talk a little
#' bit about for instrumental variables later in the semester), most assessments of correlation beyond -1, 0, or 1 are informal. Notice the
#' awkwardness of "decently strong negative" correlation. To be clear, the correlation is negative, but the strength of it and how you summarize
#' it is a "use your head" issue here.
#' 
#' Further, and recall, correlation is ambivalent about what is *x* and what is *y*. You can flip the two and get the exact same Pearon's *r*
#' back.
#' 
cor(gss_spending$sumnatsoc, gss_spending$polviews, use="complete.obs")

#' But, if you're going to just explore without overwriting anything, you can add it to a pipe workflow. Observe:

gss_spending %>%
  summarize(r = cor(polviews, sumnatsoc, use="complete.obs"))

#' I end up doing it this way since it's less code. `{tidyverse}` for the win.
#' 
#' Correlation is super useful as preliminary exploratory data analysis, and for checking for multicollinearity concerns.
#' But be mindful. Correlation may mislead you. Let's start with Anscombe's quartets.
#' 
#' ## Anscombe's Quartets
#' 
#' Francis J. Anscombe’s (1973) famous quartet is a go-to illustration for how simple associational analyses, 
#' even linear regression, can mislead the researcher if s/he is not taking care to actually look at the 
#' underlying data. Here, Anscombe created four data sets, each with 11 observations and two variables (x and y). 
#' All four quartets have the same mean for x and y, the same variance for x and y, the same regression 
#' line (y-intercept and slope for x), the same residual sum of squares, and the same correlation. However, they look quite differently.
#' 
#' Let's look at the data first.

?quartets
quartets

#' Notice these four quartets each have the same mean and standard deviation for x and y.
quartets %>%
  group_by(group) %>%
  summarize_if(is.numeric, list(mean = mean, sd = sd))

#' The correlations are effectively the same for all four quartets.
quartets %>%
  group_by(group) %>%
  summarize(r = cor(x, y))

#' The linear function of y ~ x is practically the same across all four quartets. This is where I asked to download the `{broom}` package.
#' I'll annotate this a little bit to make it clearer.
quartets %>%
  # nest the data under the particular group
  nest(data = -group) %>%
  # Then, using {purrr} `map()` magic, do a linear regression on each group.
  # Then, using {broom}'s `tidy()` magic to gank the results into a table
  mutate(fit = map(data, ~lm(y ~ x, data=.)),
         tidied = map(fit, broom::tidy)) %>%
  # unnest the table to show us what we want.
  unnest(tidied) %>%
  # select just what we want to see.
  select(group, term:p.value)

#' However, each quartet looks quite different.

quartets %>%
  ggplot(.,aes(x,y)) +
  facet_wrap(~group) + geom_point() + geom_smooth(method = "lm")

#' Basically: look at your data and be mindful that you can be easily misled if you don't do this.
#' 
#' ## The Datasaurus
#' 
#' If you want a more extreme example of the same basic thing, check out the `Datasaurus` data in `{stevedata}`.
?Datasaurus
Datasaurus

#' Basically, the people who wrote these data rightly complained that Anscombe wasn't 100% transparent with how he created the data he did.
#' Plus, you can have more fun with these data. Note that the descriptive stats here are basically identical.

Datasaurus %>%
  group_by(dataset) %>%
  summarize(x_bar = mean(x),
            x_sd = sd(x),
            y_bar = mean(y),
            y_sd = sd(y),
            cor = cor(x, y))

#' The regression line is basically the same.

Datasaurus %>%
  nest(data = -dataset) %>%
  mutate(fit = map(data, ~lm(y ~ x, data=.)),
         tidied = map(fit, broom::tidy)) %>%
  # unnest the table to show us what we want.
  unnest(tidied) %>%
  # select just what we want to see.
  select(dataset, term:p.value)

#' But the data look quite different.

Datasaurus %>%
  ggplot(.,aes(x, y)) + geom_point() +
  facet_wrap(~dataset)

#' Always look at your data. Roar.
#' 
#' ## Simpson's Paradox
#' 
#' Simpson’s paradox is a well-known problem of correlation in which a correlation analysis, almost 
#' always done in a bivariate context, may reveal a relationship that is reversed upon the introduction 
#' of some third variable. Every stylized case I’ve seen of Simpson’s paradox conceptualizes the third factor as 
#' some kind of grouping/category effect (certainly of interest to me as a mixed effects modeler), even if this may not be necessary to show a Simpson reversal.
#' 
#' One of the classic pedagogical cases of a Simpson’s paradox is from Deborah Lynne Guber’s (1999) study 
#' of public school expenditures and SAT performance in 1994-95 across all 50 states. There are other cases 
#' I’ve seen of a Simpson reversal, incidentally most involving education, but these data were readily available 
#' and are part of my `{stevedata}` package as the `Guber99` data frame.
?Guber99
Guber99

#' In Guber's (1999) case, she encounters a troubling negative correlation (among other negative correlations) that
#' states that spend more (per pupil) on their public school students have lower SAT scores than those who spend less. Observe:
Guber99 %>%
  summarize(r = cor(expendpp, total))

#' But there's a lurking variable. Use your head on this one:
#' 
#' 1. The ACT is a test-taking alternative in a lot of these states.
#' 2. Spendier states may have a regression to the mean, of sorts: they want more students taking these tests/going to college. 
#' 3. Meanwhile, only the most motivated, ambitious few in some of these other states are taking the SAT (to go out of state).
#' 
#' Some quick and dirty quartiles by the percentage of the state taking the SAT will show a Simpson reversal.
Guber99 %>%
  # create quick-and-dirty quartiles for perc of state taking SAT
  mutate(group = as.factor(ntile(perctakers, 4))) %>%
  group_by(group) %>%
  summarize(r = cor(expendpp, total))

#' At every quartile of SAT test-taker percentage, there is a positive correlation of student expenditures on SAT performance statewide.
#' [The post on my website](http://svmiller.com/blog/2020/01/illustrate-correlation-fallacies-limitations-in-r/) has a fancy pair of graphs
#' to drive this home.

#' ## Ecological Fallacy
#' 
#' We have Robinson (1950) to thank by showing that ecological correlations are not substitutes for individual correlations. These data are
#' available as `illiteracy30` in my `{stevedata}` package.

?illiteracy30
illiteracy30

#' Robinson (1950) collected data from the 1930 Census, which unsurprisingly showed a discrepancy of over-10 literacy rates
#' of non-white, non-native populations relative to the white, native-born population in the U.S.
#' FWIW, consider the time frame here. The public good of education has always been unequally allocated by race and immigrant status.
#' Further, some basic background of which Europeans were migrating to the U.S. and why will underscore what you see here.

# for context: fbwhite = "foreign born white" (i.e. immigrant),
# nwhite = native-born white, 
# fpwhite = white from "foreign/mixed parentage"
illiteracy30 %>%
  summarize_if(is.numeric, sum) %>%
  gather(var) %>%
  separate(var, c("category","literacy"),"_") %>%
  mutate(literacy = ifelse(!is.na(literacy), "Illiterate", "Total Population")) %>%
  group_by(category) %>%
  summarize(prop = min(value)/max(value))
 
#' That literacy rates are lower among migrants at this point is not a controversial thing to say.
#' But, incidentally, the correlation is *negative* between foreign population and percentage of the population that is illiterate at the state-level.
#' The only caveat to drawing a single line here is that I see a curvilinear relationship even as that's immaterial for illustrating the ecological
#' fallacy.

illiteracy30 %>%
  mutate(foreignp = fbwhite/pop,
         illiterate = pop_il/pop,
         fbilliterate = fbwhite_il/fbwhite) %>%
  ggplot(.,aes(foreignp, illiterate)) + geom_point() +
  geom_smooth(method = "lm")

#' Why? Simple: immigrants were moving to places with lower illiteracy rates, which generally had more economic opportunity (e.g. NY, NJ)
#' Basically, ecological correlations are not substitutes for individual correlations.
#' 
#' # Central Limit Theorem
#' 
#'  Central limit theorem says:
#'  
#'  1. with an infinite number samples of size n...
#'  2. from a population of N units...
#'  3. the sample means will be normally distributed
#'  4. Further: the mean of sample means would equal the population mean
#'  5. Random sampling error would equal the standard error of the sample means.
#'  
#'  Let's illustrate this with some thermometer rating data. This is Trump's therm score from the ANES exploratory testing survey done April 10-18, 2020.
#'  I love thermometer data to teach central limit theorem because it shows the underlying principles, even in data that are noisy/ugly as hell. 
#'  First, read more about the data and see it here. Just so we're on the same page: higher values indicate "warmer" attitudes toward Trump (i.e. more favorable attitudes). 
#'  Lower values indicate "colder" attitudes toward Trump (i.e. less favorable attitudes).
?therms
therms

#' Observe how ugly as hell the distribution of this variable is.
therms %>%
  ggplot(.,aes(fttrump1)) + geom_bar()

#' These data are ugly, right? Even as the mean and median are not too far removed from each other, the large standard deviation on a bounded 0-100 scale suggests
#' something is weird about the data (that you just saw).

therms %>% select(fttrump1) %>%
  na.omit %>%
  summarize_all(list(mean = mean,
                     median = median,
                     sd = sd))

#' So let's do something here. Let's treat this therm score as the "population". I.e. of 2500 or so observations, this constitutes
#' the full universe of thermometer ratings for Donald Trump. Now, we we want to see if we can approximate it by sampling from it.

therms %>%
  select(fttrump1) %>% na.omit %>%
  pull(fttrump1) -> Trump

#' Now check this shit out. First, let's set a reproducible seed for sampling so you and I get the same samples back.
#' PRO TIP: you should always set a reproducible seed for any simulation. It's good practice for max reproducibility.
#' We are going to take 1,000,000 samples of 10 observations from the data and report the means of them.
#' Let's be clear: no one should ever strive to have a random sample of 10 people. But we're going for something here:

set.seed(8675309) # Jenny, I got your number...
trumpsamples <- tibble(
  samplemean=sapply(1:1000000,
           function(i){ x <- mean(
             sample(Trump, 10,
                    replace = FALSE))
           }))
#' ^ this might take a few seconds. You're doing a million samples, after all.

#' Let's plot the distribution of our sample means. Here's a histogram underneath a density function of 
#' the normal distribution with the parameters derived from the simulations.
trumpsamples %>%
  ggplot(.,aes(samplemean)) + geom_histogram(binwidth=.5,aes(y=..density..)) +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(trumpsamples$samplemean),
                          sd=sd(trumpsamples$samplemean))) +
  xlab("Sample Mean") + ylab("Density")

#' Looks "normal", right?
#' 
#' Before going too much further, let's talk about a normal distribution.
#'  Underlying concept: data routinely cluster around some central tendency and deviations from it are less common.
#'  We owe the articulation/formalization of this concept to Carl Friedrich Gauss, who discovered this bell curve function.
#'  In Gauss' formulation: f(x) = (1/sqrt(2*pi*sigma^2))exp({-(x - mu)^2/2*sigma^2}) where mu is the mean and sigma is the variance.
#'  Some important properties of the normal density function/Gaussian distribution:
#'  
#'  1. The tails are asymptote to zero. They approach it but don't touch it.
#'  2. The kernel inside the exponent is a basic parabola. The negative sign flips it downward.
#'  3. It's denoted as a function and not a probability because it is a continuous distribution. The probability of any one value is near zero.
#'  4. The distribution is perfectly symmetrical. x is as far from mu as -x is from mu.
#'  5. x is unrestricted. It can be any value.
#'  6. mu defines the central tendency
#'  7. sigma defines how short/wide the distribution is.
#'  8. A special case occurs when mu is zero and sigma is 1. The formula becomes a lot simpler. I'll belabor that bit later.
#'  
#' Anywho, how did we do?
mean(Trump)
mean(trumpsamples$samplemean)

#' Not bad. We were off by around 4/10000th of a point. A million is a lot, but not infinity after all. The principle that emerges is still clear.
#' 
#' Central limit theorem shows its ideal to take a shitload of samples to guess a population parameter from it.
#' Sometimes, that's not practical. You really can't take infinity samples of the United States in a time-invariant way.
#' So, there's another way: get a good-sized random sample. If you understand random sampling error, you'll know you can't do
#' much about the variation inherent in the population. In our case, these data are ugly as hell. What you can do, however, is 
#' reduce the sampling error by increasing the size of the sample.
#'
#' Here's how we'll do it. We're going to take 10 random samples of sizes 10, 100, 400, and 1000. Observe:

moresamples = tibble()

set.seed(8675309) # Jenny, I got your number...
for(j in c(10, 100, 400, 1000)) {
  hold_this <- tibble(
    x= j,
    y=sapply(1:10,
             function(i){ x <- mean(
               sample(Trump, j,
                      replace = FALSE))
             }))
  moresamples <- bind_rows(moresamples, hold_this)
}
 
#' Now let's look at the results. In the code below, each dot coincides with one of 10 sample means of a given sample size (10, 100, 400, and 1000).
#' The dashed horizontal line communicates the actual population mean as we understand it.

moresamples %>%
  ggplot(.,aes(x, y)) + geom_point(size = 3) +
  scale_x_continuous(breaks = c(10, 100, 400, 1000)) +
  xlab("Sample Size") + ylab("Sample Mean") +
  geom_hline(aes(yintercept=mean(Trump)), linetype="dashed")

#' Here's how you should interpret the plot above.
#' Notice that 10 samples of 10 gets you results all over the place. 
#' This is unsurprising. We have a few samples. The sample size is small. The variation in the population is huge. We're all over the place.
#' Things start to get better at 100 and 400 samples. Notice how increasing the sample size increases the accuracy of the sample mean.
#'  They're starting to bunch around the actual population mean.
#'  The 10 samples of size 1,000 are all hovering closely around the actual population mean.
#'  PRO TIP: survey researchers have learned that a national poll "sweet spot" is about 1000 respondents. Most polls you see are thereabouts in size.
#'  It's a "bang for your buck" thing. Fewer than 1000 observations and your random sampling error starts to get appreciably bigger.
#'  Any more than that and, generally, you might find yourself careening into Literary Digest Poll territory.
#'  To be clear: others do some large-N surveys quite well, but increasing the sample size just to increase it does incentivize some non-random sampling.
#'  Plus, it's more expensive and the "payout" from a larger sample size may not be worth the cost. 
#'  It's why you get that sweet spot you routinely observe from opinion polls.
#'  
#' I mentioned above that a special case of the normal distribution occurs when mu is zero and sigma is 1.
#' It allows for an easier summary of the distribution and the areas underneath it.
#' To reiterate: the probability of any one particular value is basically zero (since it's continuous), but 
#' the area underneath the distribution constitutes the full domain and sums to 1.
#' There are some set intervals around the mu that emerge in interesting ways.
#' Observe where the cutoffs are for 68% of the distribution around the mean, 90%, 95%, and 99%.

# For ease, just use normal_dist() in {stevemisc} 
ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm,
                xlim = c(qnorm(.05),abs(qnorm(.05))), size=0,
                geom = "area", fill="#F66733", alpha=0.5) +
  stat_function(fun = dnorm,
                xlim = c(qnorm(.025),abs(qnorm(.025))), size=0,
                geom = "area", fill="#F66733", alpha=0.4) +
  stat_function(fun = dnorm,
                xlim = c(qnorm(.005),abs(qnorm(.005))), size=0,
                geom = "area", fill="#F66733", alpha=0.3) +
  geom_segment(x=1, y=0, xend=1, yend=dnorm(1,0,1), color="white", linetype="dashed") +
  geom_segment(x=-1, y=0, xend=-1, yend=dnorm(1,0,1), color="white", linetype="dashed") +
  annotate(geom = "text", x = 0, y = 0.2,
           label = "68%", size =4.5, color="white") +
  geom_segment(x=-0.15, y=.2, xend=-.99, yend=.2, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_segment(x=0.15, y=.2, xend=.99, yend=.2, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  annotate(geom = "text", x = 0, y = 0.1,
           label = "90%", size =4.5, color="white") +
  geom_segment(x=-0.15, y=.1, xend=-1.64, yend=.1, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_segment(x=0.15, y=.1, xend=1.64, yend=.1, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  annotate(geom = "text", x = 0, y = 0.05,
           label = "95%", size =4.5, color="white") +
  geom_segment(x=-0.15, y=.05, xend=-1.95, yend=.05, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_segment(x=0.15, y=.05, xend=1.95, yend=.05, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  annotate(geom = "text", x = 0, y = 0.01,
           label = "99%", size =4.5, color="white") +
  geom_segment(x=-0.15, y=.01, xend=-2.57, yend=.01, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_segment(x=0.15, y=.01, xend=2.57, yend=.01, color="white",
               arrow = arrow(length = unit(0.15, "cm"))) +
  stat_function(fun = dnorm, color="#522D80", size=1.5) +
  scale_x_continuous(breaks=c(-4, -2.58, -1.96, -1.645, -1, 0,
                              1, 1.645, 1.96, 2.58, 4)) +
  ggtitle("The Area Underneath a Normal Distribution")

#' It's why standardization is useful, among other things. 
#' Standardization (where the mean is zero and the sd = 1) is easy to calculate.
trumpsamples %>%
  mutate(z = (samplemean-mean(samplemean))/sd(samplemean)) -> trumpsamples

#' Let's compare our standardized million sample means to the basic normal distribution.
trumpsamples %>%
ggplot(., aes(z)) + geom_density() +
  stat_function(fun=dnorm,
                color="red"
  ) +
  xlab("Sample Mean (Standardized)") + ylab("Density") +
  ggtitle("Distribution of Sample Means from a Million Samples of Ten Respondents") +
  labs(caption="Data: 2018 ANES pilot study.")


#' It also allows us to make some kind of inferential statements. So let's do this:
#' We are going to collect 100 observations, randomly sampled, from this "population."
#' We're playing god here, if you will. We know what the "population" mean is. We also know the "population" standard deviation.
#' But, let's see what we can do here:

set.seed(8675309) # Jenny, I got your number...
oursample <- tibble(x = sample(Trump, 100, replace = FALSE))
mean(oursample$x)

#' Let's get a summary of what we did.
oursample %>%
  summarize(sigma = sd(Trump), # sd of the "population"
            mu = mean(Trump), # mean of the "population"
            mean = mean(x), # mean of our sample
            sem = sigma/sqrt(nrow(.)), # standard  error of the sample mean
            lwr = mean - 1.96*sem,
            upr = mean + 1.96*sem) -> oursamplesummaries

oursamplesummaries

#' Here's what we did in that code:
#' We collected the known mean and sigma (standard deviation) from the "population".
#' We collected the mean from our random sample.
#' We calculated the standard error of the sample mean (sem). It equals sigma over the square root of sample observations.
#'  We calculated the 95% confidence interval around the sample mean, given what we learned about the normal distribution above.
#'  This is the interval in which 95% of all possible sample estimates will fall by chance.
#'  If we took 100 samples of n = 100, 95 of those random samples on average would have sample means between lwr and upr.
#'  We're not saying, for the moment, that's where the true population mean is. We don't necessarily know that.
#'  But even this has some nice properties.

#' Question: how likely is was our sample mean, given the actual population mean? 
#' You can answer this question by getting the z value, which communicates distance from a proposed mu.
#'  Here, that's the actual mu, which we know.
#'  Then, you can calculate the probability of observing that z-value.

oursamplesummaries %>%
  mutate(z = (mean - mu)/sem,
         p = 1-pnorm(abs(z)))

#' Here's how you'd interpret this: if mu were actually ~42.4 (or whatever exact value it is), the probability of
#' that we observed our x-bar of 43.4 (rounded in the output you see) is around .399.
#' That's kind of a probable result. Our sample mean is obviously not the population mean, but it's in orbit and a probabilistic draw from the population.

#' What if we were to test against a claim that Trump's thermometer rating is actually 73.88?
#' Source: this is the vote share he got county-wide (Pickens, SC) in 2016.
#' To be clear, vote share != thermometer rating, but let's have fun with it.

oursamplesummaries %>%
  mutate(z = (mean - 73.88)/sem,
         p = 1-pnorm(abs(z)))

#' Here's how you'd interpret this: if the true population mean were 73.88 (which it clearly isn't), the sample mean we got is
#' almost 8 standard errors (!) away from it. The probability we observed it is something like .0000000000000002220446.
#' We can reject the claim that 73.88 is the population mean with a fair bit of confidence. In fact, we know it's not.
#' 
#' What about 50? Source: Trump would brag about 50% approval rating, when he can find a poll that shows it, when no other president would brag about that.
#' You can do the same thing, with same caveat that approval rating != thermometer rating, but alas...
# 
oursamplesummaries %>%
  mutate(z = (mean - 50)/sem,
         p = 1-pnorm(abs(z)))

#' The sample statistic we got suggests that, if Trump's thermometer rating is truly 50, the probability of us observing the sample mean
#' we got is .0449. That'd be a fluke draw. So, we should reject the claim that Trump's thermometer rating is 50 (in fact, we know it's not) 
#' and suggest our sample mean is closer to what it actually is. Which is true. We've been playing god this entire time.
#' 
#' 
#' To this point, we've been assuming we know the population sigma, if not the mu, in calculating our standard error of the sample mean.
#' This is a bit unrealistic, obviously. To know sigma is to also know mu. What about when you don't know the population parameters at all? Student's t-distribution is your answer.
#' Student's t-distribution looks like the normal distribution, but with fatter tails for fewer degrees of freedom
#' 
#' So, let's test against a hypothetical claim of 50/50.
# 
oursample %>%
  summarize(mean = mean(x),
            sem = sd(x)/sqrt(nrow(.)), # notice the numerator
            lwr = mean - 1.96*sem,
            upr = mean + 1.96*sem,
            t = (mean - 50)/sem,
            p = pt(t, nrow(.)))

#' Notice the results look very similar to  what we just did? It's because the t-distribution converges on the 
#' normal distribution with more degrees of freedom (loosely: more observations, after considering parameters to be estimated).
#' More formally: degrees of freedom = number of obs. (n) - parameters to be estimated (k). Here, we just want a simple mean (k = 1).
# 
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dt, args = list(df = 1), color="#026CCBFF") +
  stat_function(fun = dt, args = list(df = 10), color="#F51E02FF") +
  stat_function(fun = dt, args = list(df = 1000), color="#05B102FF") +
  stat_function(fun = dt, args = list(df = Inf), color="#FB9F53FF") +
  stat_function(fun = dnorm, color="black")
