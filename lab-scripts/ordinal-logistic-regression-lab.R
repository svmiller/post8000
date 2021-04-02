#' ---
#' title: "Ordinal Logistic Regression"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 1 April 2021
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
#' I'm pretty sure we've yet to install the `{ordinal}` package, which I think is the best R package for handling ordinal models of all walks. The `{MASS}` package has a
#' `polr()` function, and lots of other goodies, but a few things in it conflict with my preferred workflow. Plus, I think `{ordinal}` just has more goodies for ordinal models.
#' I've already installed it, but let's wrap it in a simple function that will install it on your end if you've yet to install it. As always, `{tidyverse}` has our main
#' workflow functions and `{stevedata}` has our data.

if_not_install <- function(packages) {
  new_pack <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_pack)) install.packages(new_pack)
}

if_not_install(c("ordinal", "tidyverse", "stevedata"))

#' Let's load the packages now.

library(tidyverse, quietly = TRUE)
library(stevedata)
library(ordinal) # Notice the function conflict here. The price of doing business

#' # Attitudes About Spending in the GSS
#' 
#' Let's revisit an old data frame from earlier in the semester. You've seen these before. If you don't remember, they're attitudes recorded in the 2018
#' General Social Survey about attitudes toward various spending programs in the United States. The bulk of the spending items are coded such that -1 = the
#' respondent thinks we're spending too much on this particular topic, 0 = the respondent thinks the U.S. is spending "about (the) right" amount, and 1 = the
#' responding thinks the country is spending too little on the topic. Conceptually, I think of these items as communicating attitudes about support for more
#' spending on an ordered categorical scale. Higher values = a respondent implicitly thinking the U.S. should spend more on these topics.

gss_spending
?gss_spending

#' # Estimating an Ordinal Logistic Regression Model
#' 
#' 
#' Last year's script, in the interest of full disclosure, did this kind of stream of consciousness. So, I already know what kind of weirdness I can 
#' expect from these models. No matter, let's plow forward. First, I want to do some recoding here. Let's create a simple dummy variable that
#' recodes the `degree` variable into a new one for those with a four-year college diploma. Let's make sure we drop the other party supporters 
#' from the partisanship (`pid7`) variable. Finally, and this is important, let's declare some of these spending prompts to be ordered-categorical
#' variables with the `ordered()` function that comes in base R. We'll focus on two prompts---attitudes
#' toward spending on welfare (`natfare`) and attitudes about spending on social security (`natsoc`)---and 1) coerce both to be ordered-categorical
#' variables and 2) add them together and coerce *that* to be an ordered-categorical variable. Basically, the `{ordinal}` functions require a
#' dependent variable that is explicitly declared beforehand as an ordered factor. If you don't do this, the errors you get from the `{ordinal}` 
#' function won't quite point you in this direction.

gss_spending %>%
  mutate(collegeed = ifelse(degree >= 3, 1, 0),
         pid7 = ifelse(partyid == 7, NA, partyid),
         natfaresoc = natsoc + natfare,
         natfare_f = ordered(natfare),
         natsoc_f = ordered(natsoc),
         natfaresoc_f = ordered(natfaresoc)) -> gss_spending



#' Let's assume we want to model attitudes toward welfare spending among white people as a function of these things:
#' age, sex (whether respondent is a woman), college education, income, partisanship (D to R), and ideology (L to C). You'd
#' do this with the `clm()` function in the `{ordinal}` package even as the syntax you see looks like every other regression
#' model you'd estimate in R.

M1 <- clm(natfare_f ~ age + sex + collegeed + rincom16 +
            pid7 + polviews, data=subset(gss_spending, race == 1))

summary(M1)

#' Remember: interpreting coefficients in your "first step" is functionally identical to what you'd do from an OLS model. That is,
#' you're looking for sign of coefficients and statistical significance. Here would be the preliminary takeaways, none of which are
#' particularly surprising beyond the gender effect. In these data, the statistically significant effects are for women, ideology,
#' and partisanship and all three are negative. Women are less likely than me to think about spending more on welfare. The ideology
#' and partisanship effects are unsurprising if you have at least a cursory understanding on American politics.
#' 
#' I tend to use very general language on coefficient interpretation for ordinal models, but if you want something more exact, here it is.
#' Observe the coefficient for `polviews` is ~-.269, which, you'll recall, is a logit.
#' Thus, the natural logged odds of observing a 1 versus a 0 or -1 decreases by about -.269 for a unit increase in the `polviews` variable.
#' Related: the natural logged odds of observing a 0 versus a -1 decreases by about -.269 for a unit increase in the `polviews` variable.
#' 
#' ## A Love/Hate Comment on Thresholds in Ordinal Models
#' 
#' I'm generally loathe to talk about these things. They're not typically parameters of interest for how you're probably using an ordinal model.
#' However, you'll want to provide them anyway. These thresholds or "cut points" are natural logged odds between two variables.
#' So, in this case: the "coefficient" reading -1|0 is the natural logged odds of being a -1 versus a 0 or 1.
#' The "coefficient" reading 0|1 is the natural logged odds of being a -1 or 0 versus a 1.
#' The "|" is kind of misleading, especially if you're used to it as a strict logical operator.
#' In this case, the "|" is like a cumulative cut point, or a way of saying it is.
#'  
#' Let's talk a bit about what's happening here. We call ordinal logistic regression an extension of (binary) logistic regression because:
#' 
#' 1. it's in spirit *multiple* (binary) logistic regressions of
#' 2. the natural logged odds of appearing in a category or below it.
#' 
#' However, we are assuming the lines are in parallel to each other, separated by the thresholds. So, in this case, think of this model
#' as kind of like two logistic regressions, each with identical betas. `logit(p(y == -1)) = -2.87 + B*X` and `logit(p(y <= 0)) = -1.4221 + B*X`.
#' 
#' ## Assessing the Proportional Odds Assumption
#' 
#' You should, at least in spirit, care about the proportional odds assumption that the slopes are the same at every level.
#' There are any number of ways of testing this and I *really* wish there was a Brant test add-on for the `{ordinal}` package. There isn't 
#' (i.e. it's there for the `polr()` function in `{MASS}`, which I eschewed here).
#' 
#' Instead, you can do a nominal test, which is the `{ordinal}` package's way of saying "likelihood ratio test."
#' Think of this as a test of the hypothesis that relaxing the proportional odds (PO) assumption of parallel lines across all levels 
#' of the response provides a better model fit. If the p < .05, you reject the hypothesis that relaxing the PO assumption does not improve model fit.
#' In other words, one or more of the covariates may have non-constant effects at all levels.

nominal_test(M1)

#' You can interpret the above output in a few ways:
#' 
#' 1. You can use this as a call for a multinomial model. This might even be advisable in this context. Basically, while my brain sees these variables as
#' three-item ordered factors communicating implicit support for more government spending on this topic, the truth is there aren't many categories in the response.
#' Thus, it might be advisable to fit a multinomial logit model (i.e. the GLM for nominal dependent variables) because this is really an unstructured response with just three
#' categories awkwardly given to the respondent. We won't discuss the multinomial logit model 
#' in class---the truth is I rarely see it in published work---but the model estimates the natural logged odds of being in one category versus some other 
#' "baseline" response. Maybe it makes sense, in this context, to have the "about rights" as the baseline and assess the natural logged odds of being a 
#' "too little" versus an "about right" or a "too much" versus an "about right."
#' 2. Alternatively, you can allow the effects of those coefficients that the nominal test flagged to vary at all levels. You can do this
#' by specifying a nominal call in the `clm()` function. Here, we'll do it just for age and sex.

M2 <- clm(natfare_f ~ collegeed + rincom16 + pid7 + polviews, nominal = ~ age + sex, data=subset(gss_spending, race == 1))
summary(M2) # Notice there's no single coefficient for age and sex. It's in the intercepts/thresholds.

nominal_test(M2)

#' Now, however, the nominal test is complaining about the college education variable. At this point, I'm probably just going to throw up my hands and say
#' "multinomial model" for this variable. But, as I mentioned at the top of the script, I wrote this kind of stream of consciousness and I suspect
#' it's the few response categories we have that's causing this issue. So, let's take out that `natfare_f` variable and add in the prompt that sums both
#' `natfare` and `natsoc` together. This creates a five-item variable where -2 = those who think we're spending too much on both welfare and social security
#' and 2 = those who think we're spending too little on both. -1, 0, and 1 are also both possible.
#' 
#' Here'd be the breakdown for those.

gss_spending %>%
  filter(race == 1) %>%
  distinct(natfaresoc, natfare, natsoc) %>%
  na.omit %>%
  arrange(natfaresoc)

#' Now let's try this again with this new dependent variable that amounts to an index sentiment on whether the respondent thinks the U.S. needs to spend
#' more on social welfare programs, here gauged by prompts on welfare and social security.

# Let's try this again
M3 <- clm(natfaresoc_f ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1))
summary(M3)

#' We do see there's no longer a significant gender difference, but the college education variable emerges as negative and statistically significant.
#' The ideology and partisanship variables are the same, basically. More values in the dependent variable mean
#' there are more thresholds through we must sift. However, we're here for the nominal test. Did we pass?

nominal_test(M3)

#' Much betta. 
#' 
#' ![](https://64.media.tumblr.com/6437f1bc98d5d0952a1edd19b9e4241e/1932ca80ea201e4f-5d/s640x960/a558c99f1fa3f6d0377ccfc48966917a8a94c8f2.gif)
#' 
#' ## Imposing Your Will on the Ordinal Model
#' 
#' `r emo::ji("fire")` #take coming up: I'm of the mentality you should always run an ordinal logistic regression if that's the DV you're handed.
#' I will throw something at you if you try running an OLS on a five-item Likert because that's just not the data you have.
#' But I kind of hate them, and I would forgive you for hating them too, because communicating them is a chore.
#' OLS has a straightforward interpretation. Binary DVs are really straightforward as well.
#' However, the PO assumption can be restrictive and there are a lot of moving pieces from the model output. Your audience may not have the appetite for it.
#' 
#' In other words, be prepared to communicate your statistical model graphically and impose your will on a somewhat unruly model accordingly.
#' 
#' In the `{ordinal}` package, you can do this with the `predict()` function and think about using it with hypothetical data. For example,
#' let's create a simple data frame that has all our right-hand side values at their typical values. But, we'll allow partisanship to
#' vary across three types. These will be the strong Democrats (`pid7 == 0`), the pure independents who say they don't lean one way or the
#' other (`pid7 == 3`), and the strong Republicans (`pid7 == 6`).

newdat <- tibble(age = median(gss_spending$age, na.rm=T),
                 collegeed = 0,
                 sex = 0,
                 pid7 = c(0, 3, 6),
                 polviews = median(gss_spending$polviews, na.rm=T),
                 rincom16 = median(gss_spending$rincom16, na.rm=T))

newdat # who dis

#' Thus, think of three types of people: a typical-aged man of average income, without a college diploma, and who says they're ideologically moderate. They're identical,
#' but for their partisanship. One is a strong Democrat, another an independent, and the last a Republican. We want to know what the effect of increasing
#' partisanship "looks like" for these three people across the handful of different responses recorded in the dependent variable. For simplicity's sake,
#' we're going to focus on that first model that looked at just attitudes about welfare, even acknowledging the model wasn't a super great fit for the data.
#' 
#' You've been warned: this code is convoluted as hell. It's why I prefer Bayes for ordinal models, but Bayes is in two weeks.
# Oh god, here we go...

predict(M1, newdata = newdat, se.fit=T) %>% # get predictions with standard errors.
  # This is a list of two matrices
  # Let's coerce it to two data frames while also begrudging that I have to do this.
  map(~as.data.frame(.)) %>% # god purrr is awesome
  # There's a hiden rowname in here. It's going to somewhat coincide with the values of pid7
  # Let's extract it
  map(~rownames_to_column(.)) %>%
  # Now let's make these two data frames into one data frame.
  # Importantly, obj is going to tell me whether it's a prediction or a standard error around the prediction
  map2_df(names(.), ~mutate(.x,obj=.y)) %>%
  # alrightie... okay. See that rowname variable? I know that's the pid7 values of 0, 3, and 6.
  # However, the clm predict doesn't save those. Let's tell them for what they are.
  rename(pid7 = rowname) %>%
  # It also delightfully thinks it's a character. So, let's humor it and overwrite it.
  mutate(pid7 = rep(c("Strong Democrat", "Independent", "Strong Republican"), 2),
         # Make it a factor in order it appears. You'll thank me later for this.
         pid7 = forcats::fct_inorder(pid7)) %>%
  # okay, tidyr::gather() is going to have to do some heavy lifting here.
  gather(var, val, -pid7, -obj) %>%
  # Importantly, I needed this longer because I want my -1, 0, and 1s (as responses) to be "long."
  # so, now this made it "longer" while still giving me a glimpse as to what's my fit and what's my se.fit
  # See that's in the obj column? Let's group_split and bind_cols to get them next to each other
  group_split(obj) %>%
  bind_cols() %>%
  # voila! I have everything I need now
  # however, I'll need to rename things and focus on just what I want
  rename(pid7 = `pid7...1`,
         natfare = `var...3`,
         fit = `val...4`,
         se = `val...8`) %>%
  select(pid7, natfare, fit, se) %>%
  # Now, let's have some fun and create a column called upr and lwr creating bounds around the estimate
  mutate(upr = fit + 1.96*se,
         lwr = fit - 1.96*se) %>%
  ggplot(.,aes(pid7, fit, ymax=upr, ymin=lwr)) +
  geom_pointrange() +
  # Oh god help me I never do anything the easy way...
  facet_wrap(~natfare, labeller=labeller(natfare = c("-1" = "Spends Too Much",
                                             "0" = "Spending About Right",
                                             "1" = "Spending Too Little"))) +
  labs(title = "Attitudes Toward Spending on Welfare, by Partisanship",
       x = "Partisanship", y = "Predicted Probability of the Response (with 95% Intervals)",
       caption = "Source: General Social Survey, 2018. Note: for pedagogical use in my grad methods class. Stay out of my mentions.",
       subtitle = "Increasing GOP partisanship increases the likelihood of the spend too much or spend about right response, but decreases the likelihood of the\nspend too little response. You knew this.")


#' ^ Consider this a preview for the quantities of interest week, that's coming up next. Basically: regression modeling is story-telling as well, in a way.
#' You, the story-teller, just have more work to do with ordinal models, even as the ordinal model may faithfully capture the underlying distribution of the DV.
#' 
#' # When Can You Jettison the Ordinal Model for OLS?
#' 
#' 
#' I want to give you an "out", of a kind. The truth is OLS models are a better fit on ordered-categorical data than they are on dummy variables. What follows 
#' will touch on some of the readings you had this week (and even earlier in the semester) on whether you can treat your ordinal DV as continuous. Here's 
#' my rule of thumb:
#' 
#' - **3-4**: basically, no. Don't do it. You have so few responses that the OLS model just isn't going to return a quantity of interest that I or the audience
#' should care to know.
#' - **5-7**: others do this. I don't, but I would say to use the OLS as a "first cut" to assess if there's a "there there", then finish with the ordinal model. Think of the
#' kind of data you have in, say, a five-item ordered categorical variable. Think of a Likert, for example. The ordinal model can tell you, with some work, 
#' the probability of being a "strongly disagree", a "neither agree nor disagree", and a "strongly agree." Those are quantities of interest that kind of present themselves
#' in these applications. The ordinal model can help you with those. The OLS model really can't. The sign and significance may be unchanged, but that's also not the point.
#' - **8+**: f*ck it, just go for it, provided there's no natural clumping of responses on some extreme in the distribution. Here'd be the more thorough interpretation. With
#' more values on a still finite scale, you can start to think of the differences as "equally spaced out" where the observed responses rest on a continuum that makes a bit
#' more sense. The OLS model is still informative, if technically wrong. In our lecture, I showed how it performed okay with simulated data, even if it was discernibly
#' off the true parameters (and that was for a five-item response variable). No one is going to give you too much grief and I won't either, 
#' but you may want to consider some form of robust standard error correction to be safe.
#' 
#' ^ On the above point in the distribution of responses on a granular ordinal scale. Remember the bribe-taking prompt from the World Values Survey? 
#' This was the justifiability of taking a bribe on a 1-10 scale. It has 10 responses, but almost all of them are at 1. In other words, don't treat that as interval below:

wvs_justifbribe %>%
  group_by(f117) %>%
  count() %>%
  na.omit %>%
  ggplot(.,aes(as.factor(f117), n)) +
  geom_bar(stat="identity", alpha=0.8, color="black") +
  scale_x_discrete(labels=c("Never Justifiable", "2", "3", "4",
                            "5", "6", "7", "8", "9", "Always Justifiable")) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label=n), vjust=-.5, colour="black",
            position=position_dodge(.9), size=4) +
  labs(y = "Number of Observations in Particular Response",
       x = "",
       title = "The Justifiability of Taking a Bribe in the World Values Survey, 1981-2016",
       caption = "Data: World Values Survey (1981-2016), via ?wvs_justifbribe in {stevedata}",
       subtitle = "There are just 10 different responses in this variable with a huge right skew.")



#' You may not even want to think of it as ordinal. With noisy as hell data like this, as I mentioned in that session, you'll probably just want to embrace 
#' the noisiness and estimate it as a binary DV of 1 versus not 1.
#' 
#' What about our dependent variable from model 3?

summary(M3)
summary(M4 <- lm(natfaresoc ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1)))

broom::tidy(M3) %>%
  filter(coef.type != "intercept") %>%
  select(-coef.type) %>%
  mutate(model = "Ordinal Logistic Regression") %>%
  bind_rows(., broom::tidy(M4) %>% mutate(model = "OLS")) %>%
  arrange(term)

#' ^ off, technically wrong, but not the worst I've ever seen. In fact, those *t*/*z* statistics look very similar even as the underlying coefficients are
#' being communicated on different scales. I'd still say to jettison OLS for the ordinal logistic regression here. Do it for the reasons I hinted
#' at above. If you have just five responses in the DV, I'm probably going to want to know about the extremes and the middle. There are a lot of moving pieces in an ordinal
#' model, but you can focus on just those responses that almost naturally present themselves in this setup. Those who advocating slapping an OLS sticker on all types
#' insist it does well enough being BLUE. My retort to that is 1) I'm not convinced it's doing that and 2) with so few response categories, OLS is going to
#' struggle in an obvious way providing reasonable fitted values. Ordinal logistic regression is tailored for communicating probabilities (albeit with
#' some work) for finite values. OLS can't do that.
#' 
#' What about something bigger, like the `sumnatsoc` variable in the `gss_spending` data? Whereas Model 3 adds just the two prompts together, this sums
#' all responses toward various "social" prompts about the environment, health, dealing with drug addiction, education, improving racial equality, welfare,
#' roads, mass transit, parks, social security, and child care.
#' 
#' Here's what this variable would look like, all 22 possible responses of it. There's a bit of a left tail for the anti-government-doing-anything folk, but this has
#' a nice juicy center for a variable with just 22 different responses.

gss_spending %>%
  filter(race == 1) %>%
  group_by(sumnatsoc) %>%
  tally() %>%
  ggplot(.,aes(ordered(sumnatsoc), n)) +
  geom_bar(stat="identity")

#' Now, let's compare the ordinal model with the OLS model.

M5 <- clm(ordered(sumnatsoc) ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1))
M6 <- lm(sumnatsoc ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1))

broom::tidy(M5) %>%
  filter(coef.type != "intercept") %>%
  select(-coef.type) %>%
  mutate(model = "Ordinal Logistic Regression") %>%
  bind_rows(., broom::tidy(M6) %>% mutate(model = "OLS")) %>%
  arrange(term)

#' Similar performance.  No one is going to yell too much at you for doing an OLS on a technically ordinal item that has like 22 different values.
#' But, maybe consider some kind of robust standard error correction.
