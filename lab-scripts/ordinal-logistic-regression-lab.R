#
# Ordinal Logistic Regression
# -----------------------------------------------------
# Steve Miller
# Date: 1 April 2020

# Let's get started with updating/installing some packages -----
install.packages("ordinal")

library(tidyverse)
library(post8000r)
library(ordinal)


# Let's revisit an old data frame from earlier in the semester.

gss_spending
?gss_spending

# First, let's do some recoding. 
# collegeed = if respondent has an undergraduate or graduate degree.
# pid7 = recoding that top category of other party supporters to be missing.
# natfare_f: declaring that the natfare variable is an ordered factor.
# You gotta do this for ordinal analyses.

gss_spending %>%
  mutate(collegeed = ifelse(degree >= 3, 1, 0),
         pid7 = ifelse(partyid == 7, NA, partyid),
         natfare_f = ordered(natfare)) -> gss_spending



# Let's assume we want to model attitudes toward welfare spending among white people as a function of these things:
# age, sex (whether respondent is a woman), college education, income, partisanship (D to R), and ideology (L to C).

M2 <- clm(natfare_f ~ age + sex + collegeed + rincom16 + pid7 + polviews, data=subset(gss_spending, race == 1))

summary(M2)
# Prelim takeaways:
# women are less likely than men to think about spending more on welfare.
# Predictable effects of partisanship and ideology.

# I tend to use very general language on coefficient interpretation for ordinal models, but if you want something more exact, here it is.
# Observe the coefficient for polviews is ~-.269, as a logit.
# Thus, the likelihood (natural logged odds) of observing a 1 versus a 0 or -1 decreases by about -.269 for a unit increase in polviews.
# Related: the likelihood (natural logged odds) of observing a 0 versus a -1 decreases by about -.269 for a unit increase in polviews.

# ON THRESHOLDS:
# I'm generally loathe to talk about these things. They're not typically parameters of interest for how you're probably using an ordinal model.
# However, you'll want to provide them anyway.
# These thresholds or "cut points" are natural logged odds between two variables.
# So, in this case: the "coefficient" reading -1|0 is the natural logged odds of being a -1 versus a 0 or 1.
# The "coefficient" reading 0|1 is the natural logged odds of being a -1 or 0 versus a 1.
# The "|" is kind of misleading, especially if you're used to it as a strict logical operator.
# In this case, the "|" is like a cumulative cut point, or a way of saying it is.

# Let's talk a bit about what's happening here. We call ordinal logistic regression an extension of (binary) logistic regression because:
# 1) it's in spirit *multiple* (binary) logistic regressions of
# 2) the natural logged odds of appearing in a category or below it.
# However, we are assuming the lines are in parallel to each other, separated by the thresholds
# So, in this case, think of M2 as kind of like two logistic regressions, each with identical betas.
# logit(p(y == -1)) = -2.87 + B*X and logit(p(y <= 0)) = -1.4221 + B*X.



# You should, at least in spirit, care about the proportional odds assumption that the slopes are the same at every level.
# There are any number of ways of testing this and I *really* wish there was a Brant test add-on for the ordinal package.
# There isn't (i.e. it's there for the polr function in MASS, which I eschewed here).

# Instead, you can do a nominal test, which is the ordinal package's way of saying "likelihood ratio test."
# Think of this as a test of the hypothesis that relaxing the proportional odds (PO) assumption of parallel lines across all levels of the response provides a better model fit.
# If the p < .05, you reject the hypothesis that relaxing the PO assumption does not improve model fit.
# In other words, one or more of the covariates may have non-constant effects at all levels.
nominal_test(M2)

# You can interpret the above in a few ways:
# You can use this as a call for a multinomial model. This might even be advisable in this context.
# You could spin me a ball of yarn that with just three categories, awkwardly given to the respondent, that this is really an unstructured response.
# OR: you can allow the effects to vary at all levels.
# You do this by specifying a nominal call in the clm function.
# Here, we'll do it for just age and sex.

M3 <- clm(natfare_f ~ collegeed + rincom16 + pid7 + polviews, nominal = ~ age + sex, data=subset(gss_spending, race == 1))
summary(M3) # Notice there's no single coefficient for age and sex. It's in the intercepts/thresholds.

nominal_test(M3)

# Here's a better idea, while also upfront confessing I'm doing this stream of consciousness.
# Let's note that the nature of the response (-1, 0, 1) is probably wanting a multinomial solution notwithstanding the order we want to impose on it.
# Instead, let's make an index of three variables: natheal, natfare, and natsoc
# Think of this as an index on attitudes toward social spending (broadly defined). Higher values = more support for more social spending
# (or, technically, that the respondent thinks we're spending too little)

gss_spending %>%
  mutate(y = natheal + natfare + natsoc,
         y_f = ordered(y)) -> gss_spending

# Here's what our variable looks like:
table(gss_spending$y_ord)

# Let's try this again

M4 <- clm(y_ord ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1))
summary(M4)

nominal_test(M4)
# Much betta https://66.media.tumblr.com/6437f1bc98d5d0952a1edd19b9e4241e/1932ca80ea201e4f-5d/s640x960/a558c99f1fa3f6d0377ccfc48966917a8a94c8f2.gif




# So, the probability of being a -3 to a -2:3 is .001, at least in the model.

# HOT #take coming up: I'm of the mentality you should always run an ordinal logistic regression if that's the DV you're handed.
# I will throw something at you if you try running an OLS on a five-item Likert because that's just not the data you have.
# But I kind of hate them, and I would forgive you for hating them too, because communicating them is a chore.
# OLS has a straightforward interpretation. Binary DVs are really straightforward as well.
# However, the PO assumption can be restrictive and there are a lot of moving pieces from the model output.
# Your audience may not have the appetite for it.

# In other words, be prepared to communicate your statistical model graphically.

# In the ordinal package, this is the predict() function and think about using it with hypothetical data.
# For example, let's create a simple data frame that has all our right-hand side values, but we'll have three variables of partisanship.
# These will be the strong Ds (0), pure indies who don't lean one way or another (3), and the strong Rs (6).
# Everything else is at a typical value (a median).

newdat <- tibble(age = median(gss_spending$age, na.rm=T),
                 collegeed = 0,
                 sex = 0,
                 pid7 = c(0, 3, 6),
                 polviews = median(gss_spending$polviews, na.rm=T),
                 rincom16 = median(gss_spending$rincom16, na.rm=T))


# Alrightie, this code is convoluted as hell, and it's why I prefer Bayes for ordinal models.
# But that's in two weeks.
# Oh god, here we go...

predict(M2, newdata = newdat, se.fit=T) %>% # get predictions with standard errors.
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
  # Now, let's have some fun and create a column called upr and lwr creating bounds around the estimate
  rename(fit = val,
         se = val1) %>%
  mutate(upr = fit + 1.96*se,
         lwr = fit - 1.96*se) %>%
  ggplot(.,aes(pid7, fit, ymax=upr, ymin=lwr)) +
  geom_pointrange() +
  # Oh god help me I never do anything the easy way...
  facet_wrap(~var, labeller=labeller(var = c("-1" = "Spends Too Much",
                                             "0" = "Spending About Right",
                                             "1" = "Spending Too Little"))) +
  labs(title = "Attitudes Toward Spending on Welfare, by Partisanship",
       x = "Partisanship", y = "Predicted Probability of the Response (with 95% Intervals)",
       caption = "Source: General Social Survey, 2018. Note: for pedagogical use in my grad methods class. Stay out of my mentions.",
       subtitle = "Increasing partisanship (with the GOP) increases the likelihood of the spend too much or spend about right response, but decreases the likelihood of the\nspend too little response. You knew this.")


# ^ Consider this a preview for the quantities of interest week, that's coming up next.
# Basically: regression modeling is story-telling as well, in a way.
# You, the story-teller, just have more work to do with ordinal models, even as the ordinal model may faithfully capture the underlying distribution of the DV.


# With that in mind, I want to give you an "out" of a kind.
# This will touch on some of the readings you had this week (and even earlier in the semester) on whether you can treat your ordinal DV as continuous.
# My rule of thumb:
# 3-5: hard no.
# 7: I'm listening...
# 10+: f*ck it, just go for it, provided there's no natural clumping of responses on some extreme in the distribution.
# ^ The more thorough interpretation: with more values on a still truncated (ordinal) scale, you can start to think of the differences as "equally spaced out."
# In which case, the OLS model is informative, if technically wrong.
# You'll remember it performed well enough in the lecture in which I explicitly simulated the data, even if it was discernibly off the true parameters.
# No one is going to give you too much grief and I won't either, but you may want to consider some form of heteroskedasticity correction to be safe.

# ^ On the above point in the distribution of responses on a granular ordinal scale. Remember the bribe-taking prompt in the US from the World Values Survey?
# This was the justifiability of taking a bribe on a 1-10 scale.
# It has 10 responses, but almost all of them are at 1.
# In other words, don't treat that as interval below:

usa_justifbribe %>%
  group_by(justifbribe) %>%
  count() %>%
  na.omit %>%
  ggplot(.,aes(as.factor(justifbribe), n)) +
  geom_bar(stat="identity", alpha=0.8, color="black") +
  scale_x_discrete(labels=c("Never Justifiable", "2", "3", "4",
                            "5", "6", "7", "8", "9", "Always Justifiable")) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label=n), vjust=-.5, colour="black",
            position=position_dodge(.9), size=4) +
  labs(y = "Number of Observations in Particular Response",
       x = "",
       title = "The Justifiability of Taking a Bribe in the U.S., 1995-2011",
       caption = "Data: World Values Survey, 1995-2011",
       subtitle = "There are just 10 different responses in this variable with a huge right skew. I wouldn't ask for a mean of this.")

# You may not even want to think of it as ordinal. With noisy as hell data like this, as I mentioned in that session, you'll probably just want to embrace
# the noisiness and estimate it as a binary DV of 1 versus not 1.

# What about in our y variable from model 4?

summary(M4)
summary(M5 <- lm(y ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1)))

broom::tidy(M4)
broom::tidy(M5)

# ^ off, technically wrong, but not so wrong.
# Recall the assumptions of the ordinal model of the underlying latent variable. This is why OLS is performing better here
# than it performed with the binary model.

# What about something bigger, like the sumnatsoc variable?

table(gss_spending$sumnatsoc)

summary(M6 <- clm(ordered(sumnatsoc) ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1)))
summary(M7 <- lm(sumnatsoc ~ age + sex + collegeed + rincom16 + pid7 + polviews,  data=subset(gss_spending, race == 1)))

# Similar performance.  No one is going to yell too much at you for doing an OLS on a technically ordinal item that has like 22 different values.
# But, maybe consider some kind of heteroskedasticity correction.
