#
# Making the Most of Regression
# -----------------------------------------------------
# Steve Miller
# Date: 9 April 2020


# Let's get started with updating/installing some packages -----
# I've yet to ask that you also install stevemisc, my personal toy package, but I'll ask that you do it.
# Pretty sure you haven't installed these, or updated these, yet, but run the following:
install.packages("arm")
install.packages("modelr")
devtools::install_github("svmiller/stevemisc")
devtools::install_github("svmiller/post8000r")

# Let's load the libraries that I know we'll be using.
# Note, I'm asking to make sure you've installed arm, but we'll call in arm::sim() remotely through stevemisc.

library(tidyverse) # for everything
library(stevemisc) # for get_sims() and r2sd(), devtools::install_github("svmiller/stevemisc")
library(post8000r)
library(modelr) # for data_grid

# NOTE: you can read this blog post here if you want an idea of what we'll be doing here:
# http://svmiller.com/blog/2020/04/post-estimation-simulation-trump-vote-midwest/

# Let's do an analysis of the TV16 data set, much like we did for the week on logistic regression.
# In this case, let's subset the data to white voters in five Midwestern states: IN, MI, OH, PA, and WI.
# These are five states that Obama won in 2008 but Trump won in 2016.

TV16 %>%
  filter(racef == "White") %>%
  filter(state %in% c("Indiana","Ohio","Pennsylvania","Wisconsin","Michigan")) -> Data

# Let's assume we wanted to model the Trump vote in 2016 for white voters in these five states as a function of:
# the respondent's age, whether the respondent is a woman, whether the respondent has a four-year college diploma,
# the household income of the respondent, the partisanship of the respondent (D to R, 7-point scale),
# the respondent's ideology (L to C, 5-point scale), whether the respondent says s/he is a born-again Christian,
# an estimate of the respondent's cognitive racism, and an estimate of the respondent's empathetic racism.

M1 <- glm(votetrump ~ age + female + collegeed + famincr +
            pid7na + ideo + bornagain + lcograc + lemprac,
          data = Data, family=binomial(link="logit"), na.action=na.exclude)

summary(M1)

# Nothing here is terribly surprising. Everything is significant beyond the coefficients for household income and gender.
# Look at this table and you might wonder if cotngitive racism had the largest effect on the Trump vote.
# After all, it has the largest coefficient. Doesn't that mean largest effect?
# The answer here is a clear "no." It should be bloody obvious that partisanship is going to have the largest effect on the vote for Trump.
# Republicans vote for Republicans while not-Republicans are much less likely to vote for Republicans.
# However, that discrete 7-point scale is going to differ from the continuous scale of cognitive racism, which is bound between about -2 and 2.
# Basically: coefficients can't be compared when they're not on a common scale. It's why Gelman recommends scaling by 2 SDs.

# The r2sd() function in my stevemisc package will do this.

Data %>%
  # Select the variables we want to mutate with mutate_at()
  # Apply the function to all of them and make a new column of them.
  mutate_at(vars("age", "famincr","pid7na","ideo","lcograc","lemprac"),
            list(z = ~r2sd(.))) %>%
  # ^ This will create a _z suffix for these new variables.
  # Let's use rename_at() to make those prefixes.
  rename_at(vars(contains("_z")),
            ~paste("z", gsub("_z", "", .), sep = "_") ) -> Data

# Re-run the model.

M2 <- glm(votetrump ~ z_age + female + collegeed + z_famincr +
            z_pid7na + z_ideo + bornagain + z_lcograc + z_lemprac,
          data = Data, family=binomial(link="logit"), na.action=na.exclude)


# It should be no surprise that putting everything on a common scale elevates the effect of partisanship.
# It will invariably be the largest predictor of vote choice in a national election like this.
# Interestingly, it looks like the effect of ideology is roughly similar in scale to cognitive racism as well.


# Next, let's create some quantities of interest. Here, we can do anything you'd like.
# Let's focus on age first for a simple reason.
# Namely, we can think of age as intuitively interval and communicating means from it makes sense.
# However, scaling age and using the scaled variable in a regression creates some confusion to the researcher as to what age corresponds with z_age.
# Let's show how you can backtrack a bit.

# First, let's pitch a quantity of interest of the expected probability of voting for Trump by the typical age of each generation.

Data %>%
  group_by(generation) %>%
  summarize(medianage = median(age)) %>% na.omit %>% arrange(medianage) %>% pull(medianage) -> genmedage

# Basically: the typical Gen Zer is 19 in this sample. The typical Millennial is 29.
# The typical Gen Xer is 44. The typical Boomer is 60. The typical Greatest or Silent is 76.
genmedage

# Now, let's find the z_ages that coincide with those because we included z_age in the model, not age. 
Data %>%
  # find ages that match what we just recorded in genmedage
  filter(age %in% genmedage) %>%
  # find the distinct z_ages that coincide with those, arrange, and extract as a vector.
  distinct(z_age) %>% arrange(z_age) %>% pull(z_age) -> z_genmedage


genmedage
z_genmedage

# Now, let's use modelr's data_grid() to create a hypothetical set of observations with unique z_ages.

Data %>%
  data_grid(.model = M2, z_age = z_genmedage, votetrump = 0) -> newdat

# ^ Here's what this did. We created five rows of identical respondents.
# Basically, by default, data_grid sets everything at the median observation unless you tell it otherwise.
# Note: the get_sims() function will want to see the DV as well, even if it's irrelevant to the simulation.
# Just make sure a column name matching the DV is in there.
# So, what we did here was create a data_grid from Data, where we leaned on the covariates in the model (M2)
# and created five rows of identical observations, save for z_age.
# In this case, z_age values are the median age for each of the five generations

newdat

# It would help, for clarity sake, to say what is the actual age that coincides with z_age

newdat %>% bind_cols(., tibble(age = genmedage)) %>%
  select(age, z_age, everything()) -> newdat


# Now, let's use the get_sims() function in my stevemisc package to create some simulations
# These simulations, which lean on the arm::sim() function quietly, are drawn from a multivariate normal distribution where
# we're relying on the model's betas and covariance matrix to create some random draws as a posterior distribution, if you will.
# the get_sims() function is a wrapper that does this for you and creates a tidy output.

# Note: the function asks for, in order: the model (M2), the newdata of hypothetical observations (newdat),
# how many simulations you want (1,000 is considered default), and, finally, a reproducible seed.
# A reproducible seed is optional, but good manners.
simsAge <- get_sims(M2, newdat, 1000, 8675309) # Jenny, I got your number.

simsAge %>%
  # Remember, these are logits. Let's inverse those real quick
  mutate(y = plogis(y)) -> simsAge

# ^ Notice this is 5,000 rows and two columns. 
# The column y was a simulated natural logged odds of voting for Trump. Now it's a probability.
# The sim column tells you which simulation it is.
# Notice how sim == 1 has five rows? Remember you had five rows of newdat?
# That means the first row in sim == 1 is the simulated logit of voting for Trump for an 18-year old
# The second row in sim == 1 is the simulated logit of voting for Trump for a 29-year old, etc.
# The only caveat comes in noting two things: 1) There are 1,000 simulations, so sim == 1 is just one of those 1,000 for 18-year olds, etc.
# 2) You, the researcher, have to remember what you're looking at.

# But, you could make it easy for yourself to match those simulations to the values of x (here: age).

newdat %>%
  # we really just want age
  select(age) %>%
  # Repeate these 5 rows however many times we did the simulations (here: 1000)
  slice(rep(row_number(), 1000))  %>%
  # bind them together
  bind_cols(simsAge, .) -> simsAge

simsAge # voila
# ^ even though the model technically used z_age, we just wanted to match z_age with age.

# Now, let's prepare a summary of this.
# Here's where I reiterate that cool thing about simulations is that the world is your oyster. You can prepare this info any way you'd like.
# For example, here's a raw summary

simsAge %>%
  # Remember, you want to summarize simulations. Group by the age (or value of whatever x you want)
  group_by(age) %>%
  # Let's create 95% intervals
  summarize(meany = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975))

# You can also prepare this graphically.

simsAge %>%
  group_by(age) %>%
  summarize(meany = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975)) %>%
  # ^ same basic summary
  # Below: treat age as a factor. Trust me, it'll be more readable
  ggplot(.,aes(as.factor(age), meany, ymin=lwr, ymax=upr)) +
  theme_steve() +
  # geom_pointrange() creates point summary with those ymin and ymax
  geom_pointrange() +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_discrete(labels = c("Typical Gen Z Age (19)", "Typical Millennial Age (29)",
                              "Typical Gen X Age (44)", "Typical Boomer Age (60)",
                              "Typical Greatest/Silent Age (76)")) +
  coord_flip() +
  labs(x = "Age", y = "Simulated Probability of Voting for Trump (with 95% Intervals)")

# I'm piqued by this potential claim that the effect of racism in explaining the Trump vote might be equivalent to the effect of ideology.
# Model 2 at least implied that, or suggested it, with standardized coefficients. How might we tease that out in simulation?

Data %>%
  distinct(ideo, z_ideo) %>% na.omit %>% arrange(ideo)

# ^ this grabbed my eye. Notice the effect of going from a 2 to 4 is almost a change of two standard deviations?
# i.e. the difference between the two is almost 1. So: about 2 SDs.
# For those curious, a 2 is a "liberal" and a 4 is a "conservative." Let's pull those.


Data %>% filter(ideo %in% c(2,4)) %>%
  distinct(z_ideo) %>%  arrange(z_ideo) %>% pull(z_ideo) -> z_ideos


# I'm going to have to get creative with these newdats, but let's create one for these two ideology values.

Data %>%
  data_grid(.model = M2, z_ideo = z_ideos, votetrump = 0) -> newdat_ideo


# The difference is almost 1, but let's be precise

diffzideo <- z_ideos[2] - z_ideos[1]
diffzideo

# Now, let's create another newdat frame. This will be for hypothetical values of z_lcograc.
# One is zero (i.e. the mean). The other value is the difference in scale on ideology.
# Because lcograc is interval-continous, this is a plausible value by itself and coincides with an identical increase in scale of ideology

Data %>%
  data_grid(.model = M2, z_lcograc = c(0,diffzideo), votetrump = 0) %>% # let's just bind the two together while we're here.
  bind_rows(newdat_ideo, .) -> newdat

# ^ so, in this object, the first two rows have different values of ideology. Everything else is constant.
# In the second two rows, there are different values of cognitive racism. Everything else is constant.

# Let's get some sims.

simsIR <- get_sims(M2, newdat, 1000, 8675309) 

simsIR
# ^ We could add in the newdat frame like we did previously, but that's less useful in this case, I think.
# Remember the second point I mentioned above about your sims. You gotta know what you're looking at.
# So let's manually create an identifier.

simsIR %>%
  mutate(indicator = rep(c("Ideology Change (L to C)","Ideology Change (L to C)",
                           "Identical Cognitive Racism Change","Identical Cognitive Racism Change"), 1000)) -> simsIR

# Now, do some group_by summarizing. Let's do first differences here instead.
simsIR %>%
  # Convert y to prob
  mutate(y = plogis(y)) %>%
  # Group by sims as well to get first diffs.
  group_by(sim, indicator) %>%
  mutate(fd = y - lag(y)) %>%
  na.omit %>% #  drop NAs, which are basically the lower category. we have what we need
  # practice safe group_by
  ungroup() %>%
  # group_by() again
  group_by(indicator) %>%
  summarize(meanfd = mean(fd),
            lwr = quantile(fd, .025),
            upr = quantile(fd, .975)) %>%
  ggplot(.,aes(indicator, meanfd, ymin=lwr, ymax=upr)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_steve() +
  geom_pointrange() + coord_flip() +
  labs(x = "Indicator",
       y = "Mean First Differences (with 95% Intervals)") 

# ^ takeaway: as a point estimate, cognitive racism is having a larger magnitude effect than ideology when precisely adjusted for scale.
# The point estimate is bigger and, incidentally, it's lower bound is above the *point* estimate for ideology.
# However, ideology's upper bound overlaps cognitive racism considerably. It's estimated first difference is more diffuse.
# You can't say for certain that cognitive racism had a larger effect, but the estimate is larger and more precise.
  
Data %>% filter(is.na(generation))
