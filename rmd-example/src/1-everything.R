# Your plan of attack should be something like this for bigger projects.
# 1. Have a .R file for loading raw data from somewhere.
# 2. Have a .R file for cleaning raw data you just loaded into data objects (to save to data/)
# 3. Have a .R file for analyzing raw data (i.e. your regression). Save model to data/ when done.
# 4. Have another .R file for doing post-estimation stuff.
# 5. Finally, have a .R file for creatings tables and figures
# Think of this for larger projects, but this is simple, so we'll keep it minimal.



# 1. Load libraries/data -----
# See the five dashes? After four dashes, RStudio thinks of it as kind of a section/bookmark. Look at the bottom left of this pane.
# It'll allow you to jump to different sections more easily.

library(tidyverse)
library(stevemisc)
library(post8000r) # We want the TV16 data because why not
library(modelr)
library(huxtable)

# 2. clean data ----- 
# Again, this is a simple job.
# See here: http://svmiller.com/blog/2020/04/post-estimation-simulation-trump-vote-midwest/

TV16 %>%
  # We want just the white respondents
  filter(racef == "White") %>%
  # We want just the white respondents in these five states
  filter(state %in% c("Indiana","Ohio","Pennsylvania","Wisconsin","Michigan")) %>%
  # If it's one of these variables, apply the r2sd() function to it
  mutate_at(vars("age", "famincr","pid7na","ideo","lcograc","lemprac"),
            list(z = ~r2sd(.))) %>%
  # Make the suffix a prefix.
  rename_at(vars(contains("_z")),
            ~paste("z", gsub("_z", "", .), sep = "_") ) -> Data

# ^ Notice: you have raw data with original columns. You want to create new columns and new data objects.
# Save your object. I recommend serial compression. This is native to R and it's quite useful. You'll want it for bigger data frames.

saveRDS(Data, "data/data.rds")


# 3. Analyze data -----
# Again: simple job. For more complex stuff, think of more flexible list-based solutions.
# See: github.com/svmiller/earr, github/svmiller/woi

M1 <- glm(votetrump ~ age + female + collegeed + famincr +
            pid7na + ideo + bornagain + lcograc + lemprac,
          data = Data, family=binomial(link="logit"), na.action=na.exclude)

M2 <- glm(votetrump ~ z_age + female + collegeed + z_famincr +
            z_pid7na + z_ideo + bornagain + z_lcograc + z_lemprac,
          data = Data, family=binomial(link="logit"), na.action=na.exclude)

list("M1" = M1,
     "M2" = M2) -> Models

saveRDS(Models, "data/models.rds")

# 4. Simulation -----
# Recall: you want quantities of interest. Let's create some.

# *  Let's get some simulations on the effect of being born again Christian ----
# ^ Notice the asterisk? It's a novel way of thinking you have kind of a subsection of your code.
# Again, check the bottom-left pane.


Data %>% # Note: I could recode the scaled stuff to be 0 but modelr will just return medians
  data_grid(.model = M2, state=0, female = 0, votetrump=0, bornagain=c(0,1)) -> newdatBA

simsM2a <- get_sims(M2, newdatBA, 1000, 8675309)

newdatBA %>%
  slice(rep(row_number(), 1000)) %>%
  # we want just the repeated bornagain variables
  select(bornagain) %>%
  # bind_cols()
  bind_cols(simsM2a, .) %>%
  # convert logit to probability
  mutate(y = plogis(y)) -> simsM2a

# * Now, let's get the effect of partisanship and cognitive racism. -----

Data %>% # Note: I could recode the scaled stuff to be 0 but modelr will just return medians
  data_grid(.model = M2, female = 0, votetrump=0,
            z_pid7na = unique(z_pid7na),
            z_lcograc = c(0, 1)) %>%
  na.omit %>% arrange(z_lcograc, z_pid7na) -> newdatRP # racism and partisanship

# Notice it's arranged by cognitive racism and partisanship

simsM2b <- get_sims(M2, newdatRP, 1000, 8675309)

newdatRP %>%
  # repeat this data frame how many times we did simulations
  slice(rep(row_number(), 1000)) %>%
  bind_cols(simsM2b, .) %>%
  # convert logit to probability
  mutate(y = plogis(y)) %>%
  select(y, sim, z_pid7na, z_lcograc) -> simsM2b

# Save...

list("simsM2a" = simsM2a,
     "simsM2b" = simsM2b) -> Sims

saveRDS(Sims, "data/sims.rds")

# 5. Create tables and figures -----
# I'm lazy and I hate huxtable...

# * Table: Regression  -----
huxreg(M2,
       coefs = c("Age" = "z_age",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Household Income" = "z_famincr",
                 "Partisanship (D to R)" = "z_pid7na",
                 "Ideology (L to C)" = "z_ideo",
                 "Born-Again Christian" = "bornagain",
                 "Cognitive Racism" = "z_lcograc",
                 "Empathetic Racism" = "z_lemprac")) %>%
  set_caption('A Regression Table') -> tab_reg

# * Table: first diffs for born-again Christians ----

simsM2a %>%
  # group by sim
  group_by(sim) %>%
  # create first difference, as mutate
  mutate(fd = y - lag(y)) %>%
  # drop NAs, which are basically the bornagain = 0
  # we have what we need anyway
  na.omit %>%
  # select just what we need
  select(y, sim, fd) %>%
  # ungroup
  ungroup() %>%
  # Create mean first difference with 95% intervals around the simulations
  summarize(meanfd = mean(fd),
            lwr = quantile(fd, .025),
            upr = quantile(fd, .975),
            neg = sum(fd < 0)) %>%
  mutate_all(~round(.,3)) -> tab_fd

# * Figure: cognitive racism and partisanship

simsM2b %>%
  mutate(pid7na = rep(seq(1:7), 2000),
         pidcat = recode(pid7na,
                         `1` = "Strong Democrat",
                         `2` = "Not a Strong Democrat",
                         `3` = "Ind., Lean Democrat",
                         `4` = "Independent",
                         `5` = "Ind., Lean Republican",
                         `6` = "Not a Strong Republican",
                         `7` = "Strong Republican"),
         pidcat = fct_inorder(pidcat)) -> simsM2b

simsM2b %>%
  # make it a category
  mutate(z_lcograc = ifelse(z_lcograc == 0, "Average Cognitive Racism", "Two S.D. Increase in Cognitive Racism")) %>%
  group_by(pidcat, z_lcograc) %>%
  summarize(meany = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975)) %>%
  ggplot(.,aes(pidcat, meany, ymin=lwr, ymax=upr, color=z_lcograc, shape=z_lcograc)) +
  geom_hline(yintercept = .5, linetype ="dashed") +
  theme_steve() +
  scale_color_manual(values = c("#377EB8", "#E41A1C")) +
  geom_pointrange(size=.8) +
  labs(color = "", shape = "",
       x = "Level of Partisanship", y = "Expected Probability of Voting for Donald Trump (with 95% Intervals)") -> fig_crp

# I generally encourage keeps tabs and figs in separate files as you progress to more complex projects.

list("tab_reg" = tab_reg,
     "tab_fd" = tab_fd,
     "fig_crp" = fig_crp) -> tabs_figs

saveRDS(tabs_figs, "data/tabs_figs.rds")
