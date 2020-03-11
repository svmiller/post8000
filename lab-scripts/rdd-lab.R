#
# Regression Discontinuity Design
# -----------------------------------------------------
# Steve Miller
# Date: 11 March 2020

# Let's get started with updating/installing some packages -----
devtools::install_github("svmiller/post8000r")

# Did I make you install modelr and infer yet? I forget. If not, install it.
install.packages("modelr")
install.packages("infer")

# Load libraries
library(tidyverse)
library(post8000r)
library(modelr)
library(infer)
library(lmtest)

# Part 1: Incumbency Advantages in Senate Elections ----
# ------------------------------------------------------

# Let's start with the Cattaneo et al. (2015) data, which is on Senate elections.
# In their case, they want to look at party advantages in the U.S. Senate.
# i.e. does a victory in the previous election help the incumbent (in this case, Democrat) in the next election?
# In these data, `vote` is the election result in six years from the observation. `margin` is the vote in that year's election.

Cattaneoetal15
?Cattaneoetal15

# We could try to run a correlation.
cor.test(Cattaneoetal15$margin, Cattaneoetal15$vote)
# ^ pretty strong correlation. Not terribly surprising.
# Yet, that doesn't tell us a causal effect. Just an obvious correlation.


# We could try a bivariate regression.
summary(M1 <- lm(vote ~ margin, data=Cattaneoetal15))
# ^ Seems to indicate that one-unit increase in margin of victory at time point t coincides with an
# estimated increase of .396 or so on the next election.
# Alas, that's still insufficient.

# * Now, let's visualize the data graphically (Do you understand what those corner observations are?)
Cattaneoetal15 %>%
  ggplot(.,aes(vote, margin)) + geom_point()

# We have a `treatment` variable that equals 1 if margin > 0, and is 0 if margin < 0.
# We could try this with another regression

summary(M2 <- lm(vote ~ margin + treatment, data=Cattaneoetal15))
# ^ It suggests the treatment effect is a 4.784-point advantage in the next election.

# Importantly: create an interaction term but, before you do, *center the margin of vote variable*.
# Yes, not only should your constituent terms have a naturally occurring zero, but it should be the mean too.

Cattaneoetal15 %>%
  mutate(c_margin = margin - mean(margin)) -> Cattaneoetal15

# Interact them
summary(M3 <- lm(vote ~ c_margin*treatment, data=Cattaneoetal15))
# ^ Keep a note on those coefficients above

# You could look at this with a margins plot, but I have a better idea.

# This will create a new tibble where each observation of margin is paired with each possible value of treatment (i.e. 0, 1).
Cattaneoetal15 %>%
  data_grid(c_margin, treatment) -> newdat

# Now, let's create some model predictions
preds <- predict(M3, newdat, interval="confidence")

# bind 'em together
cbind(newdat, preds) %>%
  tbl_df() %>%
  mutate(Category = ifelse(treatment == 0, "Treatment = 0", "Treatment = 1")) -> newdat

newdat %>%
  ggplot(.,aes(c_margin, fit, ymin=lwr, ymax=upr, linetype=Category, fill=Category)) +
  geom_line() +
  geom_ribbon(alpha=0.2, color="black") +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(-100, 100, by=10))


# Now, instead of comparing all observations in the data set, we're going to use the
# intuition behind RDD and look at just the close races.

Cattaneoetal15 %>%
  mutate(Category = ifelse(treatment == 0, "Treatment = 0", "Treatment = 1")) %>%
  filter(abs(margin) < 100) %>%
  ggplot(.,aes(margin, vote, linetype = Category, color=Category)) +
  geom_point() + geom_smooth(method="lm", color="black")


# * Now, let's "zoom in" the graph. We are selecting elections decided by a margin of vote smaller than 10%. Can you see the differences?

Cattaneoetal15 %>%
  mutate(Category = ifelse(treatment == 0, "Treatment = 0", "Treatment = 1")) %>%
  filter(abs(margin) < 10) %>%
  ggplot(.,aes(margin, vote, linetype = Category, color=Category)) +
  geom_point() + geom_smooth(method="lm", color="black")

# Enhance...
Cattaneoetal15 %>%
  mutate(Category = ifelse(treatment == 0, "Treatment = 0", "Treatment = 1")) %>%
  filter(abs(margin) < 5) %>%
  ggplot(.,aes(margin, vote, linetype = Category, color=Category)) +
  geom_point() + geom_smooth(method="lm", color="black")

# Enhance, one more time.
Cattaneoetal15 %>%
  mutate(Category = ifelse(treatment == 0, "Treatment = 0", "Treatment = 1")) %>%
  filter(abs(margin) < 1) %>%
  ggplot(.,aes(margin, vote, linetype = Category, color=Category)) +
  geom_point() + geom_smooth(method="lm", color="black")

# Some methodologists argue that if your problem is well designed and you are
# actually selecting observations close to the threshold, then a simple difference
# of mean is enough to identify the causal effect of the treatment.
#   * Take a look at the regression and the t-test below.
# *Can you repeat them restricting the margin of vote?

Cattaneoetal15 %>%
  filter(abs(margin) < 100) %>%
  t_test(formula = vote ~ treatment, alternative = "two_sided",
         order = c("0", "1"))

Cattaneoetal15 %>%
  filter(abs(margin) < 10) %>%
  t_test(formula = vote ~ treatment, alternative = "two_sided",
         order = c("0", "1"))

Cattaneoetal15 %>%
  filter(abs(margin) < 5) %>%
  t_test(formula = vote ~ treatment, alternative = "two_sided",
         order = c("0", "1"))

Cattaneoetal15 %>%
  filter(abs(margin) < 1) %>%
  t_test(formula = vote ~ treatment, alternative = "two_sided",
         order = c("0", "1"))

# /* One of the problems that methodologists point out is that we are never close
# enought to the threshold, so we usually can not be so confident about the difference
# of means' estimates. The alternatives to that are: */

summary(M4 <- lm(vote ~ treatment, data=subset(Cattaneoetal15, abs(margin) < 100)))
summary(M5 <- lm(vote ~ treatment, data=subset(Cattaneoetal15, abs(margin) < 10)))
summary(M6 <- lm(vote ~ treatment, data=subset(Cattaneoetal15, abs(margin) < 5)))
summary(M7 <- lm(vote ~ treatment, data=subset(Cattaneoetal15, abs(margin) < 1)))

# /* ...Or to use local linear regression models with an interactive term between
# the treatment dummy and the running variable: */
summary(M8 <- lm(vote ~ treatment*c_margin, data = subset(Cattaneoetal15, abs(margin) < 10)))
# Meh, let's just overwrite them
summary(M9 <- lm(vote ~ treatment*c_margin, data = subset(Cattaneoetal15, abs(margin) < 5)))


# * Or a high order local polynomial regression:
# Oof, why...

Cattaneoetal15 %>%
  mutate(margin2 = margin^2,
         margin3 = margin^3,
         margin4 = margin^4) -> Cattaneoetal15

summary(M10 <- lm(vote ~ treatment + margin + margin2 + margin3 + margin4, data=subset(Cattaneoetal15, abs(margin) < 5)))

Cattaneoetal15 %>%
  filter(abs(margin) < 5) %>%
  ggplot(.,aes(margin, vote)) + geom_smooth()

# * Or still a high order local polinomial regression with interactive terms:
# Oof, why...

summary(M11  <- lm(vote ~ treatment + margin*treatment + margin2*treatment + margin3*treatment + margin4*treatment, data=subset(Cattaneoetal15, abs(margin) < 5)))

# For what it's worth, I wrote it this way when I started looking at the data.

# How about we try it this way
Treat0 = Cattaneoetal15 %>% filter(treatment == 0 & margin != 100 & vote != 0)
Treat1 = Cattaneoetal15 %>% filter(treatment == 1 & margin != 100  & vote != 0)

M12 <- lm(vote ~ margin, data=Treat0)
M13 <- lm(vote ~ margin, data=Treat1)

Treat0 %>%
  data_grid(margin) %>%
  add_predictions(M12) -> yhat_treat0

Treat1 %>%
  data_grid(margin) %>%
  add_predictions(M13) -> yhat_treat1

# This is going to be convoluted...
ggplot() +
geom_vline(xintercept = 0, size=1.3, color="#666666", linetype="dashed") +
  geom_point(data = Senate,
             mapping = aes(x = margin, y = vote),
             color="black", fill="#666666", alpha=0.4) +
  geom_line(data = yhat_treat0,
            mapping = aes(x = margin, y = pred), colour = "red", size = 1.5) +
  geom_line(data = yhat_treat1,
            mapping = aes(x = margin, y = pred), colour = "red", size = 1.5)

# Or, this way...
spread_predictions(tibble(margin = 0),
                   M12, M13) %>%
  mutate(rd_est = M13 - M12)

# Alternatively...

(filter(broom::tidy(M13), term == "(Intercept)")[["estimate"]] -
    filter(broom::tidy(M12), term == "(Intercept)")[["estimate"]])

# If you know how to compare intercepts from two separate regressions and get some predictions on basic models, RDD is pretty easy.

# Now, let's get to Part 2

# Part 2: MLDA Regression Discontinuity Analyses -----
# ----------------------------------------------------

# I added the data as part of the post8000r package.
MLDA
?MLDA

# FIrst, let's create some indicator variables to add to these data.
# I think I'm reading the Stata .do file right here.

MLDA %>%
  mutate(age = agecell - 21, # for later regressions
         over21 = ifelse(agecell >= 21, 1, 0), # if over 21...
         # other causes of death.
         othercauses = external - homicide - suicide - mva) -> MLDA

# This should recreate Figure 4.4, with some change for your information.
MLDA %>%
  select(agecell, over21, all, mva, internal) %>%
  gather(category, value, -agecell, -over21, na.rm=T) %>%
  mutate(category = forcats::fct_recode(category,
                                        "All Causes" = "all",
                                        "Motor Vehicle Accidents" = "mva",
                                        "Internal Causes" = "internal")) %>%
  ggplot(aes(x = agecell, y = value)) +
  geom_point() +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ poly(x, 2)) +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ x, color = "black") +
  facet_grid(category ~ ., scales = "free_y") +
  labs(y = "Mortality rate (per 100,000)", x = "Age")


# This should be Table 4.1
# Note: I ganked this code from Jeff Arnold.
# I lack the time to write this myself.

responses <- c("all" = "All deaths",
               "mva" = "Motor vehicle accidents",
               "suicide" = "Suicide",
               "homicide" = "Homocide",
               "othercauses" = "Other external causes",
               "internal" = "All internal causes",
               "alcohol" = "Alcohol")


run_reg <- function(y) {
  mods <- list(
    "Ages 19-22, Linear" =
      lm(quo(!!sym(y) ~ age * over21), data = MLDA),
    "Ages 19-22, Quadratic" =
      lm(quo(!!sym(y) ~ poly(age, 2, raw = TRUE) * over21), data = MLDA),
    "Ages 20-21, Linear" =
      lm(quo(!!sym(y) ~ age * over21),
         data = filter(MLDA, agecell >= 20, agecell <= 22)),
    "Ages 20-21, Quadratic" =
      lm(quo(!!sym(y) ~ poly(age, 2, raw = TRUE) * over21),
         data = filter(MLDA, agecell >= 20, agecell <= 22))
  )
  out <- tibble(
    model_name = names(mods),
    model = mods,
    ages = rep(c("19-22", "20-21"), each = 2),
    trend = rep(c("Linear", "Quadratic"), 2),
    model_num = seq_along(mods)
  ) %>%
    mutate(coefs = map(model, ~ broom::tidy(coeftest(.x, vcovHC(.x))))) %>% # nolint
    unnest(coefs, .drop = FALSE) %>%
    filter(term == "over21") %>%
    select(model_name, model, term, estimate, std.error) %>%
    mutate(response = y)
  # sample size = df.residuals + residuals
  out[["obs"]] <- map_dfr(mods, broom::glance) %>%
    mutate(obs = df.residual + df) %>%
    pluck("obs")
  out
}

mlda_regs <- map_dfr(names(responses), run_reg) %>%
  mutate(response = recode(response, !!!as.list(responses)))

mlda_regs %>%
  select(model_name, response, estimate, std.error) %>%
  gather(stat, value, estimate, std.error) %>%
  spread(model_name, value)
