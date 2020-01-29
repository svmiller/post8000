library(post8000r)
library(tidyverse)

# install.packages("tidyverse")

# update it again
devtools::install_github("svmiller/post8000r")

# Also install
install.packages("broom")

# Ancombe's Quartets...
Quartets

# Same mean and SD
Quartets %>%
  group_by(quartet) %>%
  summarize_if(is.numeric, list(mean = mean, sd = sd))

# Correlations are effectively the same
Quartets %>%
  group_by(quartet) %>%
  summarize(r = cor(x, y))

# Linear function of y ~ x is practically the same
Quartets %>%
  group_by(quartet) %>%
  do(model = lm(y ~ x, data = .))  %>% broom::tidy(model)


# But each quartet is quite different

Quartets %>%
  ggplot(.,aes(x,y)) +
  facet_wrap(~quartet) + geom_point() + geom_smooth(method = "lm")



# Simpson's paradox

Guber99

# Negative correlation

Guber99 %>%
  summarize(r = cor(expendpp, total))

# But...

Guber99 %>%
  # create quick-and-dirty quartiles for perc of state taking SAT
  mutate(group = as.factor(ntile(perctakers, 4))) %>%
  group_by(group) %>%
  summarize(r = cor(expendpp, total))

# Ecological fallacy
Illiteracy30

Illiteracy30 %>%
  summarize_if(is.numeric, sum) %>%
  gather(var) %>%
  separate(var, c("category","literacy"),"_") %>%
  mutate(literacy = ifelse(!is.na(literacy), "Illiterate", "Total Population")) %>%
  group_by(category) %>%
  summarize(prop = min(value)/max(value))


# but...

Illiteracy30 %>%
  mutate(foreignp = fbwhite/pop,
         illiterate = pop_il/pop,
         fbilliterate = fbwhite_il/fbwhite) %>%
  ggplot(.,aes(foreignp, illiterate)) + geom_point() +
  geom_smooth(method = "lm")

# Basically, ecological correlations are not substitutes for individual correlations

# Let's talk central limit theorem
