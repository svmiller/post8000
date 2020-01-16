install.packages(c("tidyverse","devtools"))

# Once it's installed:
library(tidyverse)
library(devtools)

# Where I'll be putting some example data sets.
devtools::install_github("svmiller/post8000r")

library(post8000r)

# Note: hypothetical data
Apply <- haven::read_dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
# County unemployment
Cunemp <- read_tsv("https://download.bls.gov/pub/time.series/la/la.data.64.County") 

data(pwt_sample)
names(pwt_sample)
# also: help(pwt_sample)

pwt_sample %>% glimpse() # notice the pipe
pwt_sample %>% summary()


pwt_sample %>% select(everything())  # grab everything
pwt_sample %>% select(-labsh) # grab everything, but drop the labsh variable.
pwt_sample %>% select(country, year, rgdpna) # grab just these three columns.

# Notice we can chain some pipes together
pwt_sample %>%
  # group by country
  group_by(country) %>%
  # Get me the first observation, by group.
  slice(1)


pwt_sample %>%
  # Get me the first observation for each country
  slice(1) # womp womp. Forgot to group_by()

pwt_sample %>%
  # How many observations are in the data?
  summarize(n = n())

# Note: works *wonderfully* with group_by()
pwt_sample %>%
  group_by(country) %>%
  # Give me the max real GDP observed in the data.
  summarize(maxgdp = max(rgdpna, na.rm=T))


pwt_sample %>%
  # Convert rgdpna from real GDP in millions to real GDP in billions
  mutate(rgdpnab = rgdpna/1000)


pwt_sample %>%
  group_by(country) %>%
  # divide rgdpna over the country's max, for some reason.
  mutate(rgdpnaprop = rgdpna/max(rgdpna, na.rm=T))


pwt_sample %>%
  # give me just the USA observations
  filter(isocode == "USA")


pwt_sample %>%
  # give me the observations from the most recent year.
  filter(year == max(year))


# convert real GDP to billions
mutate(rgdpnab = rgdpna/1000) -> NewObjectName
