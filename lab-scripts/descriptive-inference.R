# new data set. Install it again.
devtools::install_github("svmiller/post8000r")

library(post8000r)
library(tidyverse)

# Check out documentation.
?gss_spending
?pwt_sample
?usa_justifbribe

# How to identify modes and medians

# R doesn't have a built-in mode function. So let's write one.

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(gss_spending$race)

# Since the mode is just the most frequently appearing variable, you could just as easily do something else that's more informative.

gss_spending %>%
  group_by(race) %>%
  count()

# graphing helps too.

gss_spending %>%
  group_by(race) %>%
  count() %>%
  ggplot(., aes(as.factor(race), n)) + 
  geom_bar(color="black", alpha=0.8, stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c("White","Black","Other")) +
  geom_text(aes(label=n), vjust=-.5, colour="black",
            position=position_dodge(.9), size=4) +
  labs(x = "",
       y = "Number of Observations",
       caption = "Data: General Social Survey (2018)",
       title = "A Bar Chart of Respondent Race in the General Social Survey (2018)")


# Median does have a built-in function.
# Be mindful, NAs in the data default these functions to NAs as well. Specify "na.rm = T" for medians and means.

median(gss_spending$degree, na.rm=T) # The median is those with a HS ed or less.
median(gss_spending$rincom16, na.rm=T) # The median is 17 ($35k-$39k) https://gssdataexplorer.norc.org/variables/6168/vshow

# graphing helps too.

gss_spending %>%
  group_by(nateduc) %>%
  count() %>%
  na.omit %>%
  ggplot(., aes(as.factor(nateduc), n)) + 
  geom_bar(color="black", alpha=0.8, stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c("Spending Too Much",
                              "Spending the Right Amount",
                              "Spending Too Little")) +
  geom_text(aes(label=n), vjust=-.5, colour="black",
            position=position_dodge(.9), size=4) +
  labs(x = "",
       y = "Number of Observations",
       caption = "Data: General Social Survey (2018).
       Prompt: ''We are faced with many problems in this country, none of which can be solved easily or inexpensively. I'm going to name some of these problems, and for each one I'd like you to name some of these problems,
       and for each one I'd like you to tell me whether you think we're spending too much money on it, too little money, or about the right amount.''",
       title = "A Bar Chart of Attitudes Toward Education Spending in the General Social Survey (2018)",
       subtitle = "A clear majority (median and mode) of Americans think we are spending too little on improving the American education system in 2018.")


# Means are probably what you would want in most applications.
# Likewise, specify na.rm = T

pwt_sample %>%
  group_by(year) %>%
  summarize(meanrgdpna = mean(rgdpna, na.rm=T))

# You can also graph this.

pwt_sample %>%
  group_by(year) %>%
  summarize(meanrgdp = mean(rgdpna, na.rm=T)) %>%
  ungroup() %>% # practice safe group_by()
  mutate(meanrgdpb = meanrgdp/1000) %>%
  ggplot(.,aes(year, meanrgdpb)) + geom_line() +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "",
       y = "Average Real GDP (in Constant 2011 National Prices, in Billion 2011 USD)",
       caption = "Data: Penn World Table (v. 9.1.)",
       title = "Average Real GDP for 21 Rich Countries, 1950-2017",
       subtitle = "The average real GDP in 2017 was over 2 trillion dollars, which should seem super sketchy.")

pwt_sample %>%
  filter(year == 2017) %>%
  mutate(rgdpb = rgdpna/1000) %>%
  ggplot(.,aes(rgdpb)) + geom_density() +
  geom_vline(aes(xintercept = median(rgdpb, na.rm = T)), linetype="dashed") +
  geom_vline(aes(xintercept = mean(rgdpb, na.rm = T))) +
  scale_x_continuous(breaks = seq(0, 20000,by=2000),
                     labels = scales::comma) +
  labs(title = "A Density Plot of Real GDP (in Billions 2011 USD) in 2017 for 21 Rich Countries",
       x = "Real GDP in Billions 2011 USD",
       y = "",
       caption = "Data: Penn World Table (v. 9.1). Median noted in the dashed line while the mean is noted in the solid line.",
       subtitle = "Suddenly, 'average' doesn't look so 'average', certainly because of a high-leverage case like the U.S.")

pwt_sample %>%
  group_by(year) %>%
  mutate(rgdpb = rgdpna/1000) %>%
  summarize(meanrgdpb = mean(rgdpb, na.rm = T),
            sd = sd(rgdpb,na.rm=T))

pwt_sample %>%
  group_by(year) %>%
  mutate(rgdpb = rgdpna/1000) %>%
  summarize(Median = median(rgdpb, na.rm=T),
            Mean = mean(rgdpb, na.rm=T)) %>%
  group_by(year) %>%
  gather(Category, value, Median:Mean) %>%
  ggplot(.,aes(year,value, color=Category, linetype=Category)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1950, 2020, by =5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "",
       y = "Median and Mean Real GDP (in Constant 2011 National Prices, in Billion 2011 USD)",
       caption = "Data: Penn World Table (v. 9.1.)",
       title = "Median and Mean Real GDP for 21 Rich Countries, 1950-2017",
       subtitle = "A mean that further separates from the mean over the course of the data suggests a worsening skew problem (here: the U.S.).")


# Let's talk "dummies" here. "Dummies" (or binary variables) are a unique subset of nominal variables.
# While some dummies have an intuitive order (e.g. "democracy" vs. "not democracy" implies "more democracy"), they are strictly "there" or "not there." They are unordered.
# They have some interesting properties for measures of central tendency.
# Basically: the mode and median will be the same and the mean will communicate the proportion of 1s.
# Do note: your dummies should *always* be 0/1, and not something like 1/2.

gss_spending %>%
  # where sex = 1 for women, 0 for men
  summarize(mode = getmode(sex),
            median = median(sex),
            mean = mean(sex))

# Is it ordinal or interval?
# We love to treat technically ordinal data as interval when we can.
# Rules of thumb: check the number of possible values, then check the skew.
# If you have at least 5 (I prefer at least 7, even to 10) distinct values, you can start to think of it as interval.
# If you have a skew problem, though, then I would resist the urge to think of it as interval.

# Age is technically ordinal, but there are typically a wide number of distinct ages in survey data spanning from 18 to the 80s and beyond.
# Therein, differences between observations are granular enough that means with decimals make some sense.
# Caveat, naturally: don't try to explain age as some outcome. Age is a linear function of time. :P

gss_spending %>%
  summarize(mean = mean(age, na.rm=T),
            median = median(age, na.rm=T),
            distinct = n_distinct(age))

# The sumnat variable in my gss_spending data frame has a small skew problem to the left, but it's cast across 33 different values.

gss_spending %>%
  summarize(mean = mean(sumnat, na.rm=T),
            median = median(sumnat, na.rm=T),
            distinct = n_distinct(sumnat))

# Some variables are too damn ugly to think of as interval the extent to which you want informative means and so on.
# Think of this example from the World Values Survey
#  Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. (Read out statements. Code one answer for each statement). Someone accepting a bribe in the course of their duties

usa_justifbribe %>%
  na.omit %>%
  summarize(mean = mean(justifbribe, na.rm=T),
            median = median(justifbribe, na.rm=T),
            distinct = n_distinct(justifbribe))

# A graph will show the problem.

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

# Other caveat: you can throw out information.

gss_spending %>%
  select(age) %>%
  na.omit %>%
  mutate(agegroups = cut(age, breaks = c(-Inf, 30, 40, 50, 65, Inf),
                         labels=c("18-30","31-40","41-50","51-65","66-89")),  # create five age groups
    # Create a 30 or younger dummy
    age30ory = ifelse(age <= 30, 1, 0)) -> agevars

# Let's see what this did
agevars %>%
  na.omit %>%
  group_by(agegroups) %>%
  summarize(min = min(age),
            max = max(age))

# But, you can't get it back. Take care when you first devise your instrument.