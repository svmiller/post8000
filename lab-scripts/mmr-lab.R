#' ---
#' title: "Making the Most of Regression (or: Regression is Story-telling and Here's How to Tell Your Story Well)"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 8 April 2021
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
#' - [Age, Income, Partisanship, Racial Attitudes and the Trump Vote in 2016](http://svmiller.com/blog/2017/04/age-income-racism-partisanship-trump-vote-2016/)
#' - [How to Make the Most of Regression: Standardization and Post-Estimation Simulation](http://svmiller.com/blog/2020/04/post-estimation-simulation-trump-vote-midwest/)
#' - [Summarizing Ordinal Models with Simulation from a Multivariate Normal Distribution](http://svmiller.com/blog/2020/04/summarizing-ordinal-models-with-simulation-multivariate-normal/)
#' 
#' # R Packages/Data for This Session
#' 
#' We'll be using a handful of packages for this lab session. You've seen `{tidyverse}` and `{stevedata}` before. `{tidyverse}` does most things workflow
#' and `{stevedata}` is my data set vanity project that is nominally useful for teaching students like you. I'm *fairly* sure I've asked you to install
#' `{modelr}` before, which is a great package for creating hypothetical data based on model parameters/inputs. The only one here that might be new is
#' `{arm}`. Basically, some of my regression helper functions in `{stevemisc}` lean on stuff in `{arm}`. `get_sims()` is a wrapper around `arm::sim()` and
#' `r2sd()` is a simpler version of `arm::rescale()`. However, I don't like directly loading `{arm}` into stuff because `{arm}` has a big function clash with
#' `{dplyr}` (part of `{tidyverse}`) over `select()`. Thus, `{stevemisc}` functions obviate the need to load `{arm}`, but emphasize the need to install `{arm}`.
#' This function will make sure everything that should be installed is ultimately installed by way of CRAN.

if_not_install <- function(packages) {
  new_pack <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_pack)) install.packages(new_pack)
}

if_not_install(c("arm", "modelr", "stevedata", "tidyverse"))

#' Here, I might finally ask that you install `{stevemisc}`, if you have not already. While this script will load modular versions of the `r2sd()` function
#' and `get_sims()` function, I think of this as a nifty package. It's mine, after all, and I am nothing if not hopelessly, terminally vain. Perhaps I'm plotting
#' an eponymous takeover of an entire programming language akin to what Cher did to pop music for the better part of 30 years. The majro difference between her and me is,
#' well, she's actually really good at what she does and I specialize exclusively in parlor tricks.
#' 
#' No matter, consider "un-commenting" (sic) this line of code and installing `{stevemisc}`. It's not yet on CRAN, mostly because the CRAN gatekeepers aren't happy
#' with some of the "hackier" (i.e. lazier) things I have in this package. That's on me, though.

# devtools::install_github("svmiller/stevemisc")

#' Okie doke. Let's load stuff. Do note that I asked you to install `{arm}`, but we won't directly load it.

library(tidyverse) # for everything
# library(stevemisc) # for get_sims() and r2sd(), devtools::install_github("svmiller/stevemisc")
library(stevedata)
library(modelr) # for data_grid()

#' If you load `{stevemisc}`, you won't need to load these functions since these are automatically loaded in it. However, let's copy those functions
#' over for this script in case you're having Curl issues on your end.

get_sims <- function(model, newdata, nsim, seed) {
  if (missing(seed)) {
    
  } else {
    set.seed(seed)
  }
  modelsim <- arm::sim(model, n.sims = nsim)
  modmat <- model.matrix(terms(model), newdata)
  the_sims <- tibble(y = numeric(), sim = numeric())
  for (i in (1:nsim)) {
    if (is(model, "lm") == TRUE | is(model, "glm") == TRUE) {
      yi <- modmat %*% coef(modelsim)[i, ]
    } else {
      # assuming it's merMod
      yi <- modmat %*% coef(modelsim)$fixef[i, ]
    }
    simval <- rep(i, length(yi))
    hold_me <-  suppressMessages(as.data.frame(cbind(yi, simval)))
    the_sims <- rbind(the_sims, hold_me)
  }
  names(the_sims) <- c("y", "sim")
  the_sims <- as_tibble(the_sims)
  return(the_sims)
}

r2sd <- function(x, na = TRUE) {
  return((x - mean(x, na.rm = na)) / (2 * sd(x, na.rm = na)))
}

#' `TV16` comes from the 2016 Cooperative Congressional Election Study (CCES) data. It allows us to model
#' the individual-level correlates of the self-reported Trump vote in 2016 using data from all 50 states,
#' stratified on state population and other demographics. Importantly, there are more Californians than
#' South Carolinians so data of this sort should (and here: does) reflect that. This is copy-pasted from
#' another lab script that introduced these data.
#' 
#' Let's use it to do an analysis, much like we did for the week on logistic regression. In this case, let's 
#' subset the data to white voters in five Midwestern states: IN, MI, OH, PA, and WI. These are five states that 
#' Obama won in 2008 but Trump won in 2016. We'll set aside, for the moment, concerns about spatial heterogeneity
#' that we could otherwise address in a mixed effects model. I'm also going to create another variable that I had
#' available in an earlier version of the data but had to take out for the "published" version of the data on CRAN.
#' These are generation cutoffs using the age data (itself derived from the birth-year data explicitly asked by
#' CCES but not included in the `TV16` data). For what it's worth, "generations" are arbitrary constructs but these
#' cutoffs come by way of Pew.

TV16 %>%
  filter(racef == "White") %>%
  filter(state %in% c("Indiana","Ohio","Pennsylvania","Wisconsin","Michigan")) %>%
  mutate(birthyr = 2016 - age,
         generation = case_when(
           birthyr <= 1945 ~ "Greatest/Silent",
           between(birthyr, 1946, 1964) ~ "Baby Boomer",
           between(birthyr, 1965, 1980) ~ "Gen X",
           between(birthyr, 1981, 1996) ~ "Millennial",
           birthyr >= 1997 ~ "Gen Z"
  )) %>%
  select(uid:age, generation, everything()) -> Data

#' # Understanding (the Limitations) of the Regression Output
#' 
#' The whole point of this lab script is to show you how to make the most of regression ("MMR"). Regression is the workhorse tool
#' for quantitative methods, and a lot of applications will lean on whether there is a "there there" for a particular independent
#' variable of interest. However, you can no longer just apply a regression and be done with it. Regression is a form of story-telling.
#' You, the researcher, have to tell your story well and in a way that is immediately accessible to people who will ideally be paying
#' you good money to do this for them. Tell your story well.
#' 
#' Let's assume we wanted to model the Trump vote in 2016 for white voters in these five states as a function of the 
#' respondent's age, whether the respondent is a woman, whether the respondent has a four-year college diploma, 
#' the household income of the respondent, the partisanship of the respondent (D to R, 7-point scale), the 
#' respondent's ideology (L to C, 5-point scale), whether the respondent says s/he is a born-again Christian, and an 
#' estimate of the respondent's cognitive racism. All these should be ported over from the previous lab session, except for
#' the cognitive racism variable. I mention a little bit about this variable in the 2017 post. Basically, DeSante and Smith's 
#' "cognitive racism" measure emerged alongside another related one ("empathetic racism") as improvements of the "racial
#' resentment" battery that had been around for 30+ years to that point. Here, cognitive racism (on this latent scale of mine)
#' comes by way of two survey items, one measuring whether a respondent acknowledges structural advantages afforded to white people
#' that are not afforded to others and whether the respondent believes racism in the U.S. is rare. The concept around these two are a
#' respondent's awareness, or lack thereof, of systemic racism.

M1 <- glm(votetrump ~ age + female + collegeed + famincr +
            pid7na + ideo + bornagain + lcograc,
          data = Data, family=binomial(link="logit"), na.action=na.exclude)

summary(M1)

#' Nothing here is terribly surprising and any differences from the version of this we had in the logistic regression lab script
#' may emerge from the inclusion of data from other states. Everything is significant beyond the coefficients for household income and gender.
#' Look at this output and you might wonder if cognitive racism has the largest effect on the Trump vote. After all, it has the largest
#' coefficient on an absolute scale. Wouldn't that mean largest effect?
#' 
#' The answer here should be a clear "no." It should be bloody obvious that partisanship is going to have the largest effect on the vote for Trump.
#' Republicans vote for Republicans while not-Republicans are much less likely to vote for Republicans.
#' However, that discrete 7-point scale is going to differ from the continuous scale of cognitive racism, which is bound between about -2 and 2. 
#' Basically: coefficients can't be compared when they're not on a common scale. The model, as it is, can be assessed for what parameters are
#' statistically discernible from a counterclaim of zero effect, but this type of comparison is not appropriate.
#' 
#' # MMR: Scale by Two Standard Deviations
#' 
#' This situation is why Gelman recommends scaling anything that's not binary by two standard deviations. The extent to which statistics 
#' textbooks introduce you scaling inputs, it's almost always just a standard deviation. This is useful; the regression coefficient that emerges
#' comes from inputs where 0 is the mean, communicates magnitude effects that might be more interesting, and it will make the intercept interpretable
#' as well. However, scaling by two standard deviations does all that *and* has the helpful side effect of putting everything on a roughly common scale. 
#' Basically, scaling by two standard deviations results in variables that have a mean 0 and a standard deviation of .5. Binary variables, except for those with
#' high "imbalance" (i.e. where pr(x == 1) is small), will also have standard deviations close to .5. This may not be perfect, but it's really good and often close.
#' 
#' The `r2sd()` function in my `{stevemisc}` package will do this.

Data %>%
  # Select the variables we want to mutate with mutate_at()
  # Apply the function to all of them and make a new column of them.
  mutate_at(vars("age", "famincr","pid7na","ideo", "lcograc"),
            list(z = ~r2sd(.))) %>%
  # ^ This will create a _z suffix for these new variables.
  # Let's use rename_at() to make those prefixes.
  rename_at(vars(contains("_z")),
            ~paste("z", gsub("_z", "", .), sep = "_") ) -> Data

#' Now, let's re-run the model.

M2 <- glm(votetrump ~ z_age + female + collegeed + z_famincr +
            z_pid7na + z_ideo + bornagain + z_lcograc,
          data = Data, family=binomial(link="logit"), na.action=na.exclude)

summary(M2)

#' It should be no surprise that putting everything on a common scale elevates the effect of partisanship. It will 
#' invariably be the largest predictor of vote choice in a national election like this. Interestingly, it looks 
#' like the effect of ideology is roughly similar in scale to cognitive racism as well. Also note the intercept means
#' something as well. It tells us the probability of a Trump vote in the data for what amounts to a typical man (who
#' is not a born-again Christian) is about .458. If you want to see how this works, recall our discussion of what is
#' a logistic regression and wrap that intercept in a `plogis()` function.
#' 
#' # MMR: Create Some Quantities of Interest
#' 
#' Another way of telling your story well is to create some quantities of interest, based on the model output. Here, you 
#' can do anything you like and the quantities of interest you generate should be tailored to whatever it is you think is
#' important to communicate on the model output. Let's focus on the age variable as a place to start. Here: age is intuitively
#' interval and communicating means from it makes sense. However, scaling age and using the scaled variable in a regression 
#' creates some confusion to the researcher as to what age corresponds with value of `z_age`. I want to show you how you can
#' backtrack a little bit here.
#' 
#' First, let's pitch a "quantity of interest" as the expected probability of voting for Trump by the typical age of each generation.

Data %>%
  group_by(generation) %>%
  summarize(medianage = median(age)) %>% arrange(medianage)  %>% pull(medianage)  -> genmedage

genmedage

#' Basically: the typical Gen Zer is 19 in this sample. The typical Millennial is 29. The typical Gen Xer is 44. 
#' The typical Boomer is 60. The typical Greatest or Silent is 76.

#' Now, let's find the z_ages that coincide with those because we included `z_age` in the model, not `age`. 

Data %>%
  # find ages that match what we just recorded in genmedage
  filter(age %in% genmedage) %>%
  # find the distinct z_ages that coincide with those, arrange, and extract as a vector.
  distinct(z_age) %>% arrange(z_age)  %>% pull(z_age) -> z_genmedage


genmedage
z_genmedage

#' Now, let's use `{modelr}`'s `data_grid()` to create a hypothetical set of observations with unique `z_ages`.

Data %>%
  data_grid(.model = M2, z_age = z_genmedage, votetrump = 0) -> newdat


newdat

#' ^ Here's what this did. We created five rows of identical respondents. Basically, by default, `data_grid()` sets 
#' everything at the median observation unless you tell it otherwise. Note: the 
#' `get_sims()` function will want to see the DV as well, even if it's irrelevant to the simulation. Just make sure 
#' a column name matching the DV is in there. So, what we did here was create a data grid from `Data`, where 
#' we leaned on the covariates in the model (`M2`) and created five rows of identical observations, save for `z_age`.
#' In this case, `z_age` values are the median age for each of the five generations.

newdat

#' It would help, for clarity sake, to say what is the actual age that coincides with z_age

newdat %>% bind_cols(., tibble(age = genmedage)) %>%
  select(age, z_age, everything()) -> newdat


#' Now, let's use the `get_sims()` function (from my `{stevemisc}` package) to create some simulations. These simulations, 
#' which lean on the `arm::sim()` function quietly, are drawn from a multivariate normal distribution where we're relying on the 
#' model's betas and covariance matrix to create some random draws as a posterior distribution, if you will. The `get_sims()` function 
#' is a wrapper that does this for you and creates a tidy output.

#' Note: the function asks for, in order: the model (`M2`), the newdata of hypothetical observations (`newdat`),
#' how many simulations you want (1,000 is considered default), and, finally, a reproducible seed. 
#' A reproducible seed is optional, but good hygiene.

simsAge <- get_sims(M2, newdat, 1000, 8675309) # Jenny, I got your number.

simsAge %>%
  # Remember, these are logits. Let's inverse those real quick
  mutate(y = plogis(y)) -> simsAge

simsAge

#' ^ Notice this is 5,000 rows and two columns. The column `y` was a simulated natural logged odds of voting for Trump. Now it's a probability.
#' The `sim` column tells you which simulation it is. Notice how sim == 1 has five rows? Remember you had five rows of newdat?
#' That means the first row in sim == 1 is the simulated logit of voting for Trump for an 19-year old. The 
#' second row in sim == 1 is the simulated logit of voting for Trump for a 29-year old, etc. The only caveat comes
#' in noting two things: 1) There are 1,000 simulations, so sim == 1 is just one of those 1,000 for 19-year olds, etc.
#' 2) You, the researcher, have to remember what you're looking at.
#' 
#' But, you could make it easy for yourself to match those simulations to the values of x (here: `age`).

newdat %>%
  # we really just want age
  select(age) %>%
  # Repeate these 5 rows however many times we did the simulations (here: 1000)
  slice(rep(row_number(), 1000))  %>%
  # bind them together
  bind_cols(simsAge, .) -> simsAge

simsAge # voila

#' ^ even though the model technically used `z_age`, we just wanted to match `z_age` with `age`.
#' 
#' Now, let's prepare a summary of this. Here's where I reiterate that cool thing about simulations 
#' is that the world is your oyster. You can prepare this info any way you'd like. For example, here's a raw summary.

simsAge %>%
  # Remember, you want to summarize simulations. Group by the age (or value of whatever x you want)
  group_by(age) %>%
  # Let's create 95% intervals
  summarize(mean_y = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975))

#' You can also prepare this graphically.

simsAge %>%
  group_by(age) %>%
  summarize(mean_y = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975)) %>%
  # ^ same basic summary
  # Below: treat age as a factor. Trust me, it'll be more readable
  ggplot(.,aes(as.factor(age), mean_y, ymin=lwr, ymax=upr)) +
  theme_bw() +
  # geom_pointrange() creates point summary with those ymin and ymax
  geom_pointrange() +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_discrete(labels = c("Typical Gen Z Age (19)", "Typical Millennial Age (29)",
                              "Typical Gen X Age (44)", "Typical Boomer Age (60)",
                              "Typical Greatest/Silent Age (76)")) +
  coord_flip() +
  labs(x = "", y = "Simulated Probability of Voting for Trump (with 95% Intervals)")

#' There are any number of things you can do here, but simulation by way of multivariate normal offers so many cool opportunities to summarize
#' your model output in a way that's immediately more intuitive to more people than just shoving a regression table in the reader's face and expecting
#' them to fill in the blanks.
#' 
#' You could also do this to do more thorough comparison/evaluation of competing coefficients. I did something similar in my
#' [recent publication in *Social Science Journal*](http://svmiller.com/research/economic-anxiety-ethnocentrism-immigration-1992-2017/) 
#' about what has the larger magnitude effects on attitudes about immigration. In this application, though, I'm piqued by this potential claim 
#' that the effect of cognitive racism in explaining the Trump vote might be equivalent to the effect of ideology (if not partisanship). 
#' Model 2 at least implied that, or suggested it, with standardized coefficients. 
#' 
#' How might we tease that out in simulation? The process would be similar. First, let's get a sense of what our `z_ideo` values are in
#' relation to our `ideo` values.

Data %>%
  distinct(ideo, z_ideo) %>% na.omit %>% arrange(ideo)

#' ^ this grabbed my eye. Notice the effect of going from a 2 to 4 is almost a change of two standard deviations? i.e. the 
#' difference between the two is almost 1. So: about 2 SDs. For those curious, a 2 is a "liberal" and a 4 is a "conservative." 
#' Let's pull those.


Data %>% filter(ideo %in% c(2,4)) %>%
  distinct(z_ideo) %>%  arrange(z_ideo) %>% pull(z_ideo) -> z_ideos


#' I'm going to have to get creative with these newdats, but let's create one for these two ideology values.

Data %>%
  data_grid(.model = M2, z_ideo = z_ideos, votetrump = 0) -> newdat_ideo


#' The difference is almost 1, but let's be precise

diffzideo <- z_ideos[2] - z_ideos[1]

diffzideo

#' Now, let's create another data frame. This will be for hypothetical values of `z_lcograc`. One is zero (i.e. the mean). 
#' The other value is the difference in scale on ideology. Because `lcograc` is interval/continuous, this is a plausible value 
#' by itself and coincides with an identical increase in scale of ideology.

Data %>%
  data_grid(.model = M2, z_lcograc = c(0,diffzideo), votetrump = 0) %>% # let's just bind the two together while we're here.
  bind_rows(newdat_ideo, .) -> newdat

#' ^ so, in this object, the first two rows have different values of ideology. Everything else is constant. In 
#' the second two rows, there are different values of cognitive racism. Everything else is constant. Always remember
#' what you're doing at every step of the way.
#' 
#' Now, let's get some simulations.

simsIR <- get_sims(M2, newdat, 1000, 8675309) 

simsIR

#' ^ We could add in the newdat frame like we did previously, but that's less useful in this case, I think. Remember the second point 
#' I mentioned above about your sims. You gotta know what you're looking at. So let's manually create an identifier.

simsIR %>%
  mutate(indicator = rep(c("Ideology Change (L to C)","Ideology Change (L to C)",
                           "Identical Cognitive Racism Change","Identical Cognitive Racism Change"), 1000)) -> simsIR

#' Now, do some group_by summarizing. Let's do first differences here instead.

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
  theme_bw() +
  geom_pointrange() + coord_flip() +
  labs(x = "Indicator",
       y = "Mean First Differences (with 95% Intervals)") 

#' ^ takeaway: as a point estimate, cognitive racism is having a larger magnitude effect than ideology when precisely adjusted for scale.
#' The point estimate is bigger and, incidentally, it's lower bound is above the *point* estimate for ideology.
#' However, ideology's upper bound overlaps cognitive racism considerably. It's estimated first difference is more diffuse.
#' You can't say for certain that cognitive racism had a larger effect, but the estimate is larger and is more precise.
#' 
#' While you should aspire to tailor the quantities of interest toward the story you want to tell to your intended audience (typically
#' regarding some variable of interest), again, you can do anything you want here. For example, we know other values vary by age as well. So, it's
#' not quite a simple case of just "holding everything constant" and allowing just one thing to vary. What if you wanted to do a more general assessment
#' of varying patterns of the Trump vote by reference to the "generations" data we created? Here's how you'd do that.

Data %>%
  group_by(generation) %>%
  summarize_at(vars(all.vars(M2$formula)[2:9]), ~list(median(., na.rm=T))) %>%
  unnest(generation:ncol(.)) %>%
  arrange(z_age) %>%
  mutate(votetrump = 0) -> newdat_generation


get_sims(M2, newdat_generation, 1000, 8675309) %>%
  mutate(y = plogis(y),
         generation = fct_inorder(rep(c("Gen Z", "Millennial", 
                                        "Gen X", "Baby Boomer", 
                                        "Greatest/Silent"), 1000))) %>%
  group_by(generation) %>%
  summarize(mean_y = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975))

#' You could also plot it as well.

get_sims(M2, newdat_generation, 1000, 8675309) %>%
  mutate(y = plogis(y),
         generation = fct_inorder(rep(c("Gen Z", "Millennial", 
                                        "Gen X", "Baby Boomer", 
                                        "Greatest/Silent"), 1000))) %>%
  group_by(generation) %>%
  summarize(mean_y = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975)) %>%
  ggplot(.,aes(generation, mean_y, ymin=lwr, ymax=upr)) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_bw() +
  coord_flip() +
  geom_pointrange() +
  labs(y = "Mean Probability of Voting for Trump (with 95% Intervals)",
       x = "")

#' What about the probability of a Trump vote for each combination of college-educated and self-identified "born again" Christian?

Data %>%
  data_grid(.model = M2,
            votetrump = 0,
            collegeed = c(0, 1),
            bornagain = c(0, 1)) -> newdat_colborn

sims_colborn <- get_sims(M2, newdat_colborn, 1000, 8675309)

newdat_colborn %>%
  # we really just want these two
  select(collegeed, bornagain) %>%
  # Repeate these 5 rows however many times we did the simulations (here: 1000)
  slice(rep(row_number(), 1000)) %>%
  bind_cols(., sims_colborn) %>%
  mutate(y = plogis(y)) %>%
  # create categories
  mutate(colcat = ifelse(collegeed == 0, "Not College Educated", "College Educated"),
         bornagaincat = ifelse(bornagain == 0, "Not Born Again", "Born Again"),
         cat = paste0(colcat, ",\n", bornagaincat)) %>%
  # summarize
  group_by(cat) %>%
  summarize(mean = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975)) %>%
  ggplot(.,aes(cat, y=mean, ymin=lwr, ymax=upr)) +
  geom_hline(yintercept = 0.5, linetype="dashed") +
  coord_flip() +
  theme_bw() +
  geom_pointrange()

#' What you do here is up to you. I don't know the story you want to tell, but I think these to be the most accessible tools to tell your story.