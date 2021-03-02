#' ---
#' title: "Instrumental Variables"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 4 March 2021
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
#' - [An Illustration of Instrumental Variables and a Two-Stage Least Squares (2SLS) Regression](http://svmiller.com/blog/2019/09/instrumental-variables-2sls/)
#' 
#' Be advised I might plagiarize myself here.
#' 
#' # R Packages/Data for This Session
#' 
#' #' We'll be using `{stevedata}` for some data and `{tidyverse}` for most things. `{stevemisc}` has the `cor2data()` function that I will just copy-paste here
#' for convenience. The only real new package to install, if you've yet to install it already, is `{estimatr}`. This package will allow for a single wrapper for
#' instrumental variable analysis that includes the important standard error corrections that you could not do manually. I will also cheat a little bit by showing you that you 
#' can also do instrumental variable analysis in `{brms}`. However, I won't ask you to install that now.
#' 
#' First, if you've yet to install these packages, do so.

if_not_install <- function(packages) {
  new_pack <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_pack)) install.packages(new_pack)
}
  
if_not_install(c("estimatr", "tidyverse", "stevedata"))

library(tidyverse)
library(stevedata)
library(estimatr)

#' Let's bring the `cor2data()` function into the chat, so to say.

cor2data <- function(cor, n, seed){
  # number of observations to simulate
  nobs = n

  # Cholesky decomposition
  U = t(chol(cor))
  nvars = dim(U)[1]

  if(missing(seed)) {

  } else {
    set.seed(seed)
  }

  # Random variables that follow the correlation matrix
  rdata = matrix(rnorm(nvars*nobs,0,1), nrow=nvars, ncol=nobs)
  X = U %*% rdata
  # Transpose, convert to data
  # require(tidyverse)
  Data = t(X)
  Data = as.data.frame(Data)
  return(Data)
}

#' # A Simple Setup for an Instrumental Variable Solution
#' 
#' We're going to start with simulated data to show how instrumental variable (intermittently hereafter: "IV") approaches can fix 
#' some endogeneity problems. Here, we have three variables of note. `control` is 100% unproblematic and will be uncorrelated with
#' anything. `treat` is correlated with the instrument at .85 and correlated with the error term `e`. In other words, there is
#' an endogeneity problem here with the treatment variable that interests us. `instr` is an instrument for the treated variable and
#' is mercifully uncorrelated with `e`. In other words, this instrument will satisfy all three conditions for a valid instrument.
#' It has a strong correlation with the treatment (relevance). It is uncorrelated with anything else (exclusion) and we will later codify
#' that further by making sure it has no systematic influence on the outcome variable. Further, it is not correlated with the error term
#' (exogeneity). 
#' 
#' Let's go ahead and set up these data.

vars = c("control", "treat", "instr", "e")
Cor <- matrix(cbind(1, 0, 0, 0,
                    0, 1, 0.85, -0.5,
                    0, 0.85, 1, 0,
                    0, -0.5, 0, 1),nrow=4)
rownames(Cor) <- colnames(Cor) <- vars

Fake <- cor2data(Cor, 1000, 8675309) %>% as_tibble() # Jenny I got your number...

#' Now, let's make an outcome variable, again keeping in mind the exclusion restriction. Here, we will create an outcome variable (`y`) as follows.
#' The y-intercept (i.e. when `control` and `treat` are 0) is about 5. Both the control variable and the treatment variable have a coefficient of .5 (i.e.
#' a one-unit change in both changes the outcome by .5). There is also that messy error term that will require some care downstream.

Fake$y <- with(Fake, 5 + .5*control + .5*treat + e)

#' Let's lookie-loo (sic) at what we did. Because we played god creating these data, we can already anticipate some problems down the road. But, let's pretend
#' we don't have an endogeneity problem and just go-go-gadget a linear model here. Here's what would happen if we pretended that we had no endogeneity problem.

summary(M1 <-lm(y ~ control + treat, data=Fake))

#' Notice that the control variable is 100% unaffected but the treatment variable is. Indeed, the nature of the problem reduces the coefficient to almost zero with
#' standard errors that will clearly cover it. In other words, the endogeneity problem---that we built into these data, btw---means we can't discern a signal
#' from the noise. The true effect is nowhere close to the coefficient and we even get a type 2 error from NHST standpoint.
#' 
#' You might ask if this is a simple omitted variable problem. What if we included that instrument? Well, we should know ahead of time that won't fix anything and this
#' isn't a simple omitted variable problem. The instrument has no systematic relationship with the outcome. So, here's what would happen if we did the equivalent of
#' administering antibiotics for a viral infection.

summary(M2 <-lm(y ~ control + treat + instr, data=Fake))

#' Not only did that not fix it, it may have made it worse the extent to which you might naively believe the treatment has a negative effect on the outcome. In other words,
#' you misdiagnosed the problem and the prescription unsurprisingly did not make it better.
#' 
#' Because we satisfied the three assumptions, we can fix this with an IV approach. The process looks like this. First, regress your contaminated treatment on the instrument.
#' Second, extract the fitted values from it. Third, regress the outcome on the fitted values from the previous regression. These "two stages" are why
#' we call it "two-stage least squares (2SLS)" regression.
#' 
#' First, here's the first-stage model. Notice the regression estimate is basically the correlation.
# First-stage model...
summary(FSM <- lm(treat ~ instr, data=Fake))

#' Next, extract the fitted values and add it to your data.
Fake %>%
  mutate(treat_hat = fitted(FSM)) -> Fake

#' Then, do the second-stage model with the fitted values as the stand-in for the contaminated treatment. Notice: we basically captured the
#' "true" effects of the control and treatment on the outcome after de-contaminating the endogenous treatment.
summary(SSM <- lm(y  ~ control + treat_hat, data=Fake))

#' The only caveat that IV people will have with doing this manually is that the standard errors in the second stage depend in part
#' on what you did in the first phase, and that doing it manually has no way to account for that. You could write your own function
#' to do this, but mercifully the `iv_robust()` function in the `{estimatr}` package will do this for you. When you're using this
#' function, the second stage goes left of the separator (`|`) while the first stage goes to the right of it. It assumes your
#' treatment variable is just to the left of the separator as well. Do note there are a few different forms of standard error
#' corrections you can do here, but I'm going with the default (which is HC2, IIRC).

summary(IVM <- iv_robust(y ~ control + treat | instr + control, data=Fake))

#' If you wanted a peek at the Bayes world, you can also do this through the `{brms}` package. This is definitely an advanced topic. I offer it here
#' to show that the amazing `{brms}` package offers a convenient wrapper for an IV analysis.

library(brms)

bf1 <- bf(treat ~ instr) # first stage
bf2 <- bf(y ~ control + treat) # second stage

B1 <- brm(bf1 + bf2, data=Fake, refresh = 0,
          seed = 8675309) # by default, should set rescor = TRUE, but I also don't know how to shut that up when spinning the doc.

#' How different/similar are these results?

broom::tidy(SSM)
broom::tidy(IVM)
summary(B1)

#' Robust standard error estimation in this context will be less sanguine about your standard errors, but the effect here is slight. The Bayesian
#' approach kind of bakes in this type of process into the model itself, also resulting in slightly more diffuse estimates.
#' The main takeaway is case of six-of-one here, though, but be sure to explore how much your estimates might be affected by this process.
#' There's probably also a bootstrapping solution here within the IV approach, but I'd need to think about it a little further before going
#' all Leeroy Jenkins on it for a lab script for graduate students.
#' 
#' # An Application: Are There Civics Returns to Education?
#' 
#' Let's do a real-world replication of an instrumental variable analysis, though I think the results here indicate a partial replication. I 
#' acquired these data in Mexico City in a two-week program at IPSA-FLACSO Mexico Summer School in 2019. Here, I milled around for about two weeks
#' getting some ideas on how to teach causal inference stuff while 1) also eating amazing food, 2) seeing one of the best cities on the whole planet, and
#' 3) getting my employer to foot the bill for the opportunity. Please invite me back for stuff, FLACSO.

Dee04
?Dee04

#' The application here is a common one. Basically: does increased education have downstream economic and societal benefits? The particular DV here is whether
#' the respondent is registered to vote. The DV here is binary and the model here is linear. Don't ever do this. Ever. Not even if there's a fire.

summary(D1 <- lm(register ~ college, data=Dee04, na.action=na.exclude))

#' The results suggest we're in orbit of what Dee is reporting in Table 1 (first column), but let's back up a little bit here. What Dee is trying to do
#' is unpack a classic case of a tricky endogeneity problem, albeit one of supreme importance (i.e. the causal effect of higher education in the United 
#' States). However, there's unobserved determinants of both. Certainly, respondents in the HS&B data had some civic values prior to college.
#' Clearly, college entrance can't explain something that happened before it.
#' 
#' Dee proposes an instrument that Card (1995) had actually introduced before. That is: the geographic availability of colleges closer to the respondent.
#' Intuitively, more colleges closer to the respondent (esp. students from disadvantaged backgrounds) should explain college attendance,
#' but those should not have any effect on adult outcomes.
#' 
#' Dee calculated a "distance" variable, which is the distance in miles between the respondent's high school and the nearest college in the county at that time.
#' There's a lot to like about this instrument. Let's start intuitively.
#' 
#' 1) it should satisfy the exogeneity condition. These colleges exist well prior to the child appearing in the HS&B data.
#' 2) you could argue it satisfies the relevance condition. The closer a college, the cheaper it is to attend, and the more opportunity to attend.
#' 3) it should satisfy the exclusion restriction. Distance affects `college`, which affects `register`. However, there's no reason, right now theoretical, to contend
#' a direct relationship between college distance and being registered to vote. Indeed, this is the "only through" language. `distance` should not directly correlated with `register`, "only through" `college`.
#' 
#' How might we check the assumptions? First, we can check the correlation between the instrument (`distance`) and the treatment (`college`) to assess relevance.

Dee04 %>% summarize(relevance = cor(distance, college))

#' It suggests a weaker instrument on the relevance frontier, certainly not as strong as our fake data.. However, a correlation test will vindicate it.

cor.test(Dee04$distance, Dee04$college)

#' How about the "exclusion" assumption? Here's a simple question: is the distance variable correlated with the outcome variable?

Dee04 %>%
  summarize(exclusion = cor(distance, register))

#' Yep. It's a small correlation, but it's one unlikely to be 0.

cor.test(Dee04$distance, Dee04$register)

#' What about the other stuff?

Dee04 %>%
  select(-schoolid, -college) %>%
  select(distance, everything()) %>%
  gather(var, val, 2:ncol(.)) %>%
  group_by(var) %>%
  summarize(cor = cor(distance, val))


#' At the risk of generalizing out this code any further, I'm going to guess that if it's a non-zero correlation with the registration variable, it's
#' going to be a non-zero correlation with the others too (but perhaps not the gender variable). Indeed, you could do that yourself and arrive at the same conclusion.
#' 
#' So, basically, exclusion restriction here is flunked. While these are not formal tests of the exclusion restriction, they are illustrative. You can use your head here too.
#' Think of a case like the race/ethnic variables and the instrument. Notwithstanding some spatial diversity (in a state like SC, there is a fairly sizable rural black population),
#' black Americans and Hispanic Americans tend to be urban and tend to be residing in places where there is more economic opportunity. Colleges are part of that story as well.
#' That's not to say that educational and economic opportunity are neatly provided to square with that, but it's a selection effect of a kind and it's going to be influencing the
#' correlation between the instrument and those categories. If a respondent is black or Hispanic, they're more likely closer to colleges (i.e. the `distance` variable decreases).
#' Makes sense, right? But it does mean we're going to flunk the exclusion assumption. 
#' 
#' I think we almost always are in some meaningful (if mostly symbolic, non-formal) way. Even in an ideal case, that "only through" language means there will likely be some
#' correlation between the outcome and the instrument that will be evident, however naively. Indeed, here it is in our simple/ideal case.
#' 
Fake %>% select(y, instr) %>% cor()

#' However, that correlation is "only through" the treatment and you'll recall there is no systematic effect of the instrument on the outcome. When you're doing IV stuff,
#' this is a story you tell and sell and not something you formally test.
#' 
#' Alrightie, let's get to it then with a replication of Dee. I think we're slightly off here because of the abbreviated nature of the data I got.

summary(D2 <- iv_robust(register ~ college + black + hispanic + female | distance + black + hispanic + female, data=Dee04))

#' It suggests a civics return to education, as Dee reports.

