#' ---
#' title: "Measurement Error"
#' author: Steven V. Miller, [svmiller.com](http://svmiller.com)
#' date: 11 February 2021
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


#' # Elsewhere on My Website
#' 
#' Some of what I write here is taken from my website. Please read these:
#' 
#' - [Visualizing Random Measurement Error in R](http://svmiller.com/blog/2020/02/random-measurement-error/)
#' 
#' Be advised I might plagiarize myself here.
#' 
#' # R Packages/Data for This Session
#' 
#' We'll be using fake data, by in large, to illustrate what measurement error does to our inferences, so there's no real data 
#' to belabor here. I will only add that you may want to install my toy R package---[{stevemisc}](https://github.com/svmiller/stevemisc)---on your computer if you have not
#' already. This R package has a function I wrote for creating fake data where, for illustration's sake, everything is drawn from a 
#' standard normal distribution. For your sake, I'll just copy-paste this function as it is from the package so you won't have to install it from
#' Github. That said, you may want to do this. This function appears as `cor2data()` below. You can peak at what it's doing, even as it's a lot of
#' matrix algebra and I *never* liked looking at matrix stuff when I was in your shoes. Briefly, `cor2data()` takes a specified correlation matrix
#' and simulates data of some desired size to satisfy/approximate those preset correlations.

library(tidyverse)


# You can either install and load stevemisc, or call this function randomly. Your pick.
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

#' We're going to use this `cor2data()` function to create a few toy data sets. We'll keep things simple here. Namely, assume just two
#' systematic influences of some outcome `y` here. These will be `x1` and `x2` here. Assume, also, an error term `e` that includes random noise
#' not picked up by our systematic influences of `x1` and `x2`. In this first case, and to spare the reader on having to chew on a correlation matrix
#' too much, these correlations among `x1`, `x2`, and `e` are functionally zero. We'll set the correlations here at .001, but that's basically zero.
#' In other words, `x1` is uncorrelated with the error term `e` and is uncorrelated with `x2`. This is kind of an ideal case of a kind.

vars = c("x1", "x2",  "e")
Cor <- matrix(cbind(1, 0.001, 0.001,
                    0.001, 1, 0.001,
                    0.001, 0.001, 1),nrow=3)
rownames(Cor) <- colnames(Cor) <- vars

#' Notice below: x1, x2, and the error term are obviously perfectly correlated with itself (the diagonals). However, there are effectively
#' zero correlations with anything else in the matrix (the .001s).
Cor

#' We'll create a data frame (`A`) from these data. The data frame will have 10,000 observations.

A <- cor2data(Cor, 10000, 8675309) %>% as_tibble() # Jenny I got your number...

#' Next, we'll create a a correlation matrix (with just `x1`, `x2`, and `e`) where `x1` and `x2` are modestly correlated (.35). However, neither
#' `x1` and `x2` are correlated with `e`.
#' 
Cor <- matrix(cbind(1, .35, 0.001,
                    .35, 1, 0.001,
                    0.001, 0.001, 1),nrow=3)
rownames(Cor) <- colnames(Cor) <- vars

#' Then, a data frame `B` that simulates data from this correlation matrix.

B <- cor2data(Cor, 10000, 8675309) %>% as_tibble() # Jenny I got your number...

#' Next up: we'll specify a correlation matrix where `x2` is correlated with the error term `e`  at .35. This is definitely not ideal. In an application like this,
#' `e` is unmeasured and it means there's going to be some important error in our inferences going forward.
Cor <- matrix(cbind(1, .001, 0.001,
                    .001, 1, .35,
                    0.001, .35, 1),nrow=3)
rownames(Cor) <- colnames(Cor) <- vars

# Then, a data frame `C` that simulates data from this correlation matrix.

C <- cor2data(Cor, 10000, 8675309) %>% as_tibble() # Jenny I got your number...


#' For simplicity's sake, let's create an outcome `y` for data frames `A`, `B`, and `C`. Simply:
#' let the outcome `y` be a function of a slope-intercept of `1 + x1 + x2 + e`. In regression parlance: 
#' the coefficients for `x1` and `x2` are 1 and the estimated value of `y` = 1 when `x1` and `x2` are 0 (plus `e`, though). For simplicity's sake, 
#' I'm giving the error term e equal footing with x1 and x2. I.e. there is still a lot left unexplained about y in the error term e.

A$y <- with(A, 1 + x1 + x2 + e)
B$y <- with(B, 1 + x1 + x2 + e)
C$y <- with(C, 1 + x1 + x2 + e)

#' # What Systematic Error Does to Inferences
#' 
#' Next, let's explore what measurement error does to our inferences, starting with the case of `A`. This is the ideal case of no measurement error.
#' 
#' ## No Error (an Ideal Case)
#' 
#' In this case, we'll be starting with data frame `A` and including all known effects on `y` (minus the unobserved `e`, obviously.)

summary(MA1 <- lm(y ~ x1 + x2, data=A))

#' This model is well-defined and the true population effects are adequately captured. The estimated intercept and coefficients for `x1` and
#' `x2` are capturing the true population effects. If I wanted to be *really* picky, I could note that technically the coefficient for `x1`
#' is more than two standard errors from the known effect of `x1` on `y`, but I think the OLS model is behaving as it should
#' because this is an ideal case of no measurement error.
#' 
#' Now, let's assume some omitted variable bias in the `A` data frame. We know `y` is in part a function of `x1`. 
#' We have `x1`. We're not going to include `x1`, though.

summary(MA2 <- lm(y ~  x2, data=A))

#' Notice `x2`'s effect on `y` is materially unaffected by the exclusion of `x1`. This is because `x1` and `x2` are uncorrelated with each other.
#' Omitted variable bias is no bias at all when some other covariate is uncorrelated with the other right-hand side stuff. To be clear, if you have
#' that information (in the form of `x1`) then for the love of god include it. The absence of `x1`'s effect on `y` does amount to an underestimation of the
#' expected value of `y` but it does not bias the causal effect of `x2`.
#' 
#' ## When Your Regression Inputs are Correlated
#' 
#' What happens when `x1` and `x2` are moderately correlated, as in data frame B? Recall:
#' 
with(B, cor(x1, x2))

#' Let's see what happens here:
summary(MB1 <- lm(y ~ x1 + x2, data=B))

#' Look familiar? Looks very much like `MA1` using the `A` data frame where both `x1` and `x2` are uncorrelated, indeed down to that fluke case where
#' `x1`'s coefficient is slightly outside the true effect. Here, in data frame `B`, they're clearly correlated. 
#' But OLS separates that out and returns partial effects as regression coefficients.
#' 
#' In the above case where `x1` and `x2` are fundamentally uncorrelated, the exclusion of `x1` had no implications for the causal effect of `x2`. 
#' However, let's see what happens when we exclude `x1` when `x1` and `x2` are uncorrelated.

summary(MB2 <- lm(y ~  x2, data=B))

#' See what happens? The exclusion of `x1` biases the coefficient for `x2` up because of the positive correlation between x1 and x2.
#' In other words: `x2` partly includes `x1`'s effect on `y` as well. This is omitted variable bias "biasing" the direction/strength of the relationship.
#' For the purpose of causal inference, it is immaterial that this particular application is one that is still "positive and statistically significant."
#' That's not at stake. We're clearly overestimating the effect of `x2`. The true relationship between `x2` and `y` is 1. Our estimate is about 1.3. Muy malo.
#' 
#' ## The Worst Case: Correlation with the Error Term (Endogeneity)
#' 
#' What about when one of the variables is correlated with the error term, which we don't directly observe in our data?
#' In other words, there's something in the ether that is not being captured by us as researchers that is correlated with a predictor of interest (`x2`).

with(C, cor(x2, e))

summary(MC1 <- lm(y ~ x1 + x2, data=C))

#' Yikes. `x2`'s effect is greatly overstated because it's positively correlated with an unobserved error term that we don't directly model in the regression.
#' Worse, again: error terms include everything not formally modeled. I.e. we know in simulation we could fix this, but,
#' in the real world, you don't have the data available (yet). This will motivate a lot of the instrumental variables stuff we'll talk about later in the semester.
#' In a situation like this, you'll have to think long and hard how to pry that correlation out of your error term. Stuff like this is much easier said than done.
#' 
#' # Random Measurement Error
#' 
#' We invest more time and energy into concerns of systematic error than we do random measurement error. The tl;dr: here is systematic error makes it more likely
#' we find the *wrong* signal from the proverbial din whereas random measurement error makes it more likely we're just unable to extract any signal from the din. 
#' Neither situations are particularly welcome. The point of causal inference is to adequately isolate the true signal from the din. You don't 
#' want to focus on the wrong signal, and you don't want to be unable to find something that you should. However, the former is worse than the latter.
#' 
#' Let's illustrate what random error is going to do, while effectively plagiarizing my blog post that I asked you to read above. Recall that the script above
#' created some data that is kind of standard normal in a lot of ways. `x1` and `x2` are simulated from standard normal (i.e. mean of 0 and standard deviation
#' of 1). The outcome variable `y` has a *y*-intercept of 1 and coefficients of 1 for both `x1` and `x2`. Plus/minus error, that means the values of `y` range
#' between -5 and 5, or thereabouts. Either way, everything is continuous and everything was randomly generated (i.e. the 43rd observation does not depend on
#' the 42nd observation and does not influence the 44th observation in the data).
#' 
#' Here's what we'll do: for every 10th value of either `y` or `x2`, let's replace that value with something that's wrong. These values that we'll plug into every
#' 10th observation will range from the ridiculously impossible to the plausible and back to the implausible.

new_vals <- c(-500, -100, -10, -5, -3, -2, -1, 0,
              1, 2, 3, 5, 10, 100, 500)

new_vals <- c(seq(1:4), seq(5, 50, 5), 75, seq(100, 500, 100))

new_vals <- c(new_vals*-1, 0, new_vals)

#' I like to talk to my undergrad students about random error as akin to entering an 11 when you meant a 1, or something to that effect. In a situation like this,
#' we have some clumsy fingers entering every 10th value as either something that's plausible (e.g. 0, the hypothetical mean) to the implausible (e.g. 500 or
#' -500).
#' 
#' Now, let's set up a loop to automate this process. I'll annotate it below to explain it. Basically, for all the particular values in `new_vals`, I'm going
#' to replace every 10th value of `x2` with it, re-estimate the statistical regression, gank the important results with `broom`'s `glance()` function, and store
#' the output into something.

randerr_x2 <- function(dat, nv){
  output <- tibble()
  for (i in nv) {
    # Looping through nv
    # For every 10th value for x2, recode it to whatever the ith value is in nv
    dat %>%
      mutate(x2re = ifelse(row_number() %in% c(seq(0, 10000, by =10)), i, x2)) -> dat
    # regress y  on this particular new x2nv variable
    mod <- lm(y ~ x1 + x2re, data=dat)
    # grab r-square
    r2 = broom::glance(mod)[1,1] %>% pull()
    # create a broom table that codes whatever the ith value of new_vals is, and the adjr2 as well
    broomed = broom::tidy(mod) %>% mutate(mod = i, r2 = r2)
    # bind it to otput
    output = bind_rows(output, broomed)
  }
  return(output)
}

#' Also, let me create a plotting function ahead of time that's going to visualize what's at stake here. Here, as I mentioned,
#' is another way of learning `ggplot` by example.

ggranderr_x2 <-  function (data, comparison) {
  data %>%
    mutate(term = ifelse(grepl("x2", term), "x2", term)) %>%
    mutate(lwr = estimate - abs(qnorm(.025))*std.error,
           upr = estimate + abs(qnorm(.025))*std.error) %>%
    ggplot(., aes(as.factor(mod), estimate, ymin=lwr, ymax=upr)) +
    #theme_steve_web() +
    # post_bg() +
    facet_wrap(~term) +
    scale_y_continuous(breaks = seq(0, 1, by = .2)) +
    geom_pointrange() +
    geom_rect(data=comparison, aes(x = NULL,y = NULL,xmin=-Inf, xmax=Inf, 
                              ymin=lwr, ymax=upr), 
              alpha=0.1,fill="red") +
    coord_flip() 
}

#' ## Random Error on the Right Hand Side of the Equation
#' 
#'   Let's start with `x2` for `A`.
AX2 <- randerr_x2(A, new_vals)

#' And let's grab/broom up `MA1`, for context.
MA1df <- broom::tidy(MA1) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error)  %>%
  mutate(r2 = broom::glance(MA1) %>% pull(1))

#' Now, let's compare our messed up random measurement error models with the actual data in which there is no measurement error (in `MA1df`).

ggranderr_x2(AX2, MA1df) +
  labs(x = "Every 10th Observation of x2 is Recoded to This Value",
       y = "Coefficient (with 95% Intervals)",
       title = "The Effect of Random Measurement Error in x2",
       subtitle = "Random measurement error in x2 plummets the estimated relationship of x2 on y to zero, making it hard to discern a signal that objectively exists.",
       caption = "Shaded area communicates 95% Intervals from OLS with no measurement error.")

#' Here's the implication: huge random error in `x2` pushes the estimated effect of `x2` to 0. It just basically becomes noise.
#' If you were to compare the r-squareds (see my blog post), you'd see the effect is to collapse r2 to a stepwise model where you just didn't estimate x2 at all.
#' 
#' Here's what would happen in the `B` case where `x2` and `x1` are moderately correlated.

BX2 <- randerr_x2(B, new_vals)

# grab/broom up MB1, for context
MB1df <- broom::tidy(MB1) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error)  %>%
  mutate(r2 = broom::glance(MB1) %>% pull(1))


ggranderr_x2(BX2, MB1df) +
  labs(x = "Every 10th Observation of x2 is Recoded to This Value",
       y = "Coefficient (with 95% Intervals)",
       title = "The Effect of Random Measurement Error in x2",
       subtitle = "When x2 and x1 are correlated, the introduction of random error to x2 is equivalent to omitted variable bias.",
       caption = "Shaded area communicates 95% Intervals from OLS with no measurement error.")

#' Echoing the subtitle of that plot, when `x2` and `x1` are correlated, the introduction of random error to `x2` is equivalent to omitted
#' variable bias. It becomes more obvious the more impossible the values of the random error are.

#' Now, here's what happens when `x2` is correlated with the unobserved error term.
#' 
CX2 <- randerr_x2(C, new_vals)


ggranderr_x2(CX2, MB1df)  + # I know this is MB1df, but why do you want biased x2 here? You already know it's 1.
  labs(x = "Every 10th Observation of x2 is Recoded to This Value",
       y = "Coefficient (with 95% Intervals)",
       title = "The Effect of Random Measurement Error in x2",
       subtitle = "When x2 is correlated with the error term, the introduction of random error to x2 has similar effects to when there is no correlation with the error term in an ideal application.",
       caption = "Shaded area communicates 95% Intervals from OLS with no measurement error.")

#' To echo the subtitle, when `x2` is correlated with the error term, the introduction of random error to `x2` has 
#' similar effects to when there is no correlation with the error term in an ideal application. It's just also the case that `x2`'s effects are already
#' being overstated at the onset.
#' 
#' ## Random Error on the Left Hand Side of the Equation
#' 
#' We focused so much time and energy on random measurement error on the right (i.e. in one of the independent variables). What about when there is random 
#' measurement error in the outcome variable? Let's see, but let's write some function first.

randerr_y <- function(dat, nv){
  output <- tibble()
  for (i in nv) {
    # Looping through nv
    # For every 10th value for y, recode it to whatever the ith value is in nv
    dat %>%
      mutate(yre = ifelse(row_number() %in% c(seq(0, 1000, by =10)), i, y)) -> dat
    # regress y  on this particular new x2nv variable
    mod <- lm(yre ~ x1 + x2, data=dat)
    # grab r-square
    r2 = broom::glance(mod)[1,1] %>% pull()
    # create a broom table that codes whatever the ith value of new_vals is, and the adjr2 as well
    broomed = broom::tidy(mod) %>% mutate(mod = i, r2 = r2)
    # bind it to otput
    output = bind_rows(output, broomed)
  }
  return(output)
}

ggranderr_y <-  function (data, comparison) {
  data %>%
    mutate(lwr = estimate - abs(qnorm(.025))*std.error,
           upr = estimate + abs(qnorm(.025))*std.error) %>%
    ggplot(., aes(as.factor(mod), estimate, ymin=lwr, ymax=upr)) +
    #theme_steve_web() +
    # post_bg() +
    facet_wrap(~term) +
    # scale_y_continuous(breaks = seq(0, 1, by = .2)) +
    geom_pointrange() +
    geom_rect(data=comparison, aes(x = NULL,y = NULL,xmin=-Inf, xmax=Inf, 
                                   ymin=lwr, ymax=upr), 
              alpha=0.1,fill="red") +
    coord_flip() 
}

#' Here's what happens when `y` is contaminated by random error.

AY <- randerr_y(A, new_vals)

ggranderr_y(AY, MA1df)

#' General takeaway: random error in `y` will mess with the intercept, but it won't "bias" `x1` and `x2.` 
#' Basically, what's happening here: the causal effect is still being reasonably identified, given the circumstances,
#' but the standard errors start to explode. In other words, the noisier `y` is, the more fruitless it is try to systematically model it. It's noise.
#' 
#' What about in `B` when `x1` and `x2` are a little correlated?
BY <- randerr_y(B, new_vals)

ggranderr_y(BY, MB1df)

#' It's effectively the same thing.
#' 
#' What about when `x2` is endogenous to the unobserved error term?

CY <- randerr_y(C, new_vals)

ggranderr_y(CY, MB1df)

#' Same thing. Not that regressing `y` on a known endogenous parameter is a good idea, but alas. Same takeaway. The noisier `y` is, the odder it is
#' to expect to extract a signal from that din.
