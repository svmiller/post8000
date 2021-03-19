library(tictoc) # https://stackoverflow.com/questions/10893611/storing-tic-toc-values-in-r
library(stevedata)
library(tidyverse)

for (x in 1:1000) {
  # passing x to tic() makes it a label at time of the matching toc() call.
  tic(x)
  lm(y ~ x, data=fakeLogit)
  # When log = TRUE, toc() pushes the measured timing to a list
  # quiet = TRUE prevents from printing the timing
  toc(log = TRUE, quiet = TRUE)
}

tictoc_list <- tic.log(format=FALSE)

lm_timings <- unlist(lapply(tictoc_list, function(x) x$toc - x$tic))
lm_timings <- as_tibble(lm_timings) %>% mutate(model = "Linear Model")

tic.clearlog()
rm(x, tictoc_list)
gc()

for (x in 1:1000) {
  # passing x to tic() makes it a label at time of the matching toc() call.
  tic(x)
  glm(y ~ x, data=fakeLogit, family=binomial(link="logit"))
  # When log = TRUE, toc() pushes the measured timing to a list
  # quiet = TRUE prevents from printing the timing
  toc(log = TRUE, quiet = TRUE)
}


tictoc_list <- tic.log(format=FALSE)

glm_timings <- unlist(lapply(tictoc_list, function(x) x$toc - x$tic))
glm_timings <- as_tibble(glm_timings) %>% mutate(model = "Generalized Linear Model")

bind_rows(lm_timings, glm_timings) %>%
  saveRDS(., file="logistic-regression/run-times.rds")
