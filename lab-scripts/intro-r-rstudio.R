#' ---
#' title: "An Intro to R, Rstudio, and '{tidyverse}'"
#' author: Steven V. Miller
#' output:
#'    html_document:
#'      toc: TRUE
#'      highlight: zenburn
#' ---

#' # Configure Rstudio
#' 
#' When you're opening R for the very first time, it'll be useful to just get a general sense of what's happening.
#' I have [a beginner's guide that I wrote in 2014](http://svmiller.com/blog/2014/08/a-beginners-guide-to-using-r/) (where did the time go!). Notice that I built it around
#' [Rstudio](https://rstudio.com/products/rstudio/), which you should download as well. Rstudio desktop is free. Don't pay for a "pro" version. 
#' You're not running a server. You won't need it. 
#' 
#' When you download and install Rstudio *on top* of R, you should customize it just a tiny bit to make the most of the graphical user interface. To do what I recommend doing,
#' select "Tools" in the menu. Scroll to "global options" (which should be at the bottom). On the pop-up, select "pane layout." Rearrange it so that "Source" is top left, "Console"
#' is top right, and the files/plots/packages/etc. is the bottom right. Thereafter: apply the changes.
#' 
#' ![](http://post8000.svmiller.com/intro-r-rstudio/rstudio-global-options.png)
#' 
#' You don't have to do this, but I think you should since it better economizes space in Rstudio. The other pane (environment/history, Git, etc.) is stuff you can either learn to not
#' need (e.g. what's in the environment) or will only situationally need at an advanced level (e.g. Git information). Minimize that outright. When you're in Rstudio, much of what you'll
#' be doing leans on the script window and the console window. You'll occasionally be using the file browser and plot panes as well.
#' 
#' If you have not done so already, open a new script (Ctrl-Shift-N in Windows/Linux or Cmd-Shift-N in Mac) to open a new script.
#' 
#' # Get Acclimated in R
#' 
#' Now that you've done that, let's get a general sense of where you are in an R session. 
#' 
#' ## Current Working Directory
#' 
#' First, let's start with identifying the current working directory. You should know where you are and this happens to be where I am, given the location of this script.

getwd()

#' Of note: by default, R's working directory is the system's "home" directory. This is somewhat straightforward in Unix-derivative systems, where there is an
#' outright "home" directory. Assume your username is "steve", then, in Linux, your home directory will be "/home/steve". In Mac, I think it's something like
#' "/Users/steve". Windows users will invariably have something clumsy like "C:/Users/steve/Documents". Notice the forward slashes. R, like everything else in the world,
#' uses forward slashes. The backslashes owe to Windows' derivation from DOS.
#' 
#' ## Create "Objects"
#' 
#' Next, let's create some "objects." R is primarily an "object-oriented" programming language. 
#' In as many words, inputs create outputs that may be assigned to objects in the workspace. You can go nuts here. Of note: I've seen R programmers use "=", "-> "<-" 
#' interchangeably for object assignment, but I've seen instances where "=" doesn't work as intended for object assignment. "->" is an option and I use it for assignment
#' for some complex objects in a "pipe" (more on that later). For simple cases (and for beginners), lean on "<-".
#' 

a <- 3
b <- 4 
A <- 7
a + b
A + b

# what objects did we create?
# Notice we did not save a + b or A + b to an object
# Also notice how a pound sign creates a comment? Kinda cool, right? Make comments to yourself.
ls()

#' Some caution, though. First, don't create objects with really complex names. To call them back requires getting every character right in the console or script. 
#' Why inconvenience yourself? Second, R comes with some default objects that are kinda important and can seriously ruin things downstream. I don't know off the top of my head
#' all the default objects in R, but there are some important ones like `pi`, `TRUE`, and `FALSE` that you DO NOT want to overwrite. You can, however, assign some built-in objects
#' to new objects.

this_Is_a_long_AND_WEIRD_objEct_name_and_yOu_shoUld_not_do_this <- 5
pi # notice there are a few built-in functions/objects
d <- pi # you can assign one built-in object to a new object.
# pi <- 3.14 # don't do this....

#' If you do something dumb (like overwrite `TRUE` with something), all hope is not lost. However, your session is. Restart R and you'll reclaim some built-in object that you overwrote.
#' 
#' ## Install/Load Libraries
#' 
#' R depends on user-created libraries to do much of its functionality. This class will lean on just a few R libraries. The first, `{tidyverse}` is our workhorse for workflow.
#' It'll also be the longest to install because it comes with lots of dependencies to maximize its functionality. The second, `{devtools}`, is my go-to interface for downloading
#' development packages off Github (and I'll eventually ask you to download/load my toy R package, `{stevemisc}`). My hunch is installation of this package will probably give some of you
#' Mac users some headaches. The answer to these headaches is probably "update Xcode".  The third package includes a whole host of toy data I've created over time. `{stevedata}` is
#' available on CRAN.

#' If you have yet to install these packages (and you almost certainly have not if you're opening R for the first time), install it as follows.
#' Note that I'm just commenting out this command so it doesn't do this when I compile this script on my end. 
# Take out the comment...
# install.packages(c("tidyverse","devtools", "stevedata"))

#' Once it's installed, load the libraries with the `library()` command. Of note: you only need to install a package once, but you'll need to load the library for each
#' R session. A caveat, though: I *may* ask you to upgrade `{stevedata}` at some point in the semester (and probably through Github) if I add a new data set for the lab session.
#' 
library(tidyverse)
library(devtools)
library(stevedata)


#' ## Load Data
#' 
#' The next thing you'll want to do is load data into R (and assign it to an object). You can do this any number of ways. For what it's worth, the toy data I'll be using
#' throughout the semester will (ideally) all be in `{stevedata}`. The data I'll be using down the script is built into `{stevedata}`. However, you can load data from the hard drive or 
#' even the internet. Some commands you'll want to learn:
#' 
#' - `haven::read_dta()`: for loading Stata .dta files
#' - `haven::read_spss()`: for loading SPSS binaries
#' - `read_csv()`: for loading comma-separated values (CSV) files
#' - `readxl::read_excel()`: for loading MS Excel spreadsheets.
#' - `read_tsv()`: for tab-separated values (TSV) files
#' - `readRDS()`: for R serialized data frames, which are awesome for file compression/speed.

#' These wrappers are also flexible with files on the internet. For example, these will work. Just remember to assign them to an object.

# Note: hypothetical data
Apply <- haven::read_dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
# County unemployment
Cunemp <- read_tsv("https://download.bls.gov/pub/time.series/la/la.data.64.County") 


#' ## Learn Some Important R/"Tidy" Functions
#' 
#' Some R packages, like my `{stevedata}` package, have built-in data. For example, my `pwt_sample` data is just a sample of 21 rich countries with information about 
#' population size (in millions), human capital per person, real GDP at constant prices (million 2011 USD), and the labor share of income for years spanning 1950 to 2017.
#' Let's make sure the data are loaded in the environment (they are by default) with the `data()` command and see the variable names in these data.

data(pwt_sample)
names(pwt_sample)

#' You can also type `help(pwt_sample)` in the R console to learn more about these data.
#' 
#' I want to dedicate the bulk of this section to learning some core functions that are part of the `{tidyverse}`. My introduction here will inevitably be incomplete because
#' there's only so much I can teach within the limited time I have. That said, I'm going to focus on the following functions available in the `{tidyverse}` that totally rethink
#' base R. These are the "pipe" (`%>%`), `glimpse()` and `summary()`, `select()`, `group_by()`, `summarize()`, `mutate()`, and `filter()`.
#' 
#' ### The Pipe (%>%)
#' 
#' I want to start with the pipe because I think of it as the most important function in the `{tidyverse}`. The pipe---represented as `%>%`---allows you to chain together
#' a series of functions. The pipe is especially useful if you're recoding data and you want to make sure you got everything the way you wanted (and correct) before assigning
#' the data to another object. You can chain together *a lot* of `{tidyverse}` commands with pipes, but we'll keep our introduction here rather minimal because I want to use it
#' to teach about some other things.
#' 
#' ### `glimpse()` and `summary()`
#' 
#' `glimpse()` and `summary()` will get you basic descriptions of your data. Personally, I find `summary()` more informative than `glimpse()` though `glimpse()` is useful if your data 
#' have a lot of variables and you want to just peek into the data without spamming the R console without output. 
#' 
#' Notice, here, the introduction of the pipe (`%>%`). In the commands below, `pwt_sample %>% glimpse()` is equivalent to `glimpse(pwt_sample)`, but I like to lean more on pipes
#' than perhaps others would. My workflow starts with (data) objects, applies various functions to them, and assigns them to objects. I think you'll get a lot of mileage
#' thinking that same way too.

pwt_sample %>% glimpse() # notice the pipe
pwt_sample %>% summary()

#' ### `select()`
#' 
#' `select()` is useful for basic (but important) data management. You can use it to grab (or omit) columns from data. For example, 
#' let's say I wanted to grab all the columns in the data. I could do that with the following command.

pwt_sample %>% select(everything())  # grab everything

#' Here's if I wanted everything except wanted to drop the labor share of income variable.
#' 
pwt_sample %>% select(-labsh) # grab everything, but drop the labsh variable.

#' Here's a more typical case. Assume you're working with a large data object and you just want a handful of things. In this case, we have
#' all these economic data on these 21 countries (ed. we really don't, but roll with it), but we just want the GDP data along with the important
#' identifying information for country and year. Here's how we'd do that in the `select()` function, again with some assistance from the pipe.
pwt_sample %>% select(country, year, rgdpna) # grab just these three columns.


#' ### `group_by()`
#' 
#' I think the pipe is probably the most important function in the `{tidyverse}` even as a critical reader might note that the pipe is 1) a port from another package (`{magrittr}`) and 
#' 2) now a part of base R in a different terminology. Thus, the critical reader (and probably me, depending on my mood) may note that `group_by()` is probably the most important
#' component of the `{tidyverse}`. Basically, `group_by()` allows you to "split" the data into various subsets, "apply" various functions to them, and "combine" them into one output.
#' You might see that terminology "split-apply-combine" as you learn more about the `{tidyverse}` and its development.
#' 
#' Here, let's do a simple `group_by()` exercise, while also introducing you to another function: `slice()`. We're going to group by country in `pwt_sample` and "slice"
#' the first observation for each group/country. Notice how we can chain these together with a pipe operator.

# Notice we can chain some pipes together
pwt_sample %>%
  # group by country
  group_by(country) %>%
  # Get me the first observation, by group.
  slice(1)

#' If you don't group-by the country first, `slice(., 1)` will just return the first observation in the data set.

pwt_sample %>%
  # Get me the first observation for each country
  slice(1) # womp womp. Forgot to group_by()

#' I offer one caveat here. If you're applying a group-specific function (that you need just once), it's generally advisable to "ungroup()" (i.e. `ungroup()`)
#' as the next function in your pipe chain. As you build together chains/pipes, the intermediate output you get will advise you of any "groups" you've declared
#' in your data.

#' ### `summarize()`
#' 
#' `summarize()` creates condensed summaries of your data, for whatever it is that you want. Here, for example, is a kind of dumb way of seeing how many observations
#' are in the data. `nrow(pwt_sample)` works just as well, but alas...

pwt_sample %>%
  # How many observations are in the data?
  summarize(n = n())

#' More importantly, `summarize()` works wonderfully with `group_by()`. For example, for each country (`group_by(country)`), let's get the maximum GDP observed in the data.

pwt_sample %>%
  group_by(country) %>%
  # Give me the max real GDP observed in the data.
  summarize(maxgdp = max(rgdpna, na.rm=T))

#' One downside (or feature, depending on your perspective) to `summarize()` is that it condenses data and discards stuff that's not necessary for creating
#' the condensed output. In the case above, notice we didn't ask for what year we observed the maximum GDP for a given country. We just asked for the maximum.
#' If you wanted something that would also tell you what year that particular observation was, you'll probably want a `slice()` command in lieu of `summarize()`.
#' Observe:
#' 

pwt_sample %>%
  group_by(country) %>%
  slice(which(rgdpna == max(rgdpna, na.rm=T)))

#' This is a convoluted way of thinking about `summarize()`, but you'll probably find yourself using it a lot.
#' 
#' ### `mutate()`
#' 
#' `mutate()` is probably the most important `{tidyverse}` function for data management/recoding. It will allow you to create new columns while retaining the original
#' dimensions of the data. Consider it the sister function to `summarize()`. But, where `summarize()` discards, `mutate()` retains.
#' 
#' Let's do something simple with `mutate()`. For example, the `rgdpna` column is real GDP in million 2011 USD. What if we wanted to convert that million to billions?
#' This is simple with `mutate()`. Helpfully, you can create a new column that has both the original/raw data and a new/recoded variable. This is great for reproducibility
#' in your data management.


pwt_sample %>%
  # Convert rgdpna from real GDP in millions to real GDP in billions
  mutate(rgdpnab = rgdpna/1000)

#' Notice that `mutate()` also works beautifully with `group_by()`. For example, let's assume---for whatever reason---we wanted a new variable that divided the real GDP variable
#' over the observed maximum real GDP for a given country. I don't know know *why* we'd want that, but we could do it with `group_by()`.

pwt_sample %>%
  group_by(country) %>%
  # divide rgdpna over the country's max, for some reason.
  mutate(rgdpnaprop = rgdpna/max(rgdpna, na.rm=T))

#' ### `filter()`
#' 
#' `filter()` is a great diagnostic tool for subsetting your data to look at particular observations. Notice one little thing, especially if you're new to programming.
#' The use of double-equal signs (`==`) is for making logical statements where as single-equal signs (`=`) is for object assignment or column creation. If you're using
#' `filter()`, you're probably wanting to find cases where something equals something (`==`), is greater than something (`>`), equal to or greater than something (`>=`), is less
#' than something (`<`), or is less than or equal to something (`<=`).
#' 
#' Here, let's grab just the American observations by filtering to where `isocode` == "USA".

pwt_sample %>%
  # give me just the USA observations
  filter(isocode == "USA")

#' We could also use `filter()` to select observations from the most recent year.

pwt_sample %>%
  # give me the observations from the most recent year.
  filter(year == max(year))

#' ## Don't Forget to Assign!
#' 
#' When you're done applying functions/doing whatever to your data, don't forget to assign what you've done to an object. For simple cases, and for beginners, I recommend 
#' thinking "left-handed" and using `<-` for object assignment (as we did above). When you're doing stuff in the pipe, my "left-handed" thinking prioritizes the starting data
#' in the pipe chain. Thus, I tend to use `->` for object assignment at the end of the pipe.
#' 
#' Consider a simple example below. I'm starting with the original data (`pwt_sample`). I'm using a simple pipe to create a new variable (within `mutate()`) that standardizes
#' the real GDP variable from millions to billions. Afterward, I'm assigning it to a new object (`Data`) with `->`.

pwt_sample %>%
  # convert real GDP to billions
  mutate(rgdpnab = rgdpna/1000) -> Data
