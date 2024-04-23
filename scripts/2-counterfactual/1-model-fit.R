library(dplyr)
library(magrittr)
library(brms)
library(tidybayes)
library(modelr)
library(Metrics)
library(ggplot2)
library(bayesplot)
# Load the model data
modelDat = read.csv("../1-taxiTrips/data/modelData/modelData.csv") %>%
  rename("ca" = "nextLoc", "pickup" = "pickups")

# Check the prior
get_prior(pickup ~ 1 + flow + (1 + flow | ca),
          data = modelDat, 
          family = gaussian())

mod = brm(
  pickup ~ 1 + flow + (1 + flow | ca),
  prior = c(
    prior(student_t(3, 60, 10), class = "Intercept"),
    prior(student_t(3, 0, 15),  class = "b"),
    prior(cauchy(0, 15), class = "sigma"),
    prior(student_t(3, 75, 20), class = "sd", coef = "Intercept", group = "ca"),
    prior(exponential(2), class = "sd", coef = "flow", group = "ca"),
    prior(lkj(5), class = "cor")
  ),
  data = modelDat,
  chains = 4,
  cores = getOption("mc.cores", 4),
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 13),
  seed = 666,
  file = "./model/model/fitted",
)
