library(bayestestR)
library(dplyr)
library(magrittr)
library(brms)
library(tidybayes)
library(modelr)
library(Metrics)
library(ggplot2)
library(bayesplot)
library(ggpubr)
# load the model
mod = readRDS("./model/fitted.rds")
modelDat = read.csv('../1-taxiTrips/data/modelData/modelData.csv')
# draw posterior
post = as_draws_df(mod, add_chain = T)

# Check the trace plot
post %>% 
  select(-lp__) %>% 
  tidyr::gather(key, value, -.chain, -.iteration) %>% 
  mutate(chain = as.character(.chain)) %>% 
  ggplot(aes(x = .iteration, y = value, group = .chain, color = .chain)) +
  geom_line(size = 1/15) +
  ylab(NULL) +
  theme(legend.position  = c(.825, .06),
        legend.direction = "horizontal") +
  facet_wrap(~key, ncol = 3, scales = "free_y")

# Rhat
mod_rhats = rhat(mod)
mcmc_rhat(mod_rhats) + yaxis_text(hjust = 0)
# Check rhat
rhatPlot = mod %>% 
  rhat %>%
  data.frame() %>% 
  tibble::rownames_to_column() %>% 
  purrr::set_names("parameter", "rhat") %>% 
  mutate(parameter = factor(parameter, levels = rev(colnames(post)[1:14]))) %>%
  ggplot(aes(x = rhat, y = parameter)) + 
  geom_segment(aes(xend = 1, yend = parameter),
               color = "#EEDA9D") +
  geom_point(aes(color = rhat < 1.05), 
             size = 2) +
  scale_color_manual(values = c("#80A0C7", "#A65141")) +
  labs(x = NULL, y = NULL, title = expression( hat(R) )) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_text(hjust = 0))

# Check effective sample size
mod_neff = neff_ratio(mod)
neffPlot = mod %>%
  neff_ratio %>%
  data.frame %>%
  tibble::rownames_to_column() %>% 
  purrr::set_names("parameter", "neff") %>%
  mutate(parameter = factor(parameter, levels = rev(colnames(post)[1:14]))) %>%
  ggplot(aes(x = neff, y = parameter)) + 
  geom_segment(aes(xend = 0, yend = parameter),
               color = "#EEDA9D") +
  geom_point(aes(color = neff > .5), 
             size = 2) +
  scale_color_manual(values = c("#80A0C7", "#A65141")) +
  labs(x = NULL, y = NULL, title = "Number of Effective Sample Size") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_text(hjust = 0))

# Present posterior predictive check
ggarrange(
  pp_check(mod, ndraws = 100) + yaxis_text(hjust = 0),
  pp_check(mod, type = "ecdf_overlay", ndraws = 100) + yaxis_text(hjust = 0),
  ncol = 2, common.legend = T
)

# Posterior distribution of model parameters
community_label = c("North Loop (CA 8)", "West Loop (CA 28)", "The Loop (CA 32)")

rbind(coef(mod)$ca[, , 1],
      coef(mod)$ca[, , 2]) %>%
  as_tibble() %>%
  mutate(param = c(
    paste(community_label),
    paste(community_label)
  ),
  reorder = c(3:1, 6:4)) %>%
  mutate(coef = rep(c("Intercept", "Coefficient"), each = 3) %>% factor) %>%
  mutate(coef = factor(coef, levels = c("Intercept", "Coefficient"))) %>%
  # plot
  ggplot(aes(x = reorder(param, reorder))) +
  geom_hline(yintercept = 0,
             linetype = 3,
             color = "#4E2A84") +
  geom_pointrange(
    aes(
      ymin = Q2.5,
      ymax = Q97.5,
      y = Estimate,
      color = coef
    ),
    shape = 20,
    size = 3 / 4
  ) +
  scale_color_manual(values = c("#FDB515", "#003262")) +
  xlab(NULL) +
  facet_grid(cols = vars(coef), scales = "free") +
  coord_flip() +
  theme_linedraw() +
  theme(
    legend.position = "none",
    axis.ticks.y    = element_blank(),
    axis.text.y     = element_text(hjust = 0)
  )

# Prediction Interval
get_variables(mod)
set.seed(2333)
predictionDF = mod$data %>%
  mutate(CA = recode_factor(ca, 
                            `8` = "North Loop", 
                            `28` = "West Loop", 
                            `32` = "The Loop")) %>%
  group_by(ca, CA) %>%
  data_grid(flow = seq_range(flow, n = 200)) %>%
  add_epred_draws(mod, ndraws = 100) %>%
  mutate(prob = .epred / flow)
  
refMax = mod$data %>% 
  group_by(ca) %>%
  summarise(maxFlow = max(flow)) %>%
  rename(flow = maxFlow) %>%
  add_epred_draws(mod) %>% 
  group_by(ca) %>%
  summarise(flow = mean(flow), pickup = mean(.epred))

refMin = mod$data %>% 
  group_by(ca) %>%
  summarise(minFlow = min(flow)) %>%
  rename(flow = minFlow) %>%
  add_epred_draws(mod) %>% 
  group_by(ca) %>%
  summarise(flow = mean(flow), pickup = mean(.epred))

epredDF = data.frame(ca = rep(c(8, 28, 32), each = 500),
                     flow = rep(1:500, times = 3)) %>% 
  add_epred_draws(mod, ndraws = 100)