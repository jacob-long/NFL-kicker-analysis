
# If pbp_f.rds exists
pbp_f <- readRDS("pbp_f.RDS")
# Otherwise run code in data_prep.R as needed. Note that for 
# source("data_prep.R") to work, you will need some saved objects
# in the working directory. If not, you'll need to uncomment some
# code in that file

library(brms)

# Fitting a series of models. Note that running all these will take several
# hours, at least.

#### Quadratic term ########################################################
## Using default improper priors
mod0 <- brm(
  made ~
    poly(kick_distance, 2) + poly(kick_distance, 2):scale(wind) + 
    scale(wind) + scale(temp) + scale(temp):poly(kick_distance, 2) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 2) + replacement:late_tga +
    (scale(wind) + poly(kick_distance, 2) | stadium) +
    (poly(kick_distance, 2) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Calculate LOO for model comparison
mod0 <- add_criterion(mod0, "loo")

## Using proper priors (hence "p")
mod0p <- brm(
  made ~
    poly(kick_distance, 2) + poly(kick_distance, 2):scale(wind) +
    scale(wind) + scale(temp) + scale(temp):poly(kick_distance, 2) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 2) + replacement:late_tga +
    (scale(wind) + poly(kick_distance, 2) | stadium) +
    (poly(kick_distance, 2) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  prior = c(prior(normal(0, 25), class = "b"), 
            prior(normal(0, 5), class = "sd")),
  sample_prior = "yes",
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Calculate LOO for model comparison
mod0p <- add_criterion(mod0p, "loo")

### 3rd degree polynomial term ################################################
## Default improper priors, interactions with full degree polynomial
mod1 <- brm(
  made ~
    poly(kick_distance, 3) + poly(kick_distance, 3):wind + wind +
    temp + temp:poly(kick_distance, 3) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 3) + replacement:late_tga +
    (wind + poly(kick_distance, 3) | stadium) +
    (poly(kick_distance, 3) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Calculate LOO for model comparison
mod1 <- add_criterion(mod1, "loo")

## Default improper priors, interactions with 2nd degree polynomial terms
mod12 <- brm(
  made ~
    poly(kick_distance, 3) + poly(kick_distance, 2):wind + wind +
    temp + temp:poly(kick_distance, 2) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 2) + replacement:late_tga +
    (wind + poly(kick_distance, 3) | stadium) +
    (poly(kick_distance, 3) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Calculate LOO for model comparison
mod12 <- add_criterion(mod12, "loo")

## Proper priors, interactions with full degree polynomial
mod1p <- brm(
  made ~
    poly(kick_distance, 3) + poly(kick_distance, 3):scale(wind) + 
    scale(wind) + scale(temp) + scale(temp):poly(kick_distance, 3) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 3) + replacement:late_tga +
    (scale(wind) + poly(kick_distance, 3) | stadium) +
    (poly(kick_distance, 3) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, 
  chains = 2, refresh = 50, cores = 4,
  prior = c(prior(normal(0, 25), class = "b"), 
            prior(normal(0, 5), class = "sd")),
  sample_prior = "yes",
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Calculate LOO for model comparison
mod1p <- add_criterion(mod1p, "loo")

## Proper priors, interactions with 2nd degree polynomial terms
mod12p <- brm(
  made ~
    poly(kick_distance, 3) + poly(kick_distance, 2):scale(wind) +
    scale(wind) + scale(temp) + scale(temp):poly(kick_distance, 2) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 2) + replacement:late_tga +
    (scale(wind) + poly(kick_distance, 3) | stadium) +
    (poly(kick_distance, 3) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  prior = c(prior(normal(0, 25), class = "b"), 
            prior(normal(0, 5), class = "sd")),
  sample_prior = "yes",
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Add LOO for model comparison
mod12p <- add_criterion(mod12p, "loo")

### 4th degree polynomial term ################################################
## Default improper prior, interactions with full degree polynomial
mod2 <- brm(
  made ~
    poly(kick_distance, 4) + poly(kick_distance, 4):wind + wind +
    temp + temp:poly(kick_distance, 4) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 4) + replacement:late_tga +
    (wind + poly(kick_distance, 4) | stadium) +
    (poly(kick_distance, 4) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Add LOO for model comparison
mod2 <- add_criterion(mod2, "loo")

## Default improper prior, interactions with second degree polynomial
mod22 <- brm(
  made ~
    poly(kick_distance, 4) + poly(kick_distance, 2):wind + wind +
    temp + temp:poly(kick_distance, 2) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 2) + replacement:late_tga +
    (wind + poly(kick_distance, 4) | stadium) +
    (poly(kick_distance, 4) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Add LOO for model comparison
mod22 <- add_criterion(mod22, "loo")

## Proper priors, interactions with full degree polynomial terms
mod2p <- brm(
  made ~
    poly(kick_distance, 4) + poly(kick_distance, 4):scale(wind) + 
    scale(wind) + scale(temp) + scale(temp):poly(kick_distance, 4) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 4) + replacement:late_tga +
    (scale(wind) + poly(kick_distance, 4) | stadium) +
    (poly(kick_distance, 4) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  prior = c(prior(normal(0, 25), class = "b"), 
            prior(normal(0, 5), class = "sd")),
  sample_prior = "yes",
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Add LOO for model comparison
mod2p <- add_criterion(mod2p, "loo")

## Proper priors, interactions with 2nd degree polynomial terms
mod22p <- brm(
  made ~
    poly(kick_distance, 4) + poly(kick_distance, 2):scale(wind) +
    scale(wind) + scale(temp) + scale(temp):poly(kick_distance, 2) +
    dome + rain + snow + fog + kicking_team + poly(season, 2) +
    iced + iced * late_tga + playoffs.x * late_tga +
    replacement + replacement:poly(kick_distance, 2) + replacement:late_tga +
    (scale(wind) + poly(kick_distance, 4) | stadium) +
    (poly(kick_distance, 4) + poly(season, 2) || kicker_player_id), 
  data = pbp_f, family = bernoulli, iter = 2000, chains = 2, refresh = 50,
  cores = 4,
  prior = c(prior(normal(0, 25), class = "b"), 
            prior(normal(0, 5), class = "sd")),
  sample_prior = "yes",
  control = list(adapt_delta = 0.80, max_treedepth = 10)
)
# Add LOO for model comparison
mod22p <- add_criterion(mod22p, "loo")

# Create LOO comparison table
loos <- loo_compare(mod0, mod0p, mod1, mod12, mod1p, mod12p, mod2, mod22, mod2p, mod22p)
loos <- as.data.frame(loos)
loos$model <- rownames(loos)

# Create stacking weights
m_weights <-
  model_weights(mod0, mod0p, mod1, mod12, mod1p, mod12p, mod2, mod22, mod2p, mod22p, cores = parallel::detectCores())
m_weights_df <- as.data.frame(m_weights)
m_weights_df$model <- rownames(m_weights_df)

# Make list of models for convenience
mod_list <- list(mod0, mod0p, mod1, mod12, mod1p, mod12p, mod2, mod22, mod2p, mod22p)

# Calculate Brier scores for each  model
brier_scores <- sapply(
  mod_list,
  function(x) {
    mean((pbp_f$made - fitted(x)[, "Estimate"])^2)
  }
)

# Calculate AUC for each model
library(pROC)
aucs <- sapply(
  mod_list,
  function(x) {
    roc <- roc(pbp_f$made, fitted(x)[, "Estimate"])
    auc(roc)
  }
)

names(aucs) <- c("mod0", "mod0p", "mod1", "mod12", "mod1p", "mod12p", "mod2",
                 "mod22", "mod2p", "mod22p")
aucs <- as.data.frame(aucs) 
aucs$model <- rownames(aucs)

names(brier_scores) <- c("mod0", "mod0p", "mod1", "mod12", "mod1p", "mod12p",
                         "mod2", "mod22", "mod2p", "mod22p")
brier_scores <- as.data.frame(brier_scores)
brier_scores$model <- rownames(brier_scores)

# Now let's put these together
library(dplyr)
loos <- inner_join(loos, m_weights_df, by = "model")
loos <- inner_join(loos, brier_scores, by = "model")
loos <- inner_join(loos, aucs, by = "model")

library(stringr)
library(magrittr)
loos %<>%
  mutate(
    poly_degree = case_when(
      str_detect(model, "mod0") ~ 2,
      str_detect(model, "mod1") ~ 3,
      str_detect(model, "mod2") ~ 4,
      TRUE ~ NA_real_
    ),
    int_degree = case_when(
      str_detect(model, "mod\\d2") ~ 2,
      TRUE ~ poly_degree
    ),
    prior_type = case_when(
      str_detect(model, "p") ~ "Proper",
      TRUE ~ "Improper"
    )
  )

# Save summary table to use in post
saveRDS(loos, "loos.RDS")

## Not used, too much of a pain in the ass
# library(mgcv)
# mod_gam <- gam(
#   made ~
#     # Global smooth
#     s(kick_distance, m = 2, k = 5) +
#     # Random slopes for global smooth
#     s(kick_distance, kicker_player_id, m = 1, k = 5, bs = "fs") +
#     s(kick_distance, stadium, m = 1, k = 5, bs = "fs") +
#     # Interaction with wind speed
#     s(kick_distance, wind, m = 1, k = 5) +
#     # Interaction with temperature
#     s(kick_distance, temp, m = 1, k = 5) +
#     # Interaction with replacement level
#     s(kick_distance, by = replacement, m = 1, k = 5, bs = "fs") +
#     # Global season smooth
#     s(season, m = 2, k = 5) +
#     # Random season slopes 
#     s(season, kicker_player_id, k = 5, m = 1, bs = "fs") +
#     # Linear predictors
#     dome + rain + snow + fog + kicking_team + iced + iced * late_tga +
#     playoffs.x * late_tga + replacement + replacement:late_tga, 
#   data = pbp_f, family = binomial, method = "REML"
# )
