# Barbecue Propane Use data generation and splitting
rm(list = ls())
set.seed(5072024)

library(tidyverse)
library(tidymodels)

# Number of observations
n_obs <- 30000

# Create the dataset
bbq_data <- tibble(
  location = sample(c("backyard", "beach", "campsite"), prob = c(0.60, 0.10, 0.30), size = n_obs, replace = TRUE),
  size_est = sample(c("small", "large"), prob = c(0.78, 0.22), size = n_obs, replace = TRUE),
  size_actual = case_when(
    size_est == "small" ~ rpois(n_obs, lambda = 9),
    size_est == "large" ~ rpois(n_obs, lambda = 42)
  ),
  charcoal_present = sample(c(0, 1), size = n_obs, replace = TRUE),
  smoker_present = sample(c(0, 1), prob = c(0.8, 0.2), size = n_obs, replace = TRUE),
  salads = NA,
  burgers = sample(c(0, 1), size = n_obs, replace = TRUE, prob = c(0.17, 0.83)),
  hotdogs = sample(c(0, 1), size = n_obs, replace = TRUE, prob = c(0.06, 0.94)),
  chicken = sample(c(0, 1), size = n_obs, replace = TRUE, prob = c(0.38, 0.62)),
  beyond_items = sample(c(0, 1), size = n_obs, replace = TRUE, prob = c(0.72, 0.28)),
  propane_used = 0  # initialize propane_used
)

for(i in 1:nrow(bbq_data)){
  if(bbq_data[i, "size_est"] == "small"){
    bbq_data[i, "salads"] <- bbq_data[i, "size_actual"]*rnorm(1, 0.64, 0.15)
  } else{
    bbq_data[i, "salads"] <- bbq_data[i, "size_actual"]*rnorm(1, 0.75, 0.1)
  }
}

# Generate propane_used based on specified associations
bbq_data <- bbq_data %>%
  mutate(
    propane_used = 50 +  # base propane usage
      (0.1 + rnorm(nrow(bbq_data), 0, 0.02)) * size_actual +  # linear effect of size_actual
      -0.005 * size_actual^2 +  # quadratic effect of size_actual
      -20 * smoker_present +  # effect of smoker_present
      -10 * charcoal_present +  # effect of charcoal_present
      case_when(
        location == "beach" ~ -30,
        location == "backyard" ~ -10,
        TRUE ~ 0
      ) +  # effect of location
      0.2 * salads +  # effect of salads
      -5 * burgers +  # effect of burgers
      -5 * hotdogs +  # effect of hotdogs
      5 * chicken +  # effect of chicken
      -10 * beyond_items +  # effect of beyond_items
      rnorm(n_obs, mean = 0, sd = 10)  # add some noise
  )

# Display the first few rows of the dataset
head(bbq_data)

###################################################
##Test how Easy -- ~90% accuracy with un-tuned RF##
###################################################
propane_folds <- vfold_cv(bbq_data)

lin_reg_spec <- linear_reg()
lin_reg_rec <- recipe(propane_used ~ ., data = bbq_data) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

lin_reg_wf <- workflow() %>%
  add_model(lin_reg_spec) %>%
  add_recipe(lin_reg_rec)
lin_reg_results <- lin_reg_wf %>%
  fit_resamples(propane_folds)

lin_reg_results %>%
  collect_metrics(summarize = FALSE)
lin_reg_results %>%
  collect_metrics()


####################################################
##Decompose Competition Data Files
####################################################
set.seed(8092023)
monster_splits <- initial_split(monsters_df, prop = 0.75, strata = class)
monster_train <- training(monster_splits)
monster_comp <- testing(monster_splits)

monster_train <- monster_train %>%
  slice_sample(n = monster_train %>% nrow(), replace = FALSE) %>%
  mutate(ID = row_number()) %>%
  select(ID, everything())

monster_comp <- monster_comp %>%
  slice_sample(n = monster_comp %>% nrow(), replace = FALSE) %>%
  mutate(ID = row_number()) %>%
  select(ID, everything())  

monster_key <- monster_comp %>%
  mutate(Usage = sample(c("Public", "Private"), prob = c(0.3, 0.7), size = monster_comp %>% nrow(), replace = TRUE)) %>%
  select(ID, Usage, class)
sample_sub <- monster_comp %>%
  select(ID) %>%
  mutate(class = "Zombie")
monster_comp <- monster_comp %>%
  select(-class)

write.csv(monster_train, "G:/My Drive/MAT434/Datasets/monster_train.csv", row.names = FALSE)
write.csv(monster_comp, "G:/My Drive/MAT434/Datasets/monster_comp.csv", row.names = FALSE)
write.csv(monster_key, "G:/My Drive/MAT434/Datasets/monster_key.csv", row.names = FALSE)
write.csv(sample_sub, "G:/My Drive/MAT434/Datasets/sample_sub.csv", row.names = FALSE)
