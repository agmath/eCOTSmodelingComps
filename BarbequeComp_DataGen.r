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
    size_est == "small" ~ 3 + rpois(n_obs, lambda = 9),
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
      #(0.1 + rnorm(nrow(bbq_data), 0, 0.02)) * size_actual +  # linear effect of size_actual
      #-0.005 * size_actual^2 +  # quadratic effect of size_actual
      (10 - 0.005*(size_actual - 40)^2) +
      -20 * smoker_present +  # effect of smoker_present
      -10 * charcoal_present +  # effect of charcoal_present
      case_when(
        location == "beach" ~ -0.1*size_actual,
        location == "backyard" ~ 0.2*size_actual,
        TRUE ~ 0
      ) +  # effect of location
      0.01 * salads +  # effect of salads
      0.02 * burgers * size_actual +  # effect of burgers
      -.1 * hotdogs * size_actual +  # effect of hotdogs
      0.1 * chicken +  # effect of chicken
      -0.05 * beyond_items +  # effect of beyond_items
      rnorm(n_obs, mean = 0, sd = 1)  # add some noise
  )

for(i in 1:nrow(bbq_data)){
  if(bbq_data[i, "propane_used"] < 4){
    bbq_data[i, "propane_used"] <- (0.2 + rnorm(1, 0.05, 0.01))*bbq_data[i, "size_actual"]
  }
}

# Display the first few rows of the dataset
head(bbq_data)

# Check that minimum propane usage isn't negative
bbq_data %>%
  summarise(min(propane_used))

#I use plotting to check that the data has the "shape" I intend.
bbq_data %>%
  ggplot() +
  geom_point(aes(x = size_actual, y = propane_used, color = location)) + 
  facet_wrap(~location, nrow = 1)

bbq_data %>%
  ggplot() +
  geom_boxplot(aes(x = size_est, y = propane_used))

################################################
##Test how Easy#################################
################################################
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

rf_reg_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")
rf_reg_rec <- recipe(propane_used ~ ., data = bbq_data) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

rf_reg_wf <- workflow() %>%
  add_model(rf_reg_spec) %>%
  add_recipe(rf_reg_rec)
rf_reg_results <- rf_reg_wf %>%
  fit_resamples(propane_folds)

rf_reg_results %>%
  collect_metrics(summarize = FALSE)
rf_reg_results %>%
  collect_metrics()


####################################################
##Decompose Competition Data Files##################
####################################################
data <- bbq_data #replace with your data set
set.seed(5102024)
data_splits <- initial_split(data, prop = 0.75) #Use strata argument for classification objectives
data_train <- training(data_splits)
data_comp <- testing(data_splits)

data_train <- data_train %>%
  slice_sample(n = data_train %>% nrow(), replace = FALSE) %>%
  mutate(ID = row_number()) %>%
  select(ID, everything())

data_comp <- data_comp %>%
  slice_sample(n = data_comp %>% nrow(), replace = FALSE) %>%
  mutate(ID = row_number()) %>%
  select(ID, everything())  

data_key <- data_comp %>%
  #mutate(Usage = sample(c("Public", "Private"), prob = c(0.3, 0.7), size = monster_comp %>% nrow(), replace = TRUE)) %>%
  #uncomment line above if you want come control over what data is used for public and private leaderboards
  select(ID, propane_used) #select ID and response column

sample_sub <- data_comp %>%
  select(ID) %>%
  #set a benchmark that you expect participants to exceed
  #choose a different summary statistic, or use model-based predictions if you prefer
  mutate(propane_used = mean(propane_used))
  #mutate(class = "Zombie") #uncomment this line and use dominant class for classification

data_comp <- data_comp %>%
  select(-propane_used) #swap out for response column

write.csv(data_train, "competition_data/data_train.csv", row.names = FALSE)
write.csv(data_comp, "competition_data/data_comp.csv", row.names = FALSE)
write.csv(data_key, "competition_data/data_key.csv", row.names = FALSE)
write.csv(sample_sub, "competition_data/sample_sub.csv", row.names = FALSE)
