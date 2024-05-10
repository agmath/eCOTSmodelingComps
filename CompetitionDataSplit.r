####################################################
##Splitting Data for NOT A KAGGLE Competition
####################################################
library(tidyverse)
data <- read_csv("bbq_data.csv")

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
  mutate(propane_used = mean(propane_used)) #con
  #mutate(class = "Zombie") #uncomment this line and use dominant class for classification
data_comp <- data_comp %>%
  select(-propane_used) #swap out for response column

write.csv(data_train, "competition_data/data_train.csv", row.names = FALSE)
write.csv(data_comp, "competition_data/data_comp.csv", row.names = FALSE)
write.csv(data_key, "competition_data/data_key.csv", row.names = FALSE)
write.csv(sample_sub, "competition_data/sample_sub.csv", row.names = FALSE)
