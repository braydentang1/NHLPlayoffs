library(tidyverse)
library(glmnet)
library(recipes)
library(caret)
library(parallel)
library(ParBayesianOptimization)

source("src/prediction/R/final-model_required-functions.R")

#..................................Read data in....................................#

cat("Reading in Data..... \n")
all_data <- read_csv("data/processed/2006-2019_hockey-reference_other.csv") %>%
  bind_cols(read_csv("data/processed/2006-2019_hockey-reference_aggregated.csv")) %>%
  bind_cols(read_csv("data/processed/2008-2019_corsica_all-team-stats_processed.csv")) %>%
  bind_cols(read_csv("data/processed/2008-2019_corsica_game-score.csv")) %>%
  bind_cols(read_csv("data/processed/2006-2019_elo-ratings.csv")) %>%
  bind_cols(read_csv("data/processed/2006-2019_espn_stats.csv")) %>%
  bind_cols(read_csv("data/processed/2008-2019_naturalstattrick_scf.csv")) %>%
  bind_cols(read_csv("data/processed/2006-2019_nhl-official.csv")) %>%
  bind_cols(read_csv("data/processed/2006-2019_puck-on-net_last20.csv")) %>%
  bind_cols(read_csv("data/processed/2006-2019_oddsportal.csv")) %>%
  bind_cols(read_csv("data/processed/2008-2019_evolving-hockey_WAR.csv")) %>%
#  bind_cols(read_csv("data/processed/2008-2019_naturalstattrick_time-related.csv")) %>%
  mutate(result_factor = as.factor(result_factor)) %>%
  filter(!is.na(result_factor)) %>%
  select(-series) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all(" ", "_") %>%
              str_replace_all("%", "_percent") %>%
              str_replace_all("/", "_per"))


#...................................Engineering of some features..................#

all_data <- all_data %>% 
  mutate(round = as.factor(c(rep(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4), 14)))) %>%
  mutate(penalty_mins_to_powerplay_log = sign(penalty_mins_pg * 60 * 82 / powerplay_percentage) * log(abs(penalty_mins_pg * 60 * 82 / powerplay_percentage) + 1)) %>%
  mutate(ratio_of_srs_to_points = (srs / points)^1/3) %>%
  mutate(power_play_to_penaltykill = sign(powerplay_percentage / penaltykill_percentage) * log(abs(powerplay_percentage / penaltykill_percentage) + 1)) %>%
  mutate(ppo_x_penaltykill = powerplay_oppurtunities * penaltykill_percentage) %>%
  mutate(gs_max_log = sign(gs_mean) * log(abs(gs_mean) + 1)) %>%
  mutate(ca_per60team_log = sign(ca_per60team) * log(abs(ca_per60team) + 1)) %>%
  mutate(ratio_of_goals_to_goals_against_log = sign(goals_for / goals_against) * log(abs(goals_for / goals_against) +1)) %>%
  mutate(ratio_of_hits_to_blocks_log = sign(hits_at_es / blocks_at_es) * log(abs(hits_at_es / blocks_at_es) + 1)) %>%
  mutate(scf_to_goalsagainst_log = sign(scf / goals_against) * log(abs(scf / goals_against) + 1)) %>%
  mutate(corsidiff_to_sos_log = sign((cf_per60team - ca_per60team) / sos) * log(abs((cf_per60team - ca_per60team) / sos) + 1)) %>%
  mutate(xgdiff_to_sos = (xgf_60 - xga_60) / sos) %>% 
  mutate(gs_to_sos = gs_mean / sos) %>%
  mutate(srs_to_sos = srs / sos) %>%
  mutate(rel_cf_max_to_ixgf60_max = rel_cf_percent_max / ixgf_per60_max) %>%
  mutate_if(is.numeric, .funs = list(~ifelse(is.nan(.), 0, .))) %>%
  mutate_if(is.numeric, .funs = list(~ ifelse(is.infinite(.), 0, .))) 

#..................................Separate out the new observations from all prior data........................#

new_data <- all_data %>% filter(is.na(result_factor))
all_data <- all_data %>% filter(!is.na(result_factor))

#...........................Preprocess all the data; give the recipe parameters for all newdata........#

pre_process_data <- function(data) {
  
  #' Pre processes the specified data as done in model validation.
  #'
  #' @param data The dataset to preprocess.
  #'
  #' @return A list containing the preprocessed dataset as well as the learned parameters needed to preprocess new data.
  #' @export
  #'
  
  recipe_parameters <- prep(pre_process_recipe(data), training = data)
  
  processed_data_set <- bake(recipe_parameters, new_data = data) 
  
  list(data = processed_data_set, recipe_parameters = recipe_parameters)
}

#...........................Prediction Script.................................................................................#
#Requires new data samples. 

predict_NHL <- function(processed_data_set, recipe_parameters, new_data, final_parameters, times = 20L) {
  
  #' Generates new predictions on new_data.
  #'
  #' @param processed_data_set A processed training set resulting from a call to the function pre_process_data.
  #' @param recipe_parameters The learned parameters for preprocessing, resulting from a call to the function pre_process_data.
  #' @param new_data A tibble containing the new observations to make predictions for.
  #' @param final_parameters A tibble with two columns alpha and lambda that give the hyperparameters for each 
  #'  bagged elastic net model used in the ensemble.
  #' @param times An integer vector of length one that gives the number of bootstrap resamples to use per each fitted bagged elastic net model.
  #'
  #' @return A list containing a tibble containing predicted probabilities for the observations in new_data, and a tibble with variable
  #'  importance scores derived from the fitted model.
  #' @export
  #'
  
  new_data <- new_data %>% 
    select(-result_factor) %>% 
    bake(recipe_parameters, new_data = .)
  
  frames_with_pca <- add_pca_variables(train_data = processed_data_set, test_data = new_data, standardize = FALSE)
  
  processed_data_set <- frames_with_pca$train
  new_data <- frames_with_pca$test
  
  rm(frames_with_pca)
  
  frames_with_knn <- add_knn_variables(train_data = processed_data_set, test_data = new_data, distances = TRUE, include_PCA = FALSE)
  
  processed_data_set <- frames_with_knn$train
  new_data <- frames_with_knn$test
  
  rm(frames_with_knn)
  
  predictions <- vector("list", nrow(final_parameters))
  var_imp <- vector("list", nrow(final_parameters))
  
  for (i in 1:nrow(final_parameters)) {
    
    model <- bagged_model(
      train = processed_data_set,
      test = new_data, 
      label_train = processed_data_set$result_factor, 
      alpha.a = finalParameters$alpha[i],
      s_lambda.a = finalParameters$lambda[i], 
      calibrate = FALSE)
    
    predictions[[i]] <- model$predictions
    var_imp[[i]] <- model$variable_importance
    
  }
  
  final_predictions_processed <- predictions %>%
    reduce(cbind) %>% 
    rowMeans(.)
  
  final_var_imp_processed <- var_imp %>% 
    reduce(left_join, by = "variable") %>%
    process_var_imp(.)
  
  list(prediction = final_predictions_processed, variable_importance = final_var_imp_processed)
}

#...........................Global.........................................#

set.seed(40689)
inner_folds <- caret::createMultiFolds(y = all_data$result_factor, k = 3, times = 5)

#...............If you want to retrain the model, uncomment this and run.................#

# final_parameters <- mclapply(X = 1:5, FUN = train_model_one_rep, inner_folds = inner_folds, main_train = all_data) %>%
#   bind_rows(.)
# 
# saveRDS(final_parameters, file = "src/prediction/rds/final-parameters.rds")
# 
# processed_data <- pre_process_data(data = all_data)
# saveRDS(processed_data, file = "src/prediction/rds/processed-data.rds")

#.................Otherwise, run this instead..................................#

final_parameters <- readRDS("src/prediction/rds/final-parameters.rds")
processed_data <- readRDS("src/prediction/rds/processed-data.rds")

#.................Find, and then present final predictions....................................#
 
predictions <- predict_NHL(
  processed_data_set = processed_data$data,
  recipe_parameters = processed_data$recipe_parameters, 
  new_data = new_data, 
  final_parameters = final_parameters)

template <- read_csv("src/scraping/templates/template.csv") %>%
  filter(Year == 2019, Round == "stanley-cup-final") %>%
  select(Team1, Team2, Highest.Seed)

final_scores <- template %>%
  bind_cols(., prob_win_highest_seed = predictions$prediction)