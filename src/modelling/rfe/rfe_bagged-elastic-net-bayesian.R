source('src/modelling/rfe/rfe_functions.R')

#..................................Read data in....................................#
#Change directories to pull in data from the "Required Data Sets" folder located in the repository.

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

#..........................Global Envrionment..............................................................#
set.seed(40689)
all_seeds <- sample(1:1000000000, 42, replace = FALSE)

give_results <- function(seed, all_data, times = 20, p = 0.8, k = 3, n_iters = 25, subsets, use_only_variables = NULL) {
  
  #' Generates a bagged (bootstrapped aggregated) elastic net model given a set of hyper parameters alpha and lambda.
  #'
  #' @param seed an integer that serves as the seed for caret::createDataPartition and caret::createMultiFolds.
  #' @param all_data a tibble containing the entire data set. Must contain a column named "result_factor", a factor variable with 
  #'  the outcome of the playoff series encoded as "W" or "L" for the higher seed.
  #' @param times an integer that gives the number of bootstrap models to fit and aggregate. Default = 20.
  #' @param p the proportion of data to be completely held out as the test data. Default = 0.8
  #' @param k the number of folds to use in k-fold cross validation for hyper parameter tuning. Default = 3.
  #' @param n_iters the number of Bayesian optimization iterations to use in hyper parameter tuning. Default = 25. 
  #' @param subsets A vector of integers that provides the top k features to use, as given by the variable importance scores.
  #' @param use_only_variables A character vector of column names to fit the kNN variables with. Default = NULL, which means that
  #'  all variables in all_data will be used (except for "result_factor").
  #'
  #' @return 
  #'  A tibble with columns subset and corresponding log loss scores for each subset on the test set.
  #'  
  #' @export
  #'
  
  set.seed(seed)
  all_folds <- caret::createDataPartition(y = all_data$result_factor, times = 1, p = p)
  main_train <- all_data[all_folds[[1]], ]
  
  set.seed(seed)
  inner_folds <- caret::createMultiFolds(y = main_train$result_factor, k = k, times = 1)
  
  inner_folds_temp <- inner_folds[str_detect(string = names(inner_folds), pattern = paste("Rep", 1, sep = ""))]
  all_processed_frames <- map(inner_folds_temp, process_folds, main_train = main_train)
  
  best_param <- BayesianOptimization(FUN =  function(alpha, lambda) {
        
    scores <- vector("numeric", length(all_processed_frames))
    var_imp <- vector("list", length(all_processed_frames))

      for (m in 1:length(all_processed_frames)) {
                                     
        model <- bagged_model(
          train = all_processed_frames[[m]]$train,
          test = all_processed_frames[[m]]$test,
          label_train = all_processed_frames[[m]]$train$result_factor,
          alpha = alpha,
          s_lambda = as.integer(lambda),
          times = times,
          calibrate = FALSE)
        
        scores[m] <- log_loss(scores = model$predictions, label = all_processed_frames[[m]]$test$result_factor)
        var_imp[[m]] <- model$variable_importance

        }
    
    var_imp <- var_imp %>%
      reduce(left_join, by = "variable") %>%
      process_var_imp(.)    
    
    list(Score = -mean(scores), variable_importance = var_imp)
    
    }
    , bounds = list(alpha = c(0, 1), lambda = c(15L, 90L)), parallel = FALSE,
                                   initPoints = 4, nIters = n_iters, convThresh = 1e+02, verbose = 1)
  
  processed_best_param <- best_param$ScoreDT[which(best_param$ScoreDT$Score == max(best_param$ScoreDT$Score)), ]
  
  final_var_imp <- processed_best_param %>%
    select(contains("variable_importance")) %>%
    rename(variable = variable_importance.variable, importance = variable_importance.importance)
  
  rm(inner_folds_temp, best_param)
  
  rfe_results <- map_df(
    subsets, 
    rfe_selection, 
    all_processed_frames = all_processed_frames,
    n_iters = 7,
    var_imp = final_var_imp) %>%
  bind_rows(., tibble(
    subset_size = nrow(final_var_imp),
    alpha = processed_best_param$alpha[1],
    lambda = as.integer(processed_best_param$lambda[1])))
  
  processed_data <- process_folds(fold_index = all_folds[[1]], main_train = all_data)
  
  final_test_set_score <- pmap(
    list(subset_n = rfe_results$subset_size, lambda_final = rfe_results$lambda, alpha_final = rfe_results$alpha),
    ~model_pipe_outer(..1, ..2, ..3, processed_data = processed_data, times = times, variable_importance = final_var_imp)) %>%
  map_dbl(., function(x) x$log_loss)
  
  tibble(subset = rfe_results$subset_size, score = final_test_set_score)  
}

# Run the model validation process. Warning: takes a long time.

results <- mclapply(
  X = seeds,
  FUN = give_results,
  allData = allData,
  times = 20, 
  p = 0.8,
  k = 3, 
  n_iters = 25,
  use_only_variables = c("h2h", "weighted_gps", "q2_record", "powerplay_oppurtunities", "penaltykill_percentage", "vegas_odds", "toi_percent_qot_mean"),
  subsets = c(95, 100, 105)) %>%
reduce(., left_join, by = "subset") 

# Report the results in a readable format.

final_log_loss <- results %>% 
  select(-subset) %>%
  transmute(overall = rowMeans(.)) %>%
  bind_cols(subset = results$subset, .)
