source('src/modelling/pca/pca_functions.R')

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
  bind_cols(read_csv("data/processed/2008-2019_naturalstattrick_time-related.csv")) %>%
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
all_seeds <- sample(1:1000000000, 1, replace = FALSE)

give_results <- function(seed, all_data, times = 20, p = 0.8, k = 3, num_of_models = 5, n_iters = 25, use_only_variables = NULL) {
  
  writeLines(paste("Seed:", seed))
  
  set.seed(seed)
  all_folds <- caret::createDataPartition(y = all_data$result_factor, times = 1, p = p)
  main_train <- all_data[all_folds[[1]], ]
  
  set.seed(seed)
  inner_folds <- caret::createMultiFolds(y = main_train$result_factor, k = k, times = num_of_models)
  
  final_parameters <- vector("list", length(inner_folds) / k)
  
  for (i in 1:(length(innerFolds) / k)) {
  
  writeLines(paste("Fitting Five Models For Seed:", seed, "in Rep:", i))
    
  inner_folds_temp <- inner_folds[str_detect(string = names(inner_folds), pattern = paste("Rep", i, sep = ""))]
  all_processed_frames <- map(inner_folds_temp, process_folds, main_train = main_train, use_only_variables = use_only_variables)
    
  writeLines(paste("Finished Processing Data For Seed:", seed, "in Rep:", i))
  
  best_param <- BayesianOptimization(FUN =  function(alpha, lambda, ncomp) {
        
    scores <- vector("numeric", length(all_processed_frames))

    for (m in 1:length(all_processed_frames)) {
        
        pca_frames <- pca_recipe(ncomp = ncomp, train = all_processed_frames[[m]]$train, test = all_processed_frames[[m]]$test)
        
        model <- bagged_model(
          train = pca_frames$train, 
          test = pca_frames$test,
          label_train = pca_frames$train$result_factor,
          alpha = alpha,
          s_lambda = as.integer(lambda), 
          times = 20,
          calibrate = FALSE)
        
        scores[m] = log_loss(scores = model$predictions, label = pca_frames$test$result_factor)

        rm(model)
        
      }
        
    list(Score = -mean(scores))
    
    }
    , bounds = list(alpha = c(0, 1), lambda = c(10L, 130L), ncomp = c(2L, 100L)), parallel = FALSE,
                                   initPoints = 4, nIters = n_iters, convThresh = 100, verbose = 1)
  
  writeLines(paste("Store Final Parameters For Seed:", seed, "in Rep:", i))
  
  final_parameters[[i]] <- tibble(alpha = best_param$ScoreDT$alpha[which.max(best_param$ScoreDT$Score)],
                                  lambda = as.integer(best_param$ScoreDT$lambda[which.max(best_param$ScoreDT$Score)]),
                                  ncomp = as.integer(best_param$ScoreDT$ncomp[which.max(best_param$ScoreDT$Score)]))

  rm(inner_folds_temp, all_processed_frames, best_param)
  gc()
  
  }
  
  rm(i, main_train)
  gc()
  
  writeLines(paste("Bind Rows for Seed:", seed))
  final_parameters <- bind_rows(final_parameters)
  
  writeLines(paste("Score the Test Set for Seed:", seed))
  processed_data <- process_folds(fold_index = all_folds[[1]], main_train = all_data)
  
  final_test_set_score <- train_ensemble(
    folds = all_folds, 
    final_parameters = final_parameters,
    processed_data = processed_data, 
    times = times,
    label_test = all_data$result_factor[-all_folds[[1]]])
  
  writeLines(paste("Log Loss Test Set:", final_test_set_score$log_loss, sep = " "))
  
  list(log_loss = final_test_set_score$log_loss, var_imp = final_test_set_score$var_imp)
}

results <- mclapply(
  X = all_seeds, 
  FUN = give_results,
  all_data = all_data, 
  use_only_variables = c("h2h", "weighted_gps", "q2_record", "powerplay_oppurtunities", "penaltykill_percentage", "vegas_odds", "toi_percent_qot_mean"),
  p = 0.8, 
  k = 3, 
  times = 20,
  num_of_models = 5, 
  mc.cores = 6,
  mc.preschedule = FALSE) %>%
map_dbl(., function(x) x$log_loss)

#results <- lapply(X = allSeeds, FUN = giveResults, allData = allData)

final_varimp <- process_var_imp(var_imp_raw = map(results, function(x) x$var_imp) %>% reduce(left_join, by = "variable"))

#...................................Paste the Results.........................................................#

paste("Final LogLoss from Repeats: ", round(mean(final_log_loss), 5))