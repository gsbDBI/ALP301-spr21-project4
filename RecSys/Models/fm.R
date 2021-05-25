# Title     : Factorization Machine (FM) helper functions
# Objective : Logic of FM recommendation system
# Created by: EZ
# Created on: 5/12/21

run_source_fm <- FALSE

fm_get_factorization_matrix<-function(params){
  
  # Set up training data
  utility_data_raw <- params$raw_matrix%>% filter(!is.na(intensity))
  utility_data_raw <- utility_data_raw %>% rename(user_id = child_id_code)
  
  
  # Set up test data
  test_data <- expand.grid(user_id = params$child_ids, story_id_code = as.integer(params$story_ids))
  
  # Add item attributes to training data (if of interest)
  if(!is.null(params$story_info)){
    story_info_model <- story_info %>% select(one_of(params$story_attr))
    merged_data_train <- left_join(utility_data_raw, story_info_model, by = c("story_id_code"="story_id_code")) %>% drop_na 
    merged_data_test <- left_join(test_data, story_info_model, by = c("story_id_code"="story_id_code")) %>% drop_na 
    utility_data_train <- merged_data_train
    utility_data_test <- merged_data_test
  }
  
  # Add user attributes to training data (if of interest)
  if(!is.null(params$user_info)){
    user_info <- params$user_info
    user_info$grade<-as.integer(str_extract(user_info$grade,"[1-9]"))
    user_info <- user_info %>% select(one_of(params$user_attr))
    merged_data_train <-left_join(utility_data_train, user_info, by = c("user_id"="user_id")) %>% drop_na
    merged_data_test <- left_join(utility_data_test, user_info, by = c("user_id"="user_id")) %>% drop_na
  }
  
  #Now to train model and generate scores
  factors <- append(params$story_attr, params$user_attr)
  formula_fm <- as.formula(paste("intensity~", paste(factors, collapse="+")))
  # Set up training data matrix form
  train_matrix <- model.matrix(formula_fm, data = merged_data_train)
  train_matrix_sparse = as(train_matrix, "RsparseMatrix")
  if(params$model_type == "binomial"){
    train_outcome <-ifelse(merged_data_train$intensity>0.4,1,0)
  } else{
    train_outcome <- merged_data_train$intensity
  }
  # Set up test data matrix form
  merged_data_test$intensity <- 0.0
  test_matrix <- model.matrix(formula_fm, data = merged_data_test)
  
  # run model
  set.seed(params$seed_value)
  fm = FactorizationMachine$new(learning_rate_w = 0.06, rank = 15, lambda_w = 0.001,
                                lambda_v = 0.001, family = params$model_type, intercept = FALSE)
  res = fm$fit(train_matrix_sparse, train_outcome, n_iter = 50)
  # New column with predictions for the test function
  merged_data_test$preds = fm$predict(test_matrix)
  # Normalize values to between 0 and 1
  merged_data_test$preds <- (merged_data_test$preds - min(merged_data_test$preds)) / 
    (max(merged_data_test$preds) - min(merged_data_test$preds))
  merged_data_test
}

fm_get_item_scores <- function(userid, ratings_matrix, fm_model_prediction, params){
  # User Index Position
  user_index <- params$user_mapping$child_id_code[params$user_mapping$X1 == userid]
  
  prediction_values <- fm_model_prediction$preds[fm_model_prediction$user_id==user_index]
  story_positions <- fm_model_prediction$story_id_code[fm_model_prediction$user_id==user_index]
  
  item_scores <- structure(prediction_values, names = as.character(story_positions))
  filtered_item_scores <- item_scores[colnames(ratings_matrix)]
  final_item_scores <- filtered_item_scores[!is.na(filtered_item_scores) == TRUE]
  if(params$model_type == "binomial"){
    final_item_scores[final_item_scores == 1] <- runif(length(final_item_scores[final_item_scores == 1]),0.4,1)
    final_item_scores
  } 
    else{
      final_item_scores
  }
}

