# Title     : Factorization Machine (FM) helper functions
# Objective : Logic of FM recommendation system
# Created by: EZ
# Created on: 5/12/21

run_source_fm <- FALSE

fm_get_factorization_matrix<-function(params){
  
  utility_data_raw <- params$raw_matrix%>% filter(!is.na(intensity))
  utility_data_raw <- utility_data_raw %>% rename(user_id = child_id_code)
  
  if(!is.null(params$story_info)){
    story_info <- story_info %>% select(one_of(params$story_attr))
    merged_data<-left_join(utility_data_raw, story_info, by = c("story_id_code"="story_id_code")) %>% drop_na 
    utility_data_raw <- merged_data
  }
  
  if(!is.null(params$user_info)){
    user_info <- params$user_info
    user_info$grade<-as.integer(str_extract(user_info$grade,"[1-9]"))
    user_info <- user_info %>% select(one_of(params$user_attr))
    merged_data<-left_join(utility_data_raw, user_info, by = c("user_id"="user_id")) %>% drop_na
  }
  
  #Now to train model and generate scores
  factors <- append(params$story_attr, params$user_attr)
  formula_fm <- as.formula(paste("intensity~", paste(factors, collapse="+")))
  train_matrix <- model.matrix(formula_fm, data = merged_data)
  train_matrix_sparse = as(train_matrix, "RsparseMatrix")
  if(params$model_type == "binomial"){
    train_outcome <-ifelse(merged_data$intensity>0.4,1,0)
  } else{
    train_outcome <- merged_data$intensity
  }
  # run model
  set.seed(params$seed_value)
  fm = FactorizationMachine$new(learning_rate_w = 0.06, rank = 15, lambda_w = 0.001,
                                lambda_v = 0.001, family = params$model_type, intercept = FALSE)
  res = fm$fit(train_matrix_sparse, train_outcome, n_iter = 50)
  merged_data$preds = fm$predict(train_matrix)
  
  merged_data
}

fm_get_item_scores <- function(userid, ratings_matrix, fm_model_prediction, params){
  # User Index Position
  user_index <- params$user_mapping$child_id_code[params$user_mapping$X1 == userid]
  
  prediction_values <- fm_model_prediction$preds[fm_model_prediction$user_id==user_index]
  story_positions <- fm_model_prediction$story_id_code[fm_model_prediction$user_id==user_index]
  
  item_scores <- structure(prediction_values, names = as.character(story_positions))
  filtered_item_scores <- item_scores[colnames(ratings_matrix)]
  final_item_scores <- filtered_item_scores[!is.na(filtered_item_scores) == TRUE]
  final_item_scores
}

