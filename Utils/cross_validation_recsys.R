# Title     : Cross-validation RecSys
# Objective : Obtain precision, recall and RMSE for a particular type of model
# though corss validations
# Created by: juan
# Created on: 5/8/21

cross_validation_recsys <- function(utility_matrix, folds, X, type, params, key="", seed=301, unknown_value = 0.0) {
  set.seed(seed)
  splitfolds <- sample(1:folds, nrow(utility_matrix), replace = TRUE)
  results_cv <- matrix(NA, nrow = folds, ncol = 5)
  # Iterate through each of the k folds
  for (k in 1:folds){
    cat("\tFold: ", k,"\n")
    colnames(results_cv) <- c("k","precision at 5","recall at 5","rmse","n")
    
    # Create training utility matrix by removing X random story interactions 
    # form the users in the test fold
    train_set <- utility_matrix
    valid_users <- which(splitfolds == k)
    stories_removed <- list()
    for (userid in valid_users){
      user <- utility_matrix[userid,]
      index_of_known <- which(user!= unknown_value)
      if(length(index_of_known)>X){
        takeout <- sample(1:length(index_of_known),size=min(X, length(index_of_known)))
        stories_removed[[userid]] <- index_of_known[takeout]
        train_set[userid, stories_removed[[userid]]] <- unknown_value
      }
    }
    
    # Train the recommendation system with the adjusted utility matrix
    recommender <- get_item_scores_generator(train_set, type, params)
    precision_vector<-rep(NA,length(valid_users))
    recall_vector<-rep(NA,length(valid_users))
    rmse_vector<-rep(NA,length(valid_users))
    
    # Compute the precision, recall and rmse for the users in the test fold
    for (userid in valid_users){
      user <- utility_matrix[userid,]
      index_of_known <- which(user!= unknown_value)
      if (length(index_of_known)>X) {
        recommended <- get_top_x_recommendations(userid, X, train_set, recommender, unknown_value)
        matched=length(intersect(story_ids[stories_removed[[userid]]],recommended))
        precision_vector[userid]<-(matched/X)
        recall_vector[userid]<-(matched/(length(index_of_known)))
        scores<-recommender(userid, train_set)
        scores[is.na(scores)]<-unknown_value
        rmse<-sqrt(mean((scores-utility_matrix[userid,])^2))
        rmse_vector[userid]<-rmse
      }
    }
    mean_precision <- mean(precision_vector, na.rm = TRUE)
    mean_recall <- mean(recall_vector, na.rm = TRUE)
    mean_rmse <- mean(rmse_vector, na.rm = TRUE)
    n <- sum(is.finite(rmse_vector))
    
    results_cv[k,]<-c(k, mean_precision, mean_recall, mean_rmse, n)
  }
  
  filename = paste("top_",X,"_",folds,"_fold_cv_",type,"_",key,".csv", sep = "")
  write.csv(data.frame(results_cv), paste("/cloud/project/Results/",filename, sep = ""), row.names = FALSE)
  
  data.frame(results_cv)
}