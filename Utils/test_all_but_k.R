# Title     : Test All But k Users
# Objective : Use test all but k methodology to compute recall, precision and RMSE
# Created by: juan
# Created on: 5/8/21

if (!exists("run_get_item_scores")) run_get_item_scores <- FALSE
if (!exists("run_get_top_x_recommendations")) run_get_top_x_recommendations <- FALSE

test_all_but_k<-function(k, ratings_matrix, type, params=list(), num_rows=NaN){
  if (run_get_item_scores) source("/cloud/project/RecSys/get_item_scores.R", local = knitr::knit_global())
  if (run_get_top_x_recommendations) source("/cloud/project/RecSys/get_top_x_recommendations.R", local = knitr::knit_global())
  precision_vector<-vector()
  recall_vector<-vector()
  rmse_vector<-vector()
  #Change the line below to run it on the entire dataset
  if (is.finite(num_rows)) {
    num_rows<-min(num_rows,nrow(ratings_matrix))
  } else {
    num_rows<-nrow(ratings_matrix)
  }
  update_similarity = !(type %in% c("ibcf", "cbf"))
  if(!update_similarity) get_item_scores <- get_item_scores_generator(ratings_matrix, type, params)
  for (userid in 1:num_rows){
    user<-ratings_matrix[userid,]
    index_of_known<-which(user!=.1)
    if(length(index_of_known)>k){
      takeout<-sample(1:length(index_of_known),size=k)
      stories_removed<-index_of_known[takeout]
      save_old<-user[stories_removed]
      ratings_matrix[userid,stories_removed]<-0
      
      if(update_similarity) get_item_scores <- get_item_scores_generator(ratings_matrix, type, params)
      recommended<-get_top_x_recommendations(userid, k, ratings_matrix, get_item_scores)
      matched<-length(intersect(story_ids[stories_removed],recommended))
      precision_vector<-append(precision_vector,matched/k)
      recall_vector<-append(recall_vector,(matched/(length(index_of_known))))
      
      ratings_matrix[userid,stories_removed]<-save_old
      allscores<-get_item_scores(userid, ratings_matrix)
      rmse<-sqrt(mean((allscores-ratings_matrix[userid,])^2))
      rmse_vector<-append(rmse_vector,rmse)
    }
  }
  c("Precision"=mean(precision_vector),"Recall"=mean(recall_vector), "RMSE"=mean(rmse_vector))
}