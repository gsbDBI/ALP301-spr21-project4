# Title     : Initialize Utility Matrix
# Objective : Generate the utility matrix and vectors with child_ids and story_ids.
# Created by: juan
# Created on: 5/8/21

initialize_utility_matrix<-function(utility_mat, story_info, num_cols=2527) {
  child_ids<-as.integer(utility_mat$child_id_code)
  story_ids<-as.integer(colnames(utility_mat[,3:num_cols]))
  utility_mat[is.na(utility_mat)]<-0.0
  
  stories_with_text<-(story_ids %in% story_info$story_id_code)
  utility_matrix<-utility_mat[,3:num_cols]
  utility_matrix<-utility_matrix[,stories_with_text]
  story_ids<-colnames(utility_matrix)
  utility_matrix<-as.matrix(utility_matrix)
  
  c("utility_matrix"=utility_matrix, "child_ids"=child_ids, "story_ids"=story_ids)
}

