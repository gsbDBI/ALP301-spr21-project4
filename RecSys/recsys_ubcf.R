get_similarity_matrix_ubcf<-function(utility_matrix){
  # utility_matrix = Matrix(utility_matrix, sparse=TRUE)
  sim1<- (utility_matrix) %*% t(utility_matrix)
  sim2<- (rowSums(utility_matrix)) %*% t(rowSums(utility_matrix))
  similarity_matrix_user <- sim1 / sim2
  similarity_matrix_user[is.nan(similarity_matrix_user)]<-0
  
  similarity_matrix_user
}

get_params_ubcf<-function(ratings_matrix){
  
}

get_top_x_recommendations_ubcf<-function(userid,X,ratings_matrix,similarity_matrix_user){
  # We need to remove items that they already know to from our recommendations.
  user_row<-ratings_matrix[userid,]
  known_stories<-user_row!=0 
  unknown_stories<-user_row==0
  other_users_ratings<-ratings_matrix[-userid,]
  similarity_vector<-as.vector(similarity_matrix_user[userid,-userid])
  item_scores<- (similarity_vector %*% other_users_ratings)/sum(similarity_vector)
  names_unknown<-story_ids[unknown_stories]
  index <- which(item_scores[unknown_stories] >= sort(item_scores[unknown_stories], decreasing=T)[X], arr.ind=TRUE)
  
  names_unknown[index]
}

get_all_item_scores_ubcf<-function(userid,ratings_matrix,similarity_matrix_user){
  user_row<-ratings_matrix[userid,]
  known_stories<-user_row!=0  
  unknown_stories<-user_row==0
  other_users_ratings<-ratings_matrix[-userid,]
  similarity_vector<-as.vector(similarity_matrix_user[userid,-userid])
  item_scores<- (similarity_vector %*% other_users_ratings)/sum(similarity_vector)
  
  item_scores
}