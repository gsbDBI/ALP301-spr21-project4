# Title     : User-Based Collaborative Filtering (UBCF) helper functons
# Objective : Logic of UBCF recommendation system
# Created by: juan
# Created on: 5/8/21

run_source_ubcf <- FALSE

ubcf_get_similarity_matrix<-function(utility_matrix){
  # utility_matrix = Matrix(utility_matrix, sparse=TRUE)
  sim1 <- (utility_matrix) %*% t(utility_matrix)
  sim2 <- (rowSums(utility_matrix)) %*% t(rowSums(utility_matrix))
  similarity_matrix_user <- sim1 / sim2
  similarity_matrix_user[is.nan(similarity_matrix_user)]<-0
  
  similarity_matrix_user
}

ubcf_get_item_scores<-function(userid, ratings_matrix, similarity_matrix_user){
  other_users_ratings<-ratings_matrix[-userid,]
  similarity_vector<-as.vector(similarity_matrix_user[userid,-userid])
  item_scores<- (similarity_vector %*% other_users_ratings)/sum(similarity_vector)
  
  item_scores
}