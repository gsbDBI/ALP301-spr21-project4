# Title     : Item-Based Collaborative Filtering (IBCF) helper functions
# Objective : Logic of IBCF recommendation system
# Created by: juan
# Created on: 5/8/21

run_source_ibcf <- FALSE

ibcf_get_similarity_matrix<-function(utility_matrix){
  # utility_matrix = Matrix(utility_matrix, sparse=TRUE)
  sim1<- t(utility_matrix) %*% (utility_matrix)
  sim2<- (colSums(utility_matrix)) %*% t(colSums(utility_matrix))
  similarity_matrix <- sim1 / sim2
  
  similarity_matrix
}

ibcf_get_item_scores<-function(userid, ratings_matrix, similarity_matrix){
  uservector <- ratings_matrix[userid,]
  index_known<-which(uservector!=0) #Get the indices of stories known.
  if(length(index_known)>1){
    item_scores<-colSums(similarity_matrix[index_known,])
  }else{
    item_scores<-(similarity_matrix[index_known,])
  }
  
  item_scores
}