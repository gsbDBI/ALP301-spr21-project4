# Title     : Content-Based Filtering (CBF) helper functions
# Objective : Logic of CBF recommendation system
# Created by: juan
# Created on: 5/8/21

run_source_cbf <- FALSE

cbf_get_similarity_matrix<-function(utility_matrix, params){
  story_info<-params$story_info
  story_ids<-params$story_ids
  story_info%>%filter(story_id_code %in% story_ids)%>%select(one_of("totpage","wordcount",
                                                                    "has_geoarea","has_color","has_fruits",
                                                                    "has_vegetables","has_animals","has_sports",
                                                                    "story_id_code"))->story_numeric
  story_numeric<-arrange(story_numeric,story_id_code)
  story_chars_matrix<-as.matrix(story_numeric[,-1]) #Remove the ID variable
  
  
  sim1<- (story_chars_matrix) %*% t(story_chars_matrix)
  sim2<- (rowSums(story_chars_matrix)) %*% t(rowSums(story_chars_matrix))
  similarity_matrix <- sim1 / sim2
  similarity_matrix
}

cbf_get_item_scores<-function(userid, ratings_matrix, similarity_matrix){
  uservector <- ratings_matrix[userid,]
  index_known<-which(uservector!=0) #Get the indices of stories known.
  index_unknown<-uservector==0
  if(length(index_known)>1){
    item_scores<-colSums(similarity_matrix[index_known,])/ncol(similarity_matrix[index_known,])
  }else{
    item_scores<-(similarity_matrix[index_known,])
  }
  
  item_scores
}
