# Title     : Get Top X Recommendations
# Objective : Get top x recommendations for a particular user
# Created by: juan
# Created on: 5/8/21

get_top_x_recommendations<-function(userid, X, ratings_matrix, get_item_scores) {
  item_scores <- get_item_scores(userid, ratings_matrix)
  
  # We need to remove items that they already know to from our recommendations.
  user_row<-ratings_matrix[userid,]
  unknown_stories<-user_row==0
  names_unknown<-story_ids[unknown_stories]
  index <- which(item_scores[unknown_stories] >= sort(item_scores[unknown_stories], decreasing=T)[X], arr.ind=TRUE)
  
  names_unknown[index]
}