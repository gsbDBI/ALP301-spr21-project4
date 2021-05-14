# Title     : Factorization Machine (FM) helper functions
# Objective : Logic of FM recommendation system
# Created by: EZ
# Created on: 5/12/21

run_source_random <- FALSE

random_get_random_matrix<-function(utility_matrix){
  n_entries <- dim(utility_matrix)[1]*dim(utility_matrix)[2]
  utility_matrix[,] <- runif(n_entries,0,1)
  utility_matrix
}

random_get_item_scores <- function(userid, ratings_matrix, similarity_matrix_random){
  # User Index Position
  item_scores <- similarity_matrix_random[userid,]
}

