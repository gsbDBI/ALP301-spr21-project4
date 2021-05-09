# Title     : Get All Item Scores
# Objective : Get all the items score from a user
# Created by: juan
# Created on: 5/8/21

run_get_item_scores <- FALSE

if (!exists(run_source_ubcf)) run_source_ubcf <- TRUE
if (!exists(run_source_ibcf)) run_source_ibcf <- TRUE
if (!exists(run_source_svd)) run_source_svd <- TRUE

get_item_scores_generator<-function(utility_matrix, type, d=5) {
  if(type == 'ubcf') {
    if(run_source_ubcf) source("/Models/ubcf.R", local = knitr::knit_global())
    similarity_matrix_user <- ubcf_get_similarity_matrix(utility_matrix)
    return(
      function(userid, ratings_matrix){
        ubcf_get_item_scores(userid, ratings_matrix, similarity_matrix_user)
      }
    )
  } else if(type == 'ibcf') {
    if(run_source_svd) source("/Models/ibcf.R", local = knitr::knit_global())
    similarity_matrix_item <- ubcf_get_similarity_matrix(utility_matrix)
    return(
      function(userid, ratings_matrix){
        ubcf_get_item_scores(userid, ratings_matrix, similarity_matrix_item)
      }
    )
  } else if(type == 'svd') {
    if(run_source_svd) source("/Models/svd.R", local = knitr::knit_global())
    factors <- svd_get_decomposition(utility_matrix, d)
    return(
      function(userid, ratings_matrix){
        svd_get_item_scores(userid, factors$U, factors$Vprime)
      }
    )
  } else {
    return()
  }
  
}