# Title     : Get All Item Scores
# Objective : Get all the items score from a user
# Created by: juan & johanna
# Created on: 5/8/21, modified 5/25/21

run_get_item_scores <- FALSE

if (!exists("run_source_ubcf")) run_source_ubcf <- TRUE
if (!exists("run_source_ibcf")) run_source_ibcf <- TRUE
if (!exists("run_source_svd")) run_source_svd <- TRUE
if (!exists("run_source_cbf")) run_source_cbf <- TRUE
if (!exists("run_source_random")) run_source_random <- TRUE

path = "/cloud/project/RecSys"
get_item_scores_generator<-function(utility_matrix, type, params=list()) {
  if(type == 'ubcf') {
    if(run_source_ubcf) source(paste(path, "/Models/ubcf.R", sep=""), local = knitr::knit_global())
    similarity_matrix_user <- ubcf_get_similarity_matrix(utility_matrix)
    return(
      function(userid, ratings_matrix){
        ubcf_get_item_scores(userid, ratings_matrix, similarity_matrix_user)
      }
    )
  } else if(type == 'ibcf') {
    if(run_source_ibcf) source(paste(path, "/Models/ibcf.R", sep=""), local = knitr::knit_global())
    similarity_matrix_item <- ibcf_get_similarity_matrix(utility_matrix)
    return(
      function(userid, ratings_matrix){
        ibcf_get_item_scores(userid, ratings_matrix, similarity_matrix_item)
      }
    )
  } else if(type == 'svd') {
    if(run_source_svd) source(paste(path, "/Models/svd.R", sep=""), local = knitr::knit_global())
    factors <- svd_get_decomposition(utility_matrix, params)
    return(
      function(userid, ratings_matrix){
        svd_get_item_scores(userid, factors$U, factors$Vprime)
      }
    )
  } else if(type == 'cbf') {
    if(run_source_cbf) source(paste(path, "/Models/cbf.R", sep=""), local = knitr::knit_global())
    similarity_matrix_story <- cbf_get_similarity_matrix(utility_matrix, params)
    return(
      function(userid, ratings_matrix){
        cbf_get_item_scores(userid, ratings_matrix, similarity_matrix_story)
      }
    )
  } else if(type == 'random') {
    if(run_source_random) source(paste(path, "/Models/random.R", sep=""), local = knitr::knit_global())
    similarity_matrix_random <- random_get_random_matrix(utility_matrix)
    return(
      function(userid, ratings_matrix){
        random_get_item_scores(userid, ratings_matrix, similarity_matrix_random)
      }
    )
  } else if(type == 'ensemble') {
    if(run_source_cbf) source(paste(path, "/Models/cbf.R", sep=""), local = knitr::knit_global())
    similarity_matrix_story <- cbf_get_similarity_matrix(utility_matrix, params)
    
    if(run_source_ubcf) source(paste(path, "/Models/ubcf.R", sep=""), local = knitr::knit_global())
    similarity_matrix_user <- ubcf_get_similarity_matrix(utility_matrix)
    
    if(run_source_ibcf) source(paste(path, "/Models/ibcf.R", sep=""), local = knitr::knit_global())
    similarity_matrix_item <- ibcf_get_similarity_matrix(utility_matrix)
    
    if(run_source_svd) source(paste(path, "/Models/svd.R", sep=""), local = knitr::knit_global())
    factors <- svd_get_decomposition(utility_matrix, params_svd)
    
    return(
      function(userid, ratings_matrix) {
        x1 = cbf_get_item_scores(userid, ratings_matrix, similarity_matrix_story)
        x2 = ubcf_get_item_scores(userid, ratings_matrix, similarity_matrix_user)
        x3 = ibcf_get_item_scores(userid, ratings_matrix, similarity_matrix_item)
        x4 = svd_get_item_scores(userid, factors$U, factors$Vprime)
        
        w <- params$w
        
        w$x1 * x1 + w$x2 * x2 + w$x3 * x3 + w$x4 * x4
      }
    )
  }
  }else {
    return()
  }