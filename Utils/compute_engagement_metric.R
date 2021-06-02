# Import RecSys models
source("../RecSys/Models/cbf.R", local = knitr::knit_global())
source("../RecSys/Models/ibcf.R", local = knitr::knit_global())
source("../RecSys/Models/svd.R", local = knitr::knit_global())
source("../RecSys/Models/ubcf.R", local = knitr::knit_global())

# Function that computes the average % change in engagement for n users
# Inputs:
#  - utility_matrix (where NAs are replaced with zero)
#  - utility_matrix_na (where NAs are kept as-is)
#  - n_users (select how many users you want to run the function on)
#  - type (type of RecSys, only IBCF implemented so far)

compute_engagement_metric <- function(utility_matrix, utility_matrix_na, n_users, type) {
  
  # TODO: Add more RecSys types
  if(type == 'ibcf') {
    similarity_matrix <- ibcf_get_similarity_matrix(utility_matrix)
  } else if(type == 'ubcf'){
    similarity_matrix <- ubcf_get_similarity_matrix(utility_matrix)
  } else if(type == 'cbf'){
    params <- list(story_info=story_info, story_ids=story_ids)
    similarity_matrix <- cbf_get_similarity_matrix(utility_matrix, params)
  } else if(type == 'svd'){
    params <- list(d=20)
    factors <- svd_get_decomposition(utility_matrix, params)
  } else {
    return()
  }
  
  users <- c(1:n_users)
  n_users <- length(users)
  
  # Create empty vectors
  changes_sum_utility <- c()
  changes_average_utility <- c()
  actual_utilities_interacted <- c()
  sum_predicted_interacted_all <- c()
  sum_predicted_recommended_all <- c()
  average_predicted_interacted_all <- c()
  average_predicted_recommended_all <- c()
  
  for (user in users) {
    
    # Skip buggy user
    if (user == 694) {
      next
    }
    
    # Extract user from the utility matrix
    user_interacted <- names(utility_matrix_na[user, utility_matrix_na[user,] > -1])
    
    # Remove NAs
    user_interacted <- na.omit(user_interacted)
    
    # Count the number of interacted stories
    n_interacted <- length(user_interacted)
    
    # Get predicted utilities for user
    # TODO: Add more RecSys types
    if (type == 'ibcf') {
      predicted_utilities <- ibcf_get_item_scores(user, utility_matrix, similarity_matrix)
    } else if (type == 'ubcf') {
      predicted_utilities <- ubcf_get_item_scores(user, utility_matrix, similarity_matrix)[1,]
    } else if (type == 'cbf') {
      predicted_utilities <- cbf_get_item_scores(user, utility_matrix, similarity_matrix)
      if (length(predicted_utilities) <= 0) {
        next
      }
      names(predicted_utilities) <- story_ids
    } else if (type == 'svd') {
      predicted_utilities <- svd_get_item_scores(user, factors$U, factors$Vprime)[1,]
      if (length(predicted_utilities) <= 0) {
        next
      }
      names(predicted_utilities) <- story_ids
    }
    
    # To avoid buggy users that sometimes don't get any predicted utilities 
    # So far only user 694, if so this line is redundant b/c already taken care of above
    if (length(predicted_utilities) <= 0) {
      next
    }
    
    # Sum the ACTUAL total utility for user
    utility_sum <- rowSums(utility_matrix, na.rm=TRUE)[user]
    
    # Sense-check: average ACTUAL utility per interacted story
    average_actual_utility_interacted <- utility_sum / n_interacted
    
    # Get predicted utilities only for interacted stories
    predicted_utilities_interacted <- predicted_utilities[c(user_interacted)]
    
    # Sum and average the predicted utilities for interacted stories
    sum_predicted_interacted <- sum(predicted_utilities_interacted)
    average_predicted_interacted <- sum_predicted_interacted / n_interacted
    
    # Sort the predicted utilities
    predicted_utilities_sorted <- sort(predicted_utilities, decreasing = TRUE)
    
    # Sum and average the predicted utilities for recommended stories
    sum_predicted_recommended <- sum(predicted_utilities_sorted[1:n_interacted])
    average_predicted_recommended <- sum_predicted_recommended / n_interacted
    
    # Compute change in engagement
    change_sum_utility <- (sum_predicted_recommended / sum_predicted_interacted - 1) * 100
    change_average_utility <- (average_predicted_recommended / average_predicted_interacted - 1) * 100
    
    # Add to vectors
    changes_sum_utility <- c(changes_sum_utility, change_sum_utility)
    changes_average_utility <- c(changes_average_utility, change_average_utility)
    actual_utilities_interacted <- c(actual_utilities_interacted, average_actual_utility_interacted)
    sum_predicted_interacted_all <- c(sum_predicted_interacted_all, sum_predicted_interacted)
    sum_predicted_recommended_all <- c(sum_predicted_recommended_all, sum_predicted_recommended)
    average_predicted_interacted_all <- c(average_predicted_interacted_all, average_predicted_interacted)
    average_predicted_recommended_all <- c(average_predicted_recommended_all, average_predicted_recommended)
    
  }
  
  # Compute average across users
  average_changes_sum_utility <- mean(changes_sum_utility)
  average_changes_average_utility <- mean(changes_average_utility)
  average_actual_utilities_interacted <- mean(actual_utilities_interacted)
  
  # Return result
  # Average % change in engagement across the n users
  results <- list("change"=average_changes_average_utility, 
                  "interacted"=average_predicted_interacted_all,
                  "recommended"=average_predicted_recommended_all)
  results
  
}