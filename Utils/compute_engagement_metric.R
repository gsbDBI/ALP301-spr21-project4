if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
pacman::p_load(here)
pacman::p_load(lmtest)
pacman::p_load(glue)
pacman::p_load(broom)
pacman::p_load(ri2)
pacman::p_load(margins)
pacman::p_load(glmnet) 
pacman::p_load(kableExtra)
pacman::p_load(stargazer)
pacman::p_load(knitr)
pacman::p_load(doParallel)
pacman::p_load(corrplot)

rm(list = ls())

# Set working directory
setwd("/cloud/project/Tutorials")

# Load story info
story_info <- read_csv("Datasets/all_story_obs.csv")

# Utility matrix that includes NA
utility_mat <- read_csv("Datasets/utils_mat_filtered.csv",col_types = cols(.default = col_double()))
num_cols <- 2527
child_ids <- as.integer(utility_mat$child_id_code)
story_ids <- as.integer(colnames(utility_mat[,3:num_cols]))
stories_with_text <- (story_ids %in% story_info$story_id_code)
utility_matrix_na <- utility_mat[,3:num_cols]
utility_matrix_na <- utility_matrix_na[,stories_with_text]
story_ids <- colnames(utility_matrix_na)
utility_matrix_na <- as.matrix(utility_matrix_na)
rm(utility_mat)

# Utility matrix that replaces NA with zero
utility_mat <- read_csv("Datasets/utils_mat_filtered.csv",col_types = cols(.default = col_double()))
num_cols <- 2527
child_ids <- as.integer(utility_mat$child_id_code)
story_ids <- as.integer(colnames(utility_mat[,3:num_cols]))
utility_mat[is.na(utility_mat)] <- 0.0
stories_with_text <- (story_ids %in% story_info$story_id_code)
utility_matrix <- utility_mat[,3:num_cols]
utility_matrix <- utility_matrix[,stories_with_text]
story_ids <- colnames(utility_matrix)
utility_matrix <- as.matrix(utility_matrix)
rm(utility_mat)

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
    }
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
  average_changes_average_utility
  
}

# Example:
test <- compute_engagement_metric(utility_matrix, utility_matrix_na, 1000, 'ibcf')
test
# Should return a 99.71% increase in engagement for users 1:1000, using the IBCF RecSys