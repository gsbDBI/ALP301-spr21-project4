# Title     : Get DF Diversity
# Objective : Finds the diversity of recommendations saved in a .csv file.
# Created by: Eric
# Created on: 5/26/21

get_df_diversity<-function(filename, ratings_matrix, unknown_value = 0.0){
  recommender_results <- read.csv(file = paste("/cloud/project/Results/",filename, sep = ""))
  recommender_results_item_one <- recommender_results %>% filter(item_rank <= 1) %>% select(story_id) %>% unique()
  recommender_results_item_five <- recommender_results %>% filter(item_rank <= 5) %>% select(story_id) %>% unique()
  
  index_results_one <- which(recommender_results_item_one$story_id %in% story_ids)
  index_results_five <- which(recommender_results_item_five$story_id %in% story_ids)
  
  popularity_ones <- colSums(ratings_matrix[, index_results_one] != unknown_value)
  popularity_fives <- colSums(ratings_matrix[, index_results_five] != unknown_value)
  
  readers_per_story <- (colSums(ratings_matrix != unknown_value))
  
  # Create a data frame of recommendations
  df1 <- data.frame(x = readers_per_story, label = rep("Overall", length(readers_per_story)))
  df2 <- data.frame(x = popularity_fives, label = rep("Five_Recs", length(popularity_fives)))
  df3 <- data.frame(x = popularity_ones, label = rep("One_Rec", length(popularity_ones)))
  df <- rbind(df1, df2, df3)
  df
}
  