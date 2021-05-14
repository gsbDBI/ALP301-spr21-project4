plot_diversity_recsys<-function(filename) {
  recommender_results <- read.csv(file = paste("/cloud/project/Results/",filename, sep = ""))
  recommender_results_item_one <- recommender_results %>% filter(item_rank <= 1) %>% select(story_id) %>% unique()
  recommender_results_item_five <- recommender_results %>% filter(item_rank <= 5) %>% select(story_id) %>% unique()
  
  index_results_one <- which(recommender_results_item_one$story_id %in% story_ids)
  index_results_five <- which(recommender_results_item_five$story_id %in% story_ids)
  
  popularity_ones <- colSums(utility_matrix[, index_results_one]>0.3)
  popularity_fives <- colSums(utility_matrix[, index_results_five]>0.3)
  
  readers_per_story <- (colSums(utility_matrix>0.3))
  
  # Create a data frame of recommendations
  df1 <- data.frame(x = readers_per_story, label = rep("Overall", length(readers_per_story)))
  df2 <- data.frame(x = popularity_fives, label = rep("Five_Recs", length(popularity_fives)))
  df3 <- data.frame(x = popularity_ones, label = rep("One_Rec", length(popularity_ones)))
  df <- rbind(df1, df2, df3)
  
  # Plot the number of readers per story
  ggplot(df, aes(x, y = ..count.., fill = label)) +
    geom_density(color = "black", alpha = 0.5) +
    xlab("Number of readers") +
    ylab("Density") + 
    xlim(0, 1000)

}