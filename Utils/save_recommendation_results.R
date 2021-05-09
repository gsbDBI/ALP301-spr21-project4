Recommender_Results_Data_Frame<-function(recommender_fun, recommender_fun_args){
  ranked_items <- exec(recommender_fun, !!!recommender_fun_args)

  if (length(ranked_items) == 0) return(ranked_items)
  X <- recommender_fun_args$X
  user_index <- recommender_fun_args$userid
  rows <- tibble(user_index, child_id = child_ids[user_index], item_rank = 1:min(X,length(ranked_items)), story_id = ranked_items)
  return(rows)
}


Save_Recommender_Results<-function(recommender_fun, recommender_fun_args, filename, full=FALSE){
  
  if(full) {
    recommender_fun_args[["userid"]] <- 1
    recommender_results_item <- Recommender_Results_Data_Frame(recommender_fun, recommender_fun_args)
    next_user = 2
  } else {
    recommender_results_item <- tibble(read_csv(paste("Results/",filename, sep = ""),col_types = cols(.default = col_double())))
    next_user = recommender_results_item$user_index[nrow(recommender_results_item)] + 1
  }
  ratings_matrix <- recommender_fun_args$ratings_matrix
  if (next_user > nrow(ratings_matrix)) {
    print("No new rows")
    return()
  }
  cat("Next user: ", next_user)
  
  num_new_rows <- 0
  for (i in next_user:nrow(ratings_matrix)) {
    recommender_fun_args[["userid"]] <- i
    rows = Recommender_Results_Data_Frame(recommender_fun, recommender_fun_args)
    if (length(rows) > 1) {
      recommender_results_item = rbind(recommender_results_item, rows)
      num_new_rows <- num_new_rows + length(rows)
    }
  }
  
  if (num_new_rows > 0) {
    cat("Updating file with ", num_new_rows, "new rows")
    write.csv(recommender_results_item, paste("Results/",filename, sep = ""), row.names = FALSE)  
  } else {
    print("No new rows")
  }
  
}


