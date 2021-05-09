num_cols <- 2527

child_ids<-as.integer(utility_mat$child_id_code)
story_ids<-as.integer(colnames(utility_mat[,3:num_cols]))
utility_mat[is.na(utility_mat)]<-0.0

stories_with_text<-(story_ids %in% story_info$story_id_code)
utility_matrix<-utility_mat[,3:num_cols]
utility_matrix<-utility_matrix[,stories_with_text]
story_ids<-colnames(utility_matrix)
utility_matrix<-as.matrix(utility_matrix)

