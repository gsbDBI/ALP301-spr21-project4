# Title     : Factorization Machine (FM) initialization
# Objective : Function that generates the parameters for the FM model
# Created by: Juan
# Created on: 5/25/21

get_fm_parameters <- function(utility_mat, story_info, story_ids, child_ids){
  user_mapping <- utility_mat[,1:2]
  utils_raw_file <- "/cloud/project/Tutorials/Datasets/utils_raw_filter60.csv"
  user_info <- read_csv("/cloud/project/Tutorials/Datasets/user_interests.csv")
  # read child-story interaction data
  utility_data_raw<-read_csv(utils_raw_file, col_types = cols(.default = col_double()))
  story_attr <- c("totpage","wordcount","story_id_code","n_people")
  user_attr <- c("i_Life_skills","grade", "user_id")
  seed_value <- 123
  # Note - below is the list of all parameters we will need to input
  params_fm<-list(raw_matrix=utility_data_raw, story_info=story_info,
                  user_info=user_info, story_attr=story_attr, user_attr=user_attr,
                  model_type="binomial", user_mapping = user_mapping
                  , story_ids = story_ids, child_ids = child_ids,
                  seed_value = seed_value)
  
  params_fm
}