# Title     : Get Story Details
# Objective : Get story details
# Created by: juan
# Created on: 5/8/21

#From index in the matrix
get_story_details<-function(story_index){
  story_id<-story_ids[story_index]
  story_info%>%filter(story_id_code==story_id)->story_characteristics
  story_characteristics
}

#From 'story text file' id
get_story_details_2<-function(story_name){
  story_info%>%filter(story_id_code==story_name)->story_characteristics
  story_characteristics
}