import pandas as pd

level2indx = dict()
indx2level = dict()
c = 0
for level1 in range(1,10):
  for level2 in ['Easy', 'Hard']:
    key = "Level %i:%s" % (level1, level2)
    level2indx[key] = c
    indx2level[c] = key
    c += 1

story = pd.read_csv("../Tutorials/Datasets/story.csv").set_index('story_id')

# column processing
story['reading_time_avg'] = story['reading_time'].str.extract(r'(\d{1,})-(\d{1,})').astype(float).mean(axis=1)

def get_grade_level_sum(x):
  x = x[2:-2].split("', '")
  return sum([level2indx[key] for key in x])/len(x)

story['grade_level_sum'] = story['grade_level'].apply(get_grade_level_sum)

# view count is by level and difficulty (increasing)
view_count = story.columns[:14].append(story.columns[51:55])

# completed count is by level and difficulty (increasing)
completed_count = story.columns[14:28].append(story.columns[55:])

# left out column 'collection' for now 
other_cols = pd.Index(['has_mcq', 'reading_time_avg', 'grade_level_sum',
              'story_type', 'freadom_point', 'reading_count', 'view_count',
              'like_count', 'is_published', 'mcq', 'landscape',
              'experience_count'])

column_mapping= pd.DataFrame(story.columns, columns=['columns'])
# print(column_mapping)
# print(story.iloc[0])

columns_out = other_cols.append(view_count.append(completed_count))
# print(columns_out)
story_out = story[columns_out].copy()

story_mapping = pd.read_csv("../Tutorials/Datasets/story_mapping.csv").set_index('story_id')

indx = story_out.index
story_out.loc[indx, 'story_id_code'] = story_mapping['story_id_code'].reindex(indx).values

story_out.to_csv("../Tutorials/Results/story_info.csv", index=False)
