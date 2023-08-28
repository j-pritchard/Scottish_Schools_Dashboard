library(tidyverse)

# Reading data and some tidying
schools_data <- readxl::read_xlsx("raw_data/SG_SchoolRoll_2022.xlsx") %>% 
  janitor::clean_names() %>% 
  select(school_type, school_name, la_name, website_address, denomination,
         pupil_roll, fte_teachers, dzsimd_percentile, latitude, longitude) %>% 
  # Add teacher/pupil ratio
  mutate(pupil_teacher_ratio = pupil_roll/fte_teachers)

ranking_data <- readxl::read_xlsx("raw_data/DE_Sec_School_Ranking.xlsx") %>% 
  janitor::clean_names()


# Joining ranking_data to schools_data
# Create key as ranking data lacks suitable key
schools_data_key <- schools_data %>% 
  filter(school_type == "Secondary") %>% 
  mutate(school_la = str_c(school_name, " ", la_name))
  
ranking_data_key <- ranking_data %>% 
  mutate(school_la = str_c(school_name, " ", la_name)) %>% 
  select(rank, five_highers, school_la)

# Inner join
sec_school_rank <- 
  inner_join(schools_data_key, ranking_data_key, by = "school_la") %>% 
  select(-school_la)

# Write clean data
write.csv(schools_data, "clean_data/all_schools.csv")
write.csv(sec_school_rank, "clean_data/sec_schools_rank.csv")


# Tidy up environment
remove(ranking_data, ranking_data_key,
       schools_data, schools_data_key,
       sec_school_rank)