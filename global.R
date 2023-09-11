library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(thematic)

thematic_shiny(cerulean)

# read in clean data
all_schools <- read_csv("clean_data/all_schools.csv")
sec_school_rank <- read_csv("clean_data/sec_schools_rank.csv")

# list of local authorites
la_list <- all_schools %>% 
  arrange(la_name) %>% 
  distinct(la_name) %>% 
  pull()

