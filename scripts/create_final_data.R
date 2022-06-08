# FILE TO CREATE AND COMBINE ALL DATASETS

library(tidyverse)

source("scripts/case_example_data_transformation.R")
source("scripts/evaluation_case_examples.R")
source("scripts/participant_data_transformation.R")

# join data sets
data_combined <- left_join(data_eval, participant_info, by = "p_id")


save(data_combined, file = "data/data_combined.rda")


