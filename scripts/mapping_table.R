library(tidyverse)


# read gorilla spreadsheet
raw_participant_map <- read.csv2("data/data-questionnaire-d1do.csv", sep = ",", na.strings = c("", " ", "null")) # ADJUST
raw_main_map <- read.csv2("data/main_questions.csv", sep = ";", na.strings = c("", " ", "null")) # ADJUST


# select relveant columns
participant_map <- raw_participant_map[,c("Participant.Private.ID",
                                               "Question.Key",)]

main_map <-  raw_main_map[,c("randomise_trials","drug")]
main_map <-  na.omit(main_map)
main_map$id <- seq_along(1:nrow(main_map))
colnames(main_map) <- c("difficulty", "drug", "id")




save(participant_map, main_map, file = "mapping_table.rda")