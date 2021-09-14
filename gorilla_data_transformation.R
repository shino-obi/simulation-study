library(tidyverse)
library(lubridate)


# add working directory path
wd <- "P:/PhD/02 Simulationsstudie/template_data"

setwd(wd)

# read data
raw_participant_info <- read.csv2("data_exp_56915-v22_questionnaire-d1do.csv", sep = ",", na.strings = c("", " ", "null"))
raw_main_data <- read.csv2("data_exp_56915-v22_task-ouv3.csv", sep = ",", na.strings = c("", " ", "null"))

# parse date
raw_participant_info$Local.Date <- lubridate::dmy_hms(raw_participant_info$Local.Date, tz = "CET")
raw_main_data$Local.Date <- lubridate::dmy_hms(raw_main_data$Local.Date, tz = "CET")




# get mapping tables for the participant and main questions
#load(file = "mapping_table.rda")


# Description of gorilla data set columns (https://support.gorilla.sc/support/reference/faq/metrics#datacolumns)
# Participant information
# column: Participant.Private.ID = individual participant ID (for merging data sets - check live)
# column: Question.Key = customary set question identifier
# column: Response = participant response

# Main study data
# Participant.Private.ID = individual participant ID (for merging data sets - check live)
# column: Spreadsheet.Row = original question number
# column: Trial.Number = participant specific question number/order
# column: Reaction.Time = participant response time in ms (divide by 1000)
# column: Response = participant response
# Filter participant responses using: Zone.Type == "response_text_entry"

# condition: randomise blocks == [number] where display == block






# select columns
participant_columns <- raw_participant_info[,c("Participant.Private.ID",
                                               "Question.Key",
                                               "Response")]

main_columns <- raw_main_data[,c("Local.Date",
                                 "Participant.Private.ID",
                                 "Spreadsheet.Row",
                                 "Trial.Number",
                                 "Zone.Type",
                                 "Reaction.Time",
                                 "Response",
                                 "randomise_blocks",
                                 "randomise_trials",
                                 "display")]


# get participant responses only
main_responses <-  main_columns %>% dplyr::filter(Zone.Type == "response_text_entry")

# check for duplicate entries and choose last response given by the participant
# first create id to identify duplicates
for (i in 1:nrow(main_responses)) {
  main_responses$dupli_id[[i]] <-
    paste(main_responses$Participant.Private.ID[[i]],
          main_responses$Trial.Number[[i]],
          sep = "-")
}
main_responses$dupli_id <- as.character(main_responses$dupli_id)

# create logical column indicating duplicates
main_responses$test_dupli <- duplicated(check_dupli$dupli_id)

# quantify duplicates in console output
summary(main_responses$test_dupli)

# select last response given by the participant
main_responses <- main_responses %>% group_by(dupli_id) %>% top_n(Local.Date, n = 1) %>% ungroup()







# finalize transformation
data_participant <- data.frame()
data_main <- data.frame()

# merge files into one data set
data <- data.frame()

# save file
#save(data, file = "data.rda")