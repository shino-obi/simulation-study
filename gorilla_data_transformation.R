library(tidyverse)
library(lubridate)


# add working directory path
wd <- "P:/PhD/02 Simulationsstudie/template_data"

setwd(wd)

# read data
raw_participant_info <- read.csv2("data-questionnaire-d1do.csv", sep = ",", na.strings = c("", " ", "null"))
raw_main_data <- read.csv2("data-task-ouv3.csv", sep = ",", na.strings = c("", " ", "null"))


# get mapping tables for the participant and main questions
load(file = "mapping_table.rda")


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

main_columns <- raw_main_data[,c("Participant.Private.ID",
                                 "Spreadsheet.Row",
                                 "Trial.Number",
                                 "Reaction.Time",
                                 "Response",
                                 "randomise_blocks",
                                 "randomise_trials",
                                 "display")]


# finalize transformation
data_participant <- data.frame()
data_main <- data.frame()

# merge files into one data set
data <- data.frame()

# save file
#save(data, file = "data.rda")