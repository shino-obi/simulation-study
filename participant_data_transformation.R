library(tidyverse)
library(lubridate)


# add working directory path
wd <- "P:/PhD/02 Simulationsstudie/simulation-study"

setwd(wd)


# read data
raw_participant_info <- read.csv2("data_exp_56915-v22_questionnaire-d1do.csv", sep = ",", na.strings = c("", " ", "null"))

# parse date
raw_participant_info$Local.Date <- lubridate::dmy_hms(raw_participant_info$Local.Date, tz = "CET")

# Description of gorilla data set columns (https://support.gorilla.sc/support/reference/faq/metrics#datacolumns)

# Participant information
# column: Participant.Private.ID = individual participant ID (for merging data sets - check live)
# column: Question.Key = customary set question identifier
# column: Response = participant response




# select columns
participant_columns <- raw_participant_info[,c("Participant.Private.ID",
                                               "Question.Key",
                                               "Response")]

participant_columns_wide <- pivot_wider(data = participant_columns,id_cols = Participant.Private.ID, names_from = Question.Key,values_from = Response)


# CREATE PARTICIPANT INFO DF



# CREATE COVARIATES