wd <- "P:/PhD/02 Simulationsstudie/template_data"

setwd(wd)

raw_participant_info <- read.csv2("data-task-ouv3.csv", sep = ",")
raw_main_data <- read.csv2("data-questionnaire-d1do", sep = ",")


# Description of data set columns (https://support.gorilla.sc/support/reference/faq/metrics#datacolumns)
# Participant information
# Participant.Private.ID = individual participant ID (for merging data sets - check live)


# Main study data
# Participant.Private.ID = individual participant ID (for merging data sets - check live)
# column: Reaction.Time = participant response time in ms (divide by 1000)
# column: Spreadsheet.Row = original question number
# column: Trial.Number = participant specific question number/order
# Filter participant responses using: Zone.Type == response_text_entry
