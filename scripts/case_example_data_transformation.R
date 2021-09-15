library(tidyverse)
library(lubridate)


# read data
raw_main_data <- read.csv2("data/data_exp_56915-v22_task-ouv3.csv", sep = ",", na.strings = c("", " ", "null"))

# parse date
raw_main_data$Local.Date <- lubridate::dmy_hms(raw_main_data$Local.Date, tz = "CET")


# get mapping tables for the participant and main questions
#load(file = "mapping_table.rda")


# Description of gorilla data set columns (https://support.gorilla.sc/support/reference/faq/metrics#datacolumns)

# Main study data
# Participant.Private.ID = individual participant ID (for merging data sets - check live)
# column: Spreadsheet.Row = original question number
# column: Trial.Number = participant specific question number/order
# column: Reaction.Time = participant response time in ms (divide by 1000)
# column: Response = participant response
# Filter participant responses using: Zone.Type == "response_text_entry"

# condition: randomise_blocks == [number] (1 = Ctrl, 2 = base, 3 = full) where display == block 



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


# CREATE BLOCK MAP TO MATCH BLOCK ORDER WITH BLOCK TYPES 

# Note following block types: 1 = ctrl, 2 = base, 3 = full

block_map_raw <- 
  raw_main_data %>%
  dplyr::filter(display == "block") %>%
  group_by(Participant.Private.ID) %>%
  arrange(Local.Date, .by_group = TRUE) %>% ungroup()

# remove duplicates blocks
block_map <- 
  block_map_raw %>%
  group_by(Participant.Private.ID, Trial.Number) %>%
  top_n(Local.Date, n = 1) %>% ungroup()

# create final block order df
block_map <- 
  block_map %>% 
  select(Participant.Private.ID, 
         Trial.Number, 
         randomise_blocks) 

colnames(block_map) <- c("p_id", "block_order", "block_type")



# CREATE DF WITH CASE EXAMPLE (CE) RESPONSES

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
main_responses <- 
  main_responses %>%
  group_by(dupli_id) %>%
    top_n(Local.Date, n = 1) %>% ungroup()

# create case example df 
case_examples_raw <- 
  main_responses %>%
  select(Participant.Private.ID, 
         Spreadsheet.Row, Trial.Number, 
         Reaction.Time, Response, 
         randomise_trials)

# create id for case examples using Spreadsheet.Row id
case_examples <- 
  case_examples_raw %>%
  group_by(Participant.Private.ID) %>%
    arrange(Spreadsheet.Row, .by_group = TRUE) %>%
      mutate(ce_id = row_number()) %>% ungroup()

case_examples$Spreadsheet.Row <- NULL

colnames(case_examples) <- c("p_id", "ce_order", "time", "response", "difficulty", "ce_id")
case_examples$ce_order <- as.numeric(case_examples$ce_order)

# convert reaction time from miliseconds to seconds and round 
case_examples$time <- as.numeric(case_examples$time)

case_examples$time <- round(case_examples$time/1000, digits = 2)

# re-establish blocks via ce_order to match with block_order

case_examples <- case_examples %>%
  mutate(
    block_order = case_when(
      ce_order <= 6 ~ 1,
    ce_order > 6 & ce_order <= 12 ~ 2,
    ce_order > 12 ~ 3
    )
  )

case_examples$block_order <- as.character(case_examples$block_order)



# MERGE BLOCK MAP WITH CASE EXAMPLES

merged_case_examples <- left_join(case_examples, block_map, by = "block_order", "p_id")
merged_case_examples <- merged_case_examples %>% filter(p_id.x == p_id.y)
merged_case_examples$p_id.y <- NULL


# JOIN WITH CASE EXAMPLE MAP

# join by ce_id
# check by block_type which solution is the correct one



# finalize transformation

data_main <- data.frame()


# save file
#save(data_main, file = "data_main.rda")
