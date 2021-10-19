library(tidyverse)
library(lubridate)

# create list with filenames
temp <- list.files(path = "data/raw_data_main/", pattern = "*.csv")

# initiate df's
temp_file <- data.frame()
raw_main_data <- data.frame()

# load csv files and bind them together
for (i in temp) {
  temp_file <- read.csv2(paste("data/raw_data_main/",i, sep = ""),
                          sep = ",",
                          na.strings = c("", " ", "null"),
                          fileEncoding = "UTF-8-BOM")
  raw_main_data <- rbind(raw_main_data, temp_file)
} 

# parse date
raw_main_data$Local.Date <- lubridate::dmy_hms(raw_main_data$Local.Date, tz = "CET")


# get mapping tables for the participant and main questions
#load(file = "mapping_table.rda")


# Description of gorilla data set columns (https://support.gorilla.sc/support/reference/faq/metrics#datacolumns)

# Main study data
# Participant.Private.ID = individual participant ID (for merging data sets - check live)
# column: Spreadsheet.Row = original question number
# column: Local.Date = use for ordering!
# column: Reaction.Time = participant response time in ms (divide by 1000)
# column: Response = participant response
# Filter participant responses using: Zone.Type == "response_text_entry"

# condition: randomise_blocks == [number] (1 = Ctrl, 2 = base, 3 = full) where display == block 

main_columns <- raw_main_data %>% select(Local.Date,
                                         Participant.Private.ID,
                                         Spreadsheet.Row,
                                         Trial.Number,
                                         Zone.Type,
                                         Reaction.Time,
                                         Response,
                                         randomise_blocks,
                                         randomise_trials,
                                         display
                                         )

# CREATE BLOCK MAP TO MATCH BLOCK ORDER WITH BLOCK TYPES 

# Note following block types: 1 = ctrl, 2 = base, 3 = full

block_map_raw <- main_columns %>%
                  dplyr::filter(display == "block") %>%
                    group_by(Participant.Private.ID) %>%
                      arrange(Local.Date, .by_group = TRUE) %>%
                        ungroup()

# remove duplicates blocks
block_map <- block_map_raw %>%
              group_by(Participant.Private.ID,
                       Trial.Number) %>%
                top_n(Local.Date, n = 1) %>%
                  ungroup()

# create final block order df
block_map <- block_map %>% 
              select(Participant.Private.ID, 
                     Trial.Number, 
                     randomise_blocks) 

colnames(block_map) <- c("p_id", "block_order", "block_type")


# CREATE DF WITH EXERCISE (EX) RESPONSES

# get participant responses only
main_responses <- main_columns %>% 
                    dplyr::filter(Zone.Type == "response_text_entry")

# check for duplicate entries and choose last response given by the participant
# first create id to identify duplicates
  for (i in 1:nrow(main_responses)) {
    main_responses$dupli_id[[i]] <- paste(main_responses$Participant.Private.ID[[i]],
                                    main_responses$Trial.Number[[i]],
                                    sep = "-")
    }

main_responses$dupli_id <- as.character(main_responses$dupli_id)

# create logical column indicating duplicates
main_responses$test_dupli <- duplicated(main_responses$dupli_id)

# quantify duplicates in console output
summary(main_responses$test_dupli)

# select last response given by the participant
main_responses <- main_responses %>%
                    group_by(dupli_id) %>%
                      top_n(Local.Date, n = 1) %>%
                        ungroup()

# create case example df 
case_examples_raw <- main_responses %>%
                      select(Participant.Private.ID,
                             Spreadsheet.Row,
                             Local.Date, 
                             Reaction.Time,
                             Response,
                             randomise_blocks,
                             randomise_trials)

# create id for case examples using Spreadsheet.Row id
case_examples <- case_examples_raw %>%
                  group_by(Participant.Private.ID) %>%
                    arrange(Local.Date, .by_group = TRUE) %>%
                      mutate(ex_order = row_number()) %>%
                        ungroup()


case_examples <- case_examples %>%
                  group_by(Participant.Private.ID) %>%
                    arrange(Spreadsheet.Row, .by_group = TRUE) %>%
                      mutate(ex_id = row_number()) %>%
                        ungroup()

case_examples$Spreadsheet.Row <- NULL
case_examples$randomise_blocks <- NULL
case_examples$Local.Date <- NULL

colnames(case_examples) <- c("p_id",
                             "time",
                             "response",
                             "difficulty",
                             "ex_order",
                             "ex_id"
                             )

case_examples$ex_order <- as.numeric(case_examples$ex_order)

# convert reaction time from milliseconds to seconds and round 
case_examples$time <- as.numeric(case_examples$time)

case_examples$time <- round(case_examples$time / 1000,
                            digits = 2)

# re-establish blocks via ex_order to match with block_order
case_examples <- case_examples %>%
  mutate(
    block_order = case_when(
      ex_order <= 6 ~ 1,
      ex_order > 6 & ex_order <= 12 ~ 2,
      ex_order > 12 ~ 3
    )
  )

case_examples$block_order <- as.character(case_examples$block_order)



# MERGE BLOCK MAP WITH CASE EXAMPLES
merged_case_examples <- left_join(case_examples,
                                  block_map, 
                                  by = c("p_id","block_order")
                                  )

# JOIN WITH CASE EXAMPLE MAP
# read case example mapping table
raw_main_map <- readxl::read_excel(path = "data/map_case_example_solutions.xlsx")

# join by ex_id
ex_results <- left_join(merged_case_examples,
                        raw_main_map,
                        by = "ex_id")

# clean participant responses
## Potential process flow:
### write code to extract all letters from the response field to check for inappropriate user inputs

### ex_response_cleaning <-  ex_results %>% select(ex_id, response)
### TO DO ### 
### (1) separate participants responses by "-" into two columns (No1, "-" , No2)
###


# check by block_type which solution is the correct one (1 = SwissMedicInfo, 2 & 3 = PEDeDose)
ex_results <- ex_results %>%
  mutate(true_result = if_else(condition = block_type == 1,
                               true = SwissMedicInfo,
                               false = PEDeDose)
  )

# create column indicating calculation error = 1
# CHECK COLUMN TYPE FIRST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ex_results$is_error <- NA
ex_results$is_error <- if_else(condition = ex_results$response == ex_results$true_result,
          true = paste0(0),
          false = paste0(1))

# calculate difference from true result for descriptive stats
# problem of dose ranges separated by "-", convert to numeric range?



# finalize transformation

data_main <- ex_results

############################ test <- data_main %>% select(drug, response, true_result, is_error, block_type, difficulty) %>% filter(is_error == 1)

# save file
save(data_main, file = "data/data_main.rda")

