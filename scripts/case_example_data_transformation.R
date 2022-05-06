library(tidyverse)
library(lubridate)

# create list with filenames
temp <- list.files(path = "data/raw_data_main/", pattern = "*.csv")

# initiate df's
temp_file <- data.frame()
raw_main_data <- data.frame()

# load csv files and bind them together
for (i in temp) 
{
  temp_file <- read.csv2(paste("data/raw_data_main/",i, sep = ""),
                         sep = ",",
                         na.strings = c("", " ", "null"),
                         fileEncoding = "UTF-8-BOM"
  )
  raw_main_data <- rbind(raw_main_data, temp_file)
} 

####################################### PILOT #######################################
# raw_main_data <- read.csv2("data/data_exp_56915-v45_task-ouv3.csv", sep = ",", stringsAsFactors = F)


# parse date
raw_main_data$Local.Date <- lubridate::dmy_hms(raw_main_data$Local.Date, tz = "CET")


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

# one participant confused blocks -> p_id 5412868 -> need to switch control and base block
block_map_switch <- block_map %>%
                        filter(p_id == 5412868) %>% mutate(block_type = case_when(
                        block_type == 1 ~ 2,
                        block_type == 2 ~ 1,
                        block_type == 3 ~ 3))
                        
  
block_map_main <- block_map %>%
                        filter(p_id != 5412868)

block_map <- rbind(block_map_switch,block_map_main)



# CREATE DF WITH EXERCISE (EX) RESPONSES

# get participant responses only
main_responses <- main_columns %>% 
                    dplyr::filter(Zone.Type == "response_text_entry")

# check for duplicate entries and choose last response given by the participant
# first create id to identify duplicates
for (i in 1:nrow(main_responses)) {
    main_responses$dupli_id[[i]] <- paste(main_responses$Participant.Private.ID[[i]],
                                    main_responses$Trial.Number[[i]],
                                    sep = "-"
                                    )
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
                             randomise_blocks
                             )

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



colnames(case_examples) <- c("p_id",
                             "datetime",
                             "time",
                             "response",
                             "ex_order",
                             "ex_id"
                             )

case_examples$ex_order <- as.numeric(case_examples$ex_order)

# convert reaction time from milliseconds to seconds and round 
case_examples$time <- as.numeric(case_examples$time)

case_examples$time <- round(case_examples$time / 1000,
                            digits = 2
                            )

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
####################################### UPDATE MAPPING TABLE WITH TRUE RESULTS #######################################
raw_main_map <- readxl::read_excel(path = "data/map_case_example_solutions.xlsx")

# join by ex_id
ex_results <- left_join(merged_case_examples,
                        raw_main_map,
                        by = "ex_id"
              )


# clean participant responses (1) and check for inappropriate characters (2)
string_test <- ex_results[, c("p_id",
                              "ex_id",
                              "drug",
                              "response")]


# 1) clean responses
# remove whitespaces
string_test$response <-  str_replace_all(string = string_test$response,
                                         pattern = "[:blank:]",
                                         replacement = ""
)


# switch commas to points
string_test$response <-  str_replace_all(string = string_test$response,
                                         pattern = ",",
                                         replacement = "\\."
)

# 2) checking for inappropriate characters
# check for hyphen: - either 0 or 1 allowed, if correct = TRUE
string_test$str_hyphen <-  if_else(
                              condition = str_count(string = string_test$response,
                                                    pattern = "-") <= 1,
                              true = TRUE,
                              false = FALSE
                           )

# check for points: either 0, 1, or 2 allowed and 2 are only allowed when there is a hyphen, if correct = TRUE
string_test$str_points <- str_count(string = string_test$response,
                                    pattern = "\\."
                          )
                                  
string_test$str_points <- case_when(
                              string_test$str_points <= 1 ~ TRUE,
                              string_test$str_points == 2 & str_count(string = string_test$response,
                                                                       pattern = "-") == 1  ~ TRUE,
                              TRUE ~ FALSE                            
                            )


# check for letters: none allowed
string_test$str_letter <- str_detect(string = string_test$response,
                                     pattern = "[:alpha:]",
                                     negate = TRUE
                          )

# check for other characters: none allowed
string_test$str_punct <- str_replace_all(string = string_test$response, pattern = "[\\.-]", replacement = "")

string_test$str_punct <- str_detect(string = string_test$str_punct,
                                    pattern = "[:punct:]",
                                    negate = TRUE
                         )

# conditions to detect problematic input
string_test$str_clean <- if_else(condition = 
                                      string_test$str_hyphen == TRUE &
                                      string_test$str_points == TRUE &
                                      string_test$str_letter == TRUE &
                                      string_test$str_punct == TRUE,
                                   true = TRUE,
                                   false = FALSE
                            )

# summarize clean inputs
summary(string_test$str_clean) # If there is a FAlSE, there is a problem. If all TRUE, everything FINE
####################################### CHECK WHAT TO DO WITH UNCLEAN INPUTS #######################################

# standardize participant input (remove space and change commas to points)
ex_results$response <- str_replace_all(string = ex_results$response, pattern = ",",replacement = ".")
ex_results$response <- str_replace_all(string = ex_results$response, pattern = " ",replacement = "")


# check by block_type which solution is the correct one (1 = SwissMedicInfo, 2 & 3 = PEDeDose)
ex_results <- ex_results %>%
                mutate(true_result =
                         if_else(condition =
                                  block_type == 1,
                                 true = SwissMedicInfo,
                                 false = PEDeDose
                         )
                )



# create id combining exercise with block type
ex_results$id_exercise_block <- NA

for (i in 1:nrow(ex_results)) {
  ex_results$id_exercise_block[i] <- paste(ex_results$ex_id[i], ex_results$block_type[i], sep = "_")
}


# give nice name
data_main <- ex_results

# create unique id for each row by combining p_id and ex_id
data_main$key <- paste0(data_main$p_id, sep = "_", data_main$ex_id)


# save file
save(data_main, file = "data/data_main.rda")

