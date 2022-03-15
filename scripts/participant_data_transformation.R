library(tidyverse)
library(lubridate)


# create list with filenames
temp <- list.files(path = "data/raw_data_participants/", pattern = "*.csv")

# initiate df's
temp_file <- data.frame()
raw_participant_info <- data.frame()

# load csv files and bind them together
for (i in temp) {
  temp_file <- read.csv2(paste("data/raw_data_participants/",i, sep = ""),
                         sep = ",",
                         na.strings = c("", " ", "null"),
                         fileEncoding = "UTF-8-BOM")
  raw_participant_info <- rbind(raw_participant_info, temp_file)
} 

# parse date
raw_participant_info$Local.Date <- lubridate::dmy_hms(raw_participant_info$Local.Date, tz = "CET")

# use only date
raw_participant_info$Local.Date <- lubridate::date(raw_participant_info$Local.Date)

# Description of gorilla data set columns (https://support.gorilla.sc/support/reference/faq/metrics#datacolumns)

# Participant information
# column: Participant.Private.ID = individual participant ID (for merging data sets - check live)
# column: Question.Key = customary set question identifier
# column: Response = participant response


# select columns
participant_columns <- raw_participant_info %>% select(`Participant.Private.ID`,
                                                       `Local.Date`,
                                                       `Question.Key`,
                                                       `Response`)

# remove rows with only NA's
for (i in 1:nrow(participant_columns)) {
  if (all(is.na(participant_columns[i,])) == TRUE) {
    participant_columns <- participant_columns %>% slice(-i)
  }
}

# CREATE PARTICIPANT INFO DF
# remove `BEGIN QUESTIONNAIRE` and `END QUESTIONNAIRE` values
participant_columns <- participant_columns %>% filter(`Question.Key` != "BEGIN QUESTIONNAIRE" & 
                                       `Question.Key` != "END QUESTIONNAIRE") 
                                           

# convert df to wide format
participant_columns_wide <- pivot_wider(data = participant_columns, id_cols = c(`Participant.Private.ID`, `Local.Date`), names_from = `Question.Key`, values_from = `Response`)





# CREATE COVARIATES
participant_covar <- participant_columns_wide %>% select(`Participant.Private.ID`,
                                                         `hospital-1-quantised`,
                                                         `profession-1-quantised`,
                                                         `experience-1-quantised`,
                                                         `pededose-1-quantised`,
                                                         `calc-1-quantised`
                                                         )

colnames(participant_covar) <- c("p_id",
                                 "clinic",
                                 "prof",
                                 "exp",
                                 "pededose",
                                 "calc")
# ID as char
participant_covar$p_id <- as.character(participant_covar$p_id)

#### CHECK CHANGES!!!!!!!!!!!!!!
# clinic (1 = KISPI, 2 = OKS, 3 = UKBB)
participant_covar$clinic <- factor(x = participant_covar$clinic,
                                   levels = c(1,2,3,4),
                                   labels = c("Kinderspital", "Spital", "Arztpraxis", "Offizin-Apotheke"))

# profession (1 = Kinderärztin/Kinderarzt, 2 = Ärztin/Arzt, 3 = Apo)
participant_covar$prof <- factor(x = participant_covar$prof,
                                 levels = c(1,2,3),
                                 labels = c("Pediatrician","Physician", "Pharmacist"))

# experience (1 = <5J, 2 = 5-10J, 3 = >10J)
participant_covar$exp <- factor(x = participant_covar$exp,
                                levels = c(1,2,3),
                                labels = c("<5y","5-10y",">10y"),
                                ordered = TRUE)


# pededose (1 = nein, 2 = teilweise, 3 = ja)
participant_covar$pededose <- factor(x = participant_covar$pededose,
                                     levels = c(1,2,3),
                                     labels = c("no", "partly", "yes"),
                                     ordered = TRUE)

# calc (1 = no, 2 = yes) => change to: 0 = no, 1 = yes
participant_covar$calc <- if_else(condition = participant_covar$calc == "1",true = 0,false = 1)

participant_covar$calc <- factor(x = participant_covar$calc,
                                 levels = c(0,1),
                                 labels = c("no", "yes"))


# finalize transformation ONLY IF NECESSARY
data_covar <- participant_covar

# save file
save(data_covar, file = "data/data_covar.rda")

