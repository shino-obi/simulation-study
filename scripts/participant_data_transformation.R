library(tidyverse)
library(lubridate)


# read data
raw_participant_info <- read.csv2("data/data_exp_56915-v22_questionnaire-d1do.csv", sep = ",", na.strings = c("", " ", "null"))

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

# remove rows with only NA's
for (i in 1:nrow(participant_columns)) {
  if (all(is.na(participant_columns[i,])) == TRUE) {
    participant_columns <- participant_columns %>% slice(-i)
  }
}

# CREATE PARTICIPANT INFO DF
# convert df to wide format
participant_columns_wide <- pivot_wider(data = participant_columns, id_cols = Participant.Private.ID, names_from = Question.Key,values_from = Response)

participant_columns_wide$`BEGIN QUESTIONNAIRE` <- NULL
participant_columns_wide$`END QUESTIONNAIRE` <- NULL




# CREATE COVARIATES
participant_covar <- participant_columns_wide %>% select(Participant.Private.ID,
                                                         `hospital-1-quantised`,
                                                         `profession-1-quantised`,
                                                         `experience-1-quantised`,
                                                         `age-1`,
                                                         `pededose-1-quantised`,
                                                         `calc-1-quantised`
                                                         )

colnames(participant_covar) <- c("p_id",
                                 "clinic",
                                 "prof",
                                 "exp",
                                 "age",
                                 "pededose",
                                 "calc")
# ID as char
participant_covar$p_id <- as.character(participant_covar$p_id)

#### CHECK CHANGES!!!!!!!!!!!!!!
# clinic (1 = KISPI, 2 = OKS, 3 = UKBB)
participant_covar$clinic <- factor(x = participant_covar$clinic,
                                   levels = c(1,2,3),
                                   labels = c("KISPI", "OKS", "UKBB"))

# profession (1 = Ärztin/Arzt, 2 = Apo, 3 = Pflege, 4 = Pharma. Assi)
participant_covar$prof <- factor(x = participant_covar$prof,
                                 levels = c(1,2,3,4),
                                 labels = c("Physician", "Pharmacist", "Nurse", "PharmTech"))

# experience (1 = <5J, 2 = 5-10J, 3 = >10J)
participant_covar$exp <- factor(x = participant_covar$exp,
                                levels = c(1,2,3),
                                labels = c("<5y","5-10y",">10y"),
                                ordered = TRUE)

# age (in years) -> CHECK IF NOT BETTER AS FACTOR
participant_covar$age <- as.numeric(participant_covar$age)

# pededose (1 = nein, 2 = teilweise, 3 = ja)
participant_covar$pededose <- factor(x = participant_covar$pededose,
                                     levels = c(1,2,3),
                                     labels = c("no", "partly", "yes"),
                                     ordered = TRUE)

# calc (1 = no, 2 = yes) => change to: 0 = no, 1 = yes
participant_covar$calc <- if_else(condition = participant_covar$calc == "1",true = 0,false = 1)



# finalize transformation ONLY IF NECESSARY
data_covar <- participant_covar

# save file
#save(data_covar, file = "data_covar.rda")
