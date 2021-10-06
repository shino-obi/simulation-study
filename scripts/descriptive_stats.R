library(tidyverse)

load("data_covar.rda")
load("data_main.rda")



# Participant characteristics

covar_factor <- data_covar %>% select(p_id, clinic, prof, exp, pededose, calc)

# create a list with df frequency table for each variable
table_1 <- list()
for (i in 2:ncol(covar_factor)) {
    table_1[[i - 1]]  <- as.data.frame(table(covar_factor[,i]))
      
    }


for (i in seq(table_1)) {
  assign(paste0("table_1_", i), table_1[[i]])
  }

table_1_age <- data.frame(
  mean = mean(data_covar$age),
  median = median(data_covar$age),
  q25 = quantile(data_covar$age, probs = 0.25),
  q75 = quantile(data_covar$age, probs = 0.75)
)

# ERRORS

### sum of errors

#### total

#### by condition


### sum of errors by question difficulty

#### total

#### by condition



### sum of total errors by each question

#### total

#### by condition


## Error Types

### Decimal errors (e.g. 10-fold)

#### total

#### by condition


### Subgroup of difficult questions: Violation of maximal dose 

#### total

#### by condition


### Subgroup of difficult questions: Body surface calculation

#### total

#### by condition


### Subgroup of difficult questions: Preterm age

#### total

#### by condition


### Clinically relevant errors

#### total

#### by condition


### Repetitions misunderstood in conventional dosing information

#### total


### Transcription errors (verified by PEDeDose calculator logging data)

#### total



# TIME

## Time per question by condition

## Sum of time of normal vs hard questions by condition

## Sum time per participant of all questions



# Time-Error relationship
