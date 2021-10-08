library(tidyverse)

load("data_covar.rda")
load("data_main.rda")



# Participant characteristics
table_covar <- data_covar %>% select(p_id,
                                     clinic, 
                                     prof, 
                                     exp, 
                                     pededose, 
                                     calc
                                     )

# Create a table long format of covariates to calculate frequency of categorical variables
## switch factor coding to character as pivot longer seems to have a problem with it
table_covar[] <- lapply(table_covar, as.character)

## long format
table_factor <- pivot_longer(data = table_covar, 
                             cols = c(clinic, 
                                      prof, 
                                      exp, 
                                      pededose, 
                                      calc), 
                             names_to = "variable", 
                             values_to = "values")  

## calc frequency of categorical variables
table_1_factor <- table_factor %>% group_by(variable, values) %>% tally()

## IQR of age
table_1_age <- data.frame(
  mean = mean(data_covar$age),
  median = median(data_covar$age),
  q25 = quantile(data_covar$age, probs = 0.25),
  q75 = quantile(data_covar$age, probs = 0.75))


# ERRORS
### Errors made over all exercises and all conditions
overall.total <- data_main %>%
  count(is_error)

overall.total$is_error <- NULL
overall.total$total_possible <- nrow(data_main)
overall.total$percent <- round(x = overall.total$n / overall.total$total_possible * 100, 
                               digits = 1)

### Errors made over all exercises stratified by condition
condition.total <- data_main %>%
  group_by(block_type) %>% 
  count(is_error) %>%
  ungroup()

condition.total$is_error <- NULL

### Errors made over all conditions stratified by exercise
exercise.total <- data_main %>%
  group_by(ex_id) %>%
  count(is_error) %>% 
  ungroup()

exercise.total$is_error <- NULL

### Errors made stratified by exercise and by condition
ex.cond.total <- data_main %>%
  group_by(block_type, ex_id) %>%
  count(is_error) %>% 
  ungroup()

ex.cond.total$is_error <- NULL




### sum of errors by question difficulty

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
