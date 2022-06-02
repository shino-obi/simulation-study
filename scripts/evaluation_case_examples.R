library(tidyverse)

# load transformed data
load(file = "data/data_main.rda")


####################################### AUTOMATED ERROR DETECTION #######################################

data_eval <- data_main %>% select(p_id,
                                  datetime,
                                  key,
                                  drug,
                                  block_type,
                                  response_lower,
                                  response_upper,
                                  true_lower,
                                  true_upper,
                                  tw_narrow
)

data_eval$response_lower <- as.numeric(data_eval$response_lower)
data_eval$response_upper <- as.numeric(data_eval$response_upper)
data_eval$true_lower <- as.numeric(data_eval$true_lower)
data_eval$true_upper <- as.numeric(data_eval$true_upper)

# add margin
data_eval$margin_lower <- if_else(condition = data_eval$tw_narrow == FALSE,
                                                true = 0.1*data_eval$true_lower,
                                                false = 0.05*data_eval$true_lower 
)
 
data_eval$margin_upper <- if_else(condition = data_eval$tw_narrow == FALSE,
                                                true = 0.1*data_eval$true_upper,
                                                false = 0.05*data_eval$true_upper 
)

# general error definition
data_eval$is_error <- if_else(condition = 
                                data_eval$response_lower < (data_eval$true_lower - data_eval$margin_lower) |
                                data_eval$response_upper > (data_eval$true_upper + data_eval$margin_upper),
                                              true = TRUE,
                                              false = FALSE
                                
                             
)

#### ----> check NA's

test <- data_eval %>% group_by(block_type) %>% tally(is_error)
data_error <- data_eval %>% filter(is_error == TRUE)                                              

