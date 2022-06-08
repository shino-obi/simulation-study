library(tidyverse)

# load transformed data
load(file = "data/data_main.rda")


####################################### AUTOMATED ERROR DETECTION #######################################

data_eval <- data_main
data_eval$p_id <- as.character(data_eval$p_id)

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
                                              true = 1,
                                              false = 0
)

save(data_eval, file = "data/data_eval.rda")
