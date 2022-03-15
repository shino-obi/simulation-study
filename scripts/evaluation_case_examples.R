# load transformed data
load(file = "data/data_main.rda")


# create column indicating calculation error = 1
####################################### CHECK COLUMN TYPE FIRST #######################################
data_main$is_error <- NA
data_main$is_error <- if_else(condition = 
                                data_main$response == data_main$true_result,
                               true = paste0(0),
                               false = paste0(1)
)


####################################### FOR PILOT #######################################
test <- data_main %>% select(p_id, block_type, ex_id, response, true_result, is_error) %>% filter(is_error == 1)
