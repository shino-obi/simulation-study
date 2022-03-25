library(tidyverse)

# load transformed data
load(file = "data/data_main.rda")


# split main dataset into df with dose ranges and with single dosages
data_range <-
  data_main %>% filter(str_detect(response, pattern = "-") == TRUE)

data_single <-
  data_main %>% filter(str_detect(response, pattern = "-") == FALSE)


# data_range: create 2 new columns for lower and upper dose (for response and true result)
# response
split_response <-
  str_split(string = data_range$response,
            pattern = "-",
            simplify = TRUE)

split_response <- as.data.frame(split_response)
colnames(split_response) <- c("response_lower", "response_upper")

# true result
split_true_result <-
  str_split(string = data_range$true_result,
            pattern = "-",
            simplify = TRUE)

split_true_result <- as.data.frame(split_true_result)
colnames(split_true_result) <- c("true_lower", "ture_upper")

# combine again
data_range <- cbind(data_range, split_response, split_true_result)

#



####################################### FOR PILOT #######################################
test <- data_main %>% select(p_id, block_type, ex_id, response, true_result, is_error) %>% filter(is_error == 1)
