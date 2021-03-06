library(tidyverse)
library(jsonlite)
library(lubridate)
library(data.table)

####################################### PILOT #######################################
raw_log <- read.csv2("data_test/activitylog_example.csv", sep = ";", stringsAsFactors = F)


# select only pededosetest gmail logs




# select only calcdose logs
raw_calc_log <- raw_log %>% filter(context == "CALCULATOR.CALCDOSAGE")
raw_calc_log$id_code <- seq_along(along.with = raw_calc_log$id)

# create two different df's for product and substance searches
  #product_calc_logs <- raw_calc_log %>% filter(str_detect(string = message, pattern = "^\\{\"p") == T)
  #substance_calc_logs <- raw_calc_log %>% filter(str_detect(string = message, pattern = "^\\{\"p", negate = T) == T)



# parse json into a single list
parsed_raw <- purrr::map(raw_calc_log$message, jsonlite::fromJSON)

unlisted_jsonlist <- lapply(parsed_raw, unlist)

# convert list elements to df's
json_df_list <- lapply(unlisted_jsonlist, as.data.frame)



# rename columns
for (i in seq_along(json_df_list)) {
  json_df_list[[i]]$names <-  rownames(json_df_list[[i]])
  json_df_list[[i]]$values <- json_df_list[[i]]$`X[[i]]`
  json_df_list[[i]]$`X[[i]]` <- NULL
  rownames(json_df_list[[i]]) <- NULL
}

# convert to wide format
json_df <- lapply(json_df_list, pivot_wider, names_from = names, values_from = values)

# bind list to one df
json_df <- data.table::rbindlist(l = json_df, use.names = T, fill = T, idcol = "id")

test <-  json_df
colnames(test) <-  str_replace(string = colnames(test), pattern = "[:digit:]{2,}$", replacement = "")
colnames(test) <-  str_replace(string = colnames(test), pattern = "[3-9]$", replacement = "")



# create colnames from JSON string
list_col_names <- map(.x = parsed_data, names)
vector_col_names <- unlist(list_col_names)
col_names <- unique(vector_col_names[vector_col_names != ""])



# create df with colnames from before
log_data_frame <- data.frame(matrix(ncol = length(col_names), nrow = length(parsed_data)))
colnames(log_data_frame) <- col_names


for (i in seq(1:length(parsed_data))) {
  for (j in seq(1:length(parsed_data[[i]]))) {
    
        if_else(condition = names(parsed_data[[i]][j]) %in% colnames(log_data_frame),
                true = log_data_frame[i,j] <- parsed_data[[i]][[j]],
                false = next)
  }
}

# CHECK NUMBER OF COLUMNS !!!!!
final_log_data <- log_data_frame[,c(1:55)]
final_log_data$id_code <- seq_along(along.with = log_data_frame$product_id)


log_data <- left_join(raw_calc_log, final_log_data, by = "id_code")

# parse datetime
log_data$datetime <- ymd_hms(log_data$datetime)
log_data$birthday <- dmy(log_data$birthday)


# save file
save(log_data,file =  "data/log_data.rda")
