library(tidyverse)
library(jsonlite)


####################################### PILOT #######################################
raw_log <- read.csv2("data_test/activitylog_example.csv", sep = ";", stringsAsFactors = F)



# select only pededosetest gmail logs

# select only calcdose logs
raw_calc_log <- raw_log %>% filter(context == "CALCULATOR.CALCDOSAGE")

# create two different df's for product and substance searches
product_calc_logs <- raw_calc_log %>% filter(str_detect(string = message, pattern = "^\\{\"p") == T)
substance_calc_logs <- raw_calc_log %>% filter(str_detect(string = message, pattern = "^\\{\"p", negate = T) == T)

# parsing json
product_logs <- product_calc_logs %>% 
  mutate(json = map(message, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json)

substance_logs <- substance_calc_logs %>% 
  mutate(json = map(message, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json)


# save file
#write.csv2(x = {R object} = "data_test/log_test.csv")
