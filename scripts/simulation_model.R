library(tidyverse)
library(lme4)

load("data/data_combined.rda")
load("data/participant_info.rda")

# exclude participant no. 5412871 ???
#data_combined <- data_combined[!data_combined$p_id == 5412871,]

# define variables

data_combined <- data_combined %>% mutate(prev_use = case_when(
     pededose == "no" ~ paste("no"),
     pededose == "partly" & calc == "no calc" ~ paste("no"),
     pededose == "partly" & calc == "mostly no calc" ~ paste("partly"),
     pededose == "partly" & calc == "mostly with calc" ~ paste("yes"),
     pededose == "partly" & calc == "with calc" ~ paste("yes"),
     pededose == "yes" & calc == "no calc"~ paste("no"),
     pededose == "yes" & calc == "mostly no calc"~ paste("partly"),
     pededose == "yes" & calc == "mostly with calc"~ paste("yes"),
     pededose == "yes" & calc == "with calc"~ paste("yes")
))

data_combined$prev_use <- factor(x = data_combined$prev_use,
                                 levels = c("no", "partly", "yes"),
                                 labels = c("no", "partly", "yes"),
                                 ordered = TRUE)
