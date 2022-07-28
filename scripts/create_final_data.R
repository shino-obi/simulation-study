# FILE TO CREATE AND COMBINE ALL DATASETS

source("scripts/case_example_data_transformation.R")
source("scripts/evaluation_case_examples.R")
source("scripts/participant_data_transformation.R")

library(tidyverse)
library(MASS)

# adjust participant variables definition
participant_info <- participant_covar

# change colnames to keep original definiton in the data
participant_info$prof_raw <- participant_info$prof
participant_info$clinic_raw <- participant_info$clinic


# prof: physician + pediatrician = physician)
participant_info$prof <- fct_collapse(participant_info$prof, Physician = "Pediatrician")

# clinic: offizin + arztpraxis = public
participant_info$clinic <- fct_collapse(participant_info$clinic, "Ambulant" = c("Arztpraxis", "Offizin-Apotheke"))


# join datasets (for descriptive stats)
data_combined <- left_join(data_eval, participant_info, by = "p_id")


# adjust variable types
# block type as factor
data_combined$block_type <- factor(data_combined$block_type,
                                   levels = c(1,2,3),
                                   labels = c("control", "basic", "full"),
                                   ordered = T)

# ex_id as factor
data_combined$ex_id <- factor(data_combined$ex_id, levels = c(seq(1,18)))


# OUTLIER EXCLUSION
# log transform time and create new column

data_combined$log_time_raw <- log(data_combined$time)

data_combined$log_time <- data_combined$log_time_raw

# calculate 3 sd from mean
data_combined <- data_combined %>% 
      group_by(block_type) %>% mutate(time_3sd = mean(log_time, na.rm = T) + 3*sd(log_time, na.rm = T)) %>% ungroup()

print(data_combined %>% filter(log_time > time_3sd) %>% dplyr::select(key))

data_combined[data_combined$key == "5412833_7",]$log_time <- NA
data_combined[data_combined$key == "5412851_15",]$log_time <- NA

# CONTRASTS
# CLINIC
data_combined$clinic_c_sum <- data_combined$clinic
contrasts(data_combined$clinic_c_sum) <- contr.sum(levels(data_combined$clinic_c_sum))


# PROFESSION
data_combined$prof_c_sum <- data_combined$prof_raw
contrasts(data_combined$prof_c_sum) <- contr.sum(levels(data_combined$prof_c_sum))



# EXPERIENCE
# Difference
data_combined$exp_c_diff <- data_combined$exp
contrasts(data_combined$exp_c_diff) <- MASS::contr.sdif(levels(data_combined$exp_c_diff))

# Helmert
data_combined$exp_c_helm <- data_combined$exp
contrasts(data_combined$exp_c_helm) <- contr.helmert(levels(data_combined$exp_c_helm))


# PEDEDOSE USER
# Difference
data_combined$pededose_c_diff <- data_combined$pededose
contrasts(data_combined$pededose_c_diff) <- MASS::contr.sdif(levels(data_combined$pededose_c_diff))

# Helmert
data_combined$pededose_c_helm <- data_combined$pededose
contrasts(data_combined$pededose_c_helm) <- contr.helmert(levels(data_combined$pededose_c_helm))




# Participant ID as factor
data_combined$p_id <- factor(data_combined$p_id)

save(data_combined, file = "data/data_combined.rda")
save(participant_info, file = "data/participant_info.rda")

# to define error types
data_error_type <- data_combined %>% dplyr::select(p_id,
                                                   ex_id,
                                                   block_type,
                                                   response,
                                                   response_lower,
                                                   response_upper,
                                                   true_result,
                                                   true_lower,
                                                   true_upper,
                                                   margin_lower,
                                                   margin_upper,
                                                   tw_narrow,
                                                   is_error
)

write.csv2(x = data_error_type,file = "data_error_type.csv", fileEncoding = "UTF-8")
