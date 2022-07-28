library(tidyverse)
library(lme4)
library(ggeffects)
library(performance)
library(MASS)

load("data/data_combined.rda")
data_exploratory <- data_combined

# define variables
data_exploratory$is_error <- factor(data_exploratory$is_error,
                                    levels = c(0,1),
                                    labels = c(0,1)
)

data_exploratory$is_error <- if_else(condition = data_exploratory$is_error == 1, true = paste(0), false = paste(1))

data_exploratory$is_error <- as.factor(data_exploratory$is_error)


# CONTRASTS block_type
# Difference
data_combined$block_type_c_diff <- data_combined$block_type
contrasts(data_combined$block_type_c_diff) <- MASS::contr.sdif(levels(data_combined$block_type_c_diff))

# Helmert
data_combined$block_type_c_helm <- data_combined$block_type
contrasts(data_combined$block_type_c_helm) <- contr.helmert(levels(data_combined$block_type_c_helm))



model_e0 <- glmer(is_error ~ block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                  data = data_exploratory,
                  family = binomial(link = "logit"),
                  glmerControl(optimizer = "nlminbwrap")
)

# no EXP (-> because of singularity)
model_e1 <- glmer(is_error ~ block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                  data = data_exploratory,
                  family = binomial(link = "logit"),
                  glmerControl(optimizer = "nlminbwrap")
)

# no PROF
model_e2 <- glmer(is_error ~ block_type + clinic_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                  data = data_exploratory,
                  family = binomial(link = "logit"),
                  glmerControl(optimizer = "nlminbwrap")
)

# no CLINIC
model_e3 <- glmer(is_error ~ block_type + prof_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                  data = data_exploratory,
                  family = binomial(link = "logit"),
                  glmerControl(optimizer = "nlminbwrap")
)

# no PEDEDOSE
model_e4 <- glmer(is_error ~ block_type + prof_c_sum + clinic_c_sum + (block_type | p_id) + (block_type | ex_id), 
                  data = data_exploratory,
                  family = binomial(link = "logit"),
                  glmerControl(optimizer = "nlminbwrap")
)

# no PEDEDOSE
model_test <- glmer(is_error ~ block_type + (block_type | p_id) + (block_type | ex_id), 
                  data = data_exploratory,
                  family = binomial(link = "logit"),
                  glmerControl(optimizer = "nlminbwrap")
)

summary(model_e1)
summary(model_e2)
summary(model_e3)
summary(model_e4)

anova(model_e1, model_e2)
anova(model_e1, model_e3)
anova(model_e1, model_e4)


# TIME
# FULL
model_time_e0 <- lmer(log_time ~  block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_exploratory)
# no EXP
model_time_e1 <- lmer(log_time ~  block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_exploratory)
# no PROF
model_time_e2 <- lmer(log_time ~  block_type + clinic_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_exploratory)
# no CLINIC
model_time_e3 <- lmer(log_time ~  block_type + prof_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                    data = data_exploratory)
# no PEDEDOSE
model_time_e4 <- lmer(log_time ~  block_type + prof_c_sum + clinic_c_sum + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_exploratory)

anova(model_time_e0, model_time_e1)
anova(model_time_e0, model_time_e2)
anova(model_time_e0, model_time_e3)
anova(model_time_e0, model_time_e4)

summary(model_time_e0)
summary(model_time_e1)
summary(model_time_e2)
summary(model_time_e3)
summary(model_time_e4)

