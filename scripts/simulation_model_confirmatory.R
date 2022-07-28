library(tidyverse)
library(lme4)
library(ggeffects)
library(performance)
library(emmeans)
load("data/data_combined.rda")


data_confirmatory <- data_combined %>% filter(block_type != "basic")


data_confirmatory$block_type <- fct_drop(f = data_confirmatory$block_type, only = "basic")

data_confirmatory$is_error <- if_else(condition = data_confirmatory$is_error == 1, true = paste(0), false = paste(1))
data_confirmatory$is_error <- as.factor(data_confirmatory$is_error)

#data_confirmatory$is_error[is.na(data_confirmatory$is_error) ] <- 0

# ERRORS

# Singularity

# model_0 <- glmer(is_error ~ block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
#                  data = data_confirmatory,
#                  family = binomial(link = "logit"),
#                  glmerControl(optimizer = "nlminbwrap")
# )

# no EXP (-> because of singularity)
model_1 <- glmer(is_error ~ block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                 data = data_confirmatory,
                 family = binomial(link = "logit"),
                 glmerControl(optimizer = "nlminbwrap")
)

# no PROF
model_2 <- glmer(is_error ~ block_type + clinic_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                  data = data_confirmatory,
                  family = binomial(link = "logit"),
                  glmerControl(optimizer = "nlminbwrap")
)

# no CLINIC
model_3 <- glmer(is_error ~ block_type + prof_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                            data = data_confirmatory,
                            family = binomial(link = "logit"),
                            glmerControl(optimizer = "nlminbwrap")
)

# no PEDEDOSE
model_4 <- glmer(is_error ~ block_type + prof_c_sum + clinic_c_sum + (block_type | p_id) + (block_type | ex_id), 
                 data = data_confirmatory,
                 family = binomial(link = "logit"),
                 glmerControl(optimizer = "nlminbwrap")
)

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

anova(model_1, model_2)
anova(model_1, model_3)
anova(model_1, model_4)

VarCorr(model_1, 2)


model_emm <- ggemmeans(model_1, c("block_type"))
plot(model_emm)


model_performance(model_1)



# TIME
# FULL
model_time_0 <- lmer(log_time ~  block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_confirmatory)
# no EXP
model_time_1 <- lmer(log_time ~  block_type + prof_c_sum + clinic_c_sum + pededose_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_confirmatory)
# no PROF
model_time_2 <- lmer(log_time ~  block_type + clinic_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_confirmatory)
# no CLINIC
model_time_3 <- lmer(log_time ~  block_type + prof_c_sum + pededose_c_diff + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_confirmatory)
# no PEDEDOSE
model_time_4 <- lmer(log_time ~  block_type + prof_c_sum + clinic_c_sum + exp_c_diff + (block_type | p_id) + (block_type | ex_id), 
                     data = data_confirmatory)

anova(model_time_0, model_time_1)
anova(model_time_0, model_time_2)
anova(model_time_0, model_time_3)
anova(model_time_0, model_time_4)

summary(model_time_0)
summary(model_time_1)
summary(model_time_2)
summary(model_time_3)
summary(model_time_4)
