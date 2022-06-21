library(tidyverse)
library(lme4)

load("data/data_combined.rda")
load("data/participant_info.rda")

# exclude participant no. 5412871 ???
#data_combined <- data_combined[!data_combined$p_id == 5412871,]

# define variables
data_combined$is_error <- factor(data_combined$is_error, levels = c(0,1), labels = c(0,1))


data_confirmatory <- data_combined %>% filter(block_type != "2")

# refactor (before -> ctrl = 1, full = 3)
data_confirmatory$block_type <- if_else(condition = data_confirmatory$block_type == 1,
                                        true = paste(0),
                                        false = paste(1))

data_confirmatory$block_type <- factor(data_confirmatory$block_type,
                                       levels = c(0,1),
                                       labels = c("control",
                                                  "PEDeDose"))

data_confirmatory$p_id <- factor(data_confirmatory$p_id)

contrasts()

model <- glmer(formula = is_error ~ (1 | p_id) + (1 + p_id | block_type), data = data_confirmatory, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+08)))

summary(model)

VarCorr(model, 2)
