library(tidyverse)
library(lme4)

load("data/data_combined.rda")
load("data/participant_info.rda")

# exclude participant no. 5412871 ???
#data_combined <- data_combined[!data_combined$p_id == 5412871,]

# define variables
data_combined$is_error <- factor(data_combined$is_error,
                                 levels = c(0,1),
                                 labels = c(0,1)
)

data_exploratory <- data_combined

data_exploratory$block_type <- factor(data_exploratory$block_type,
                                      levels = c(1,2,3),
                                      labels = c("control",
                                                 "basic",
                                                 "full")
)

# set contrasts
block_type.fdiff = matrix(c(2/3, -1/3, -1/3, 1/3, 1/3, -2/3), ncol = 2)
block_type.fdiff

contrasts(data_exploratory$block_type) <- block_type.fdiff

model_exp <- glmer(formula = is_error ~ (p_id | block_type), data = data_exploratory, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+08)))

summary(model_exp)

