## Simulation template

# load packages

library(tidyverse)

# read data
setwd("P:/PhD/02 Simulationsstudie/")
participants <-
  read.csv2(file = "template_data/participants_template.csv")


# generation of simulated template data
Q_no <- c()
for (i in seq(1:30)) {
  Q_no[i] <- i
}

data <- as.data.frame(Q_no)

data <- data %>% mutate(set =
                          case_when(Q_no <= 10 ~ "A",
                                    Q_no > 10 & Q_no <= 20 ~ "B",
                                    Q_no > 20 ~ "C"))


# replicate each id 3x for each condition
participants <- participants %>% slice(rep(row_number(), 3))

# function for the assingment of set, condition and order
random_assignment <- function(data, colname, sample_vector) {
  data %>%
    group_by(id) %>%
    mutate({{colname}} := sample(sample_vector, size = 3, replace = FALSE))
}

# apply assingment function
sample_set <- c("A", "B", "C")

sample_condition <- c("control", "base", "full")

sample_order <- c(1, 2, 3)

participants <-
  random_assignment(data = participants,
                    colname = "set",
                    sample_vector = sample_set)

participants <-
  random_assignment(data = participants,
                    colname = "condition",
                    sample_vector = sample_condition)

participants <-
  random_assignment(data = participants,
                    colname = "order",
                    sample_vector = sample_order)

# create full dataset
study_data <- participants %>%
                group_by(id) %>%
                left_join(., data, by = "set")

# add participant responses
sample_response <- c(0, 1)

study_data$response  <- sample(sample_response, size = 1500, replace = TRUE)

# save data in R and csv format
write.csv2(study_data, file = "template_data/simulated_data.csv")

save(study_data, file = "template_data/simulated_data.rda")


## Data analysis
