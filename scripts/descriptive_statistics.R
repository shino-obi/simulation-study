library(tidyverse)

load("data/data_combined.rda")
load("data/participant_info.rda")

data_combined$block_type <- factor(data_combined$block_type,
                                           levels = c(1,2,3),
                                           labels = c("control", "basic", "full"),
                                           ordered = T)


## TABLE 1 - PARTICIPANT INFO SUMMARY
### count each level of factors

table_1 <- data.frame()

for (i in c("clinic", "prof", "exp", "pededose", "calc")) {
        temp <- as.data.frame(fct_count(participant_info[[i]], prop = T), i)
        temp$variable <- i
        table_1 <- bind_rows(table_1, temp)
}

# make df pretty
colnames(table_1) <- c("levels","count","percent", "variable")
table_1$percent <- table_1$percent * 100



## BY CONDITION
### TIME
# mean time by participant and condition 
time_participant_data <- data_combined %>%
                                group_by(p_id, block_type) %>%
                                summarise(mean_time = mean(time), .groups = "keep")

# mean time by condition
time_block_data <- data_combined %>%
                        group_by(block_type) %>%
                        summarise(mean_time = mean(time))


#### violin plot for time distribution

ggplot(data = time_participant_data,
       aes(x = block_type,
           y = mean_time,
           fill = block_type)
       ) +
       scale_fill_manual(values = c("control" = "red",
                                    "basic" = "blue",
                                    "full" = "green")
       ) +
       geom_violin() +
       geom_boxplot(width = 0.15,
                    color = "black",
                    lwd = 0.7
                    ) +
       xlab("conditions") +
       ylab("time [s]")



### ERROR
error_participant_data <- data_combined %>%
                                group_by(p_id, block_type) %>%
                                summarise(count_error = all(is_error), .groups = "keep")
error_block_data <- 



## BY CASE EXAMPLE
### TIME


#### violin plot for time distribution



### ERROR


