library(tidyverse)

load("data/data_combined.rda")
load("data/participant_info.rda")

data_combined$block_type <- factor(data_combined$block_type,
                                           levels = c(1,2,3),
                                           labels = c("control", "basic", "full"),
                                           ordered = T)

data_combined$ex_id <- factor(data_combined$ex_id, levels = c(seq(1,18)))




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


# SUMMARY STATS ARE SPLIT INTO TWO PARTS: "by block" and "by case example"


## BY BLOCK ############################################################################

### TIME

# mean time by participant and block 
time_participant_data <- data_combined %>%
                                group_by(p_id, block_type) %>%
                                summarise(mean_time = mean(time), .groups = "keep")

# mean time by block
time_block_data <- data_combined %>%
                        group_by(block_type) %>%
                        summarise(mean_time = mean(time))


#### violin plot for time distribution

violin_time_block <- ggplot(data = time_participant_data,
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

#### save plot
ggsave("plots/violin_time_block.jpg", plot = violin_time_block, dpi = 300)



### ERROR

# error count by participant and block 
error_participant_data <- data_combined %>%
                                group_by(p_id, block_type) %>%
                                summarise(count_error = sum(is_error, na.rm = T), .groups = "keep")
# error count by block
error_block_data <- data_combined %>%
                        group_by(block_type) %>%
                        summarise(count_error = sum(is_error, na.rm = T))


## BY CASE EXAMPLE ############################################################################

### TIME

# mean time by case_example and block 
time_ce_block_data <- data_combined %>%
        group_by(id_exercise_block, drug) %>%
        summarise(mean_time = mean(time), .groups = "keep")

# mean time by case_example
time_ce_data <- data_combined %>%
        group_by(ex_id, drug) %>%
        summarise(mean_time = mean(time), .groups = "keep")


#### violin plot for time distribution ------------------------------> REDO PLOT
violin_time_ce <- ggplot(data = time_ce_block_data,
                                aes(x = ex_id,
                                    y = mean_time,
                                    fill = block_type)
) +
        scale_fill_discrete() +
        geom_violin(position = "dodge") +
        geom_boxplot(width = 0.15,
                     color = "black",
                     lwd = 0.7
        ) +
        xlab("ex_id") +
        ylab("time [s]")

#### save plot
#ggsave("plots/violin_time_ce.jpg", plot = violin_time_ce, dpi = 300)


### ERROR

# error count by case_example and block
error_ce_block_data <- data_combined %>%
        group_by(id_exercise_block, ex_id, drug) %>%
        summarise(count_error = sum(is_error, na.rm = T), .groups = "keep")


# error count by case_example
error_ce_data <- data_combined %>%
        group_by(ex_id, drug) %>%
        summarise(count_error = sum(is_error, na.rm = T), .groups = "keep")


# count number of case_example for each block
count_ce_data <- data_combined %>%
        group_by(id_exercise_block) %>%
        tally()


error_ce_block_data <- left_join(error_ce_block_data, count_ce_data, by = "id_exercise_block")

## SAVE ALL SUMMARY FILES
save(list = c("time_participant_data",
              "time_block_data",
              "error_participant_data",
              "error_block_data",
              "time_ce_block_data",
              "time_ce_data",
              "error_ce_data",
              "error_ce_block_data"),
     file = "results/descriptive_stats.rda")

