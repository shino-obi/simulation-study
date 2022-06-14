library(tidyverse)

load("data/data_combined.rda")
load("data/participant_info.rda")

data_combined$block_type <- factor(data_combined$block_type,
                                           levels = c(1,2,3),
                                           labels = c("control", "basic", "full"),
                                           ordered = T)

data_combined$ex_id <- factor(data_combined$ex_id, levels = c(seq(1,18)))

# exclude participant no. 5412871 ???
#data_combined <- data_combined[!data_combined$p_id == 5412871,]


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
                        summarise(mean_time = mean(time), "0.75" = quantile(time, 3/4), "0.25" = quantile(time, 1/4)) %>% unique()


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
        ylab("time [s]") +
        ggtitle(label = "Summary of mean time per participant for each condition")

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


summary_block <- left_join(error_block_data, time_block_data, by = "block_type")
summary_participant <- left_join(error_participant_data, time_participant_data, by = c("p_id", "block_type"))

## BY CASE EXAMPLE ############################################################################

### TIME

# mean time by case_example and block 
time_ce_block_data <- data_combined %>%
        group_by(id_exercise_block, drug, block_type) %>%
        summarise(mean_time = mean(time), .groups = "keep")

# mean time by case_example
time_ce_data <- data_combined %>%
        group_by(ex_id, drug) %>%
        summarise(mean_time = mean(time), .groups = "keep")


#### violin plot for time distribution ------------------------------> REDO PLOT
time_data_for_plot <- data_combined %>% select(id_exercise_block, block_type, ex_id, p_id, time)
id_ce_block_counts <- time_data_for_plot %>% group_by(id_exercise_block) %>% tally()

time_data_for_plot <- left_join(time_data_for_plot, id_ce_block_counts, by = "id_exercise_block")

time_data_for_plot$labels <- paste(time_data_for_plot$block_type, " (n = ", time_data_for_plot$n , ")", sep = "") 


# make plot for each exercise

#for (j in seq(1:18)) {
        plot_data <- time_data_for_plot %>% filter(ex_id == j)
        
        label_names <- unique(plot_data$labels)
        
        name_1 <- str_which(string = label_names,
                            pattern = "control")
        name_2 <- str_which(string = label_names,
                            pattern = "basic")
        name_3 <- str_which(string = label_names,
                            pattern = "full")
        dynamic_labels <- c(label_names[name_1],label_names[name_2],label_names[name_3])
        
        plot_data$labels <- factor(x = plot_data$labels,
                                   levels = c(dynamic_labels[1],
                                              dynamic_labels[2],
                                              dynamic_labels[3]),
                                   labels = c(dynamic_labels[1],
                                              dynamic_labels[2],
                                              dynamic_labels[3]),
                                   ordered = T) 
        
        violin_time_ce <- ggplot(data = plot_data,
                                 aes(x = labels,
                                     y = time,
                                     fill = block_type)
        ) +
                scale_x_discrete(waiver()) +
                scale_fill_manual(values = c("control" = "red",
                                             "basic" = "blue",
                                             "full" = "green")) +
                
                geom_violin() +
                geom_boxplot(width = 0.15,
                             color = "black",
                             lwd = 0.7
                ) +
                xlab("condition") +
                ylab("time [s]") +
                ggtitle(label = paste("Case example ", j, " - Summary of participant time for each condition", sep = ""))
        
        plot(violin_time_ce)
 
        #### save plot
        ggsave(filename = paste("plots/ce_plots/violin_time_ce_",j, ".jpg", sep = ""), plot = violin_time_ce, dpi = 300)
#}

### ERROR

# error count by case_example and block
error_ce_block_data <- data_combined %>%
        group_by(id_exercise_block, drug) %>%
        summarise(count_error = sum(is_error, na.rm = T), .groups = "keep")


# count number of case_example for each block
count_ce_block_data <- data_combined %>%
        group_by(id_exercise_block) %>%
        tally()


# error count by case_example
error_ce_data <- data_combined %>%
        group_by(ex_id, drug) %>%
        summarise(count_error = sum(is_error, na.rm = T), .groups = "keep")


# combine errors by case_example with number of solved case_examples
error_ce_block_data <- left_join(error_ce_block_data, count_ce_block_data, by = "id_exercise_block")


# combine time and error by case_example
summary_ce_block <- left_join(error_ce_block_data, time_ce_block_data[,c("id_exercise_block", "mean_time")], by = "id_exercise_block")
summary_ce <- left_join(error_ce_data, time_ce_data[, c("ex_id","mean_time")], by = "ex_id")


## SAVE ALL SUMMARY FILES
save(list = c("summary_block",
              "summary_participant",
              "summary_ce_block",
              "summary_ce"),
     file = "results/descriptive_stats.rda")

write.csv2(x = table_1, file = "table_1.csv")

