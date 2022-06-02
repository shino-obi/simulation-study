library(tidyverse)

load("data/participant_info.rda")
load("data/data_main.rda")





### TIME ###


time_participant_data <- data_main %>% group_by(p_id, block_type) %>% summarise(mean_time = mean(time))

time_block_data <- data_main %>% group_by(block_type) %>% summarise(mean_time = mean(time))



### ERROR ###
time_participant_data$block_type <- factor(time_participant_data$block_type, levels = c(1,2,3), labels = c("control", "basic", "full"), ordered = T)

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



