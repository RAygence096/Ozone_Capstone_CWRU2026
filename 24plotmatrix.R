library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)


df <- readRDS("data_out/cleveland_2024_data.rds")


cor_data <- df %>%
  filter(parameter_code == "44201") %>%
  mutate(
   
    full_site_id = paste(state_code, county_code, site_number, sep = "-")
  ) %>%

  select(date_local, time_local, full_site_id, sample_measurement) %>%
  distinct(date_local, time_local, full_site_id, .keep_all = TRUE) %>%
 
  pivot_wider(names_from = full_site_id, values_from = sample_measurement) %>%
 
  select(-date_local, -time_local)


p_cor <- ggpairs(cor_data,
                 title = "2024 Cleveland Ozone: Site A vs Site B (Full AQS ID)",
                 lower = list(continuous = wrap("points", alpha = 0.2, size = 0.5)),
                 upper = list(continuous = wrap("cor", size = 3))) +
  theme_bw() +
  theme(axis.text = element_text(size = 6),
        strip.text = element_text(size = 7, face = "bold"))


ggsave("data_out/plots_2024/Task_A_Correlation_Matrix_FullID.png", 
       plot = p_cor, width = 15, height = 15, dpi = 300)