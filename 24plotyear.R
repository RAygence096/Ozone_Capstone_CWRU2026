library(dplyr)
library(ggplot2)

# 1. 初始化 df 
df <- readRDS("data_out/cleveland_2024_data.rds") %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
    full_site_id = paste("39", county_code, site_number, sep = "-")
  )

# 2. 筛选 + 计算日均值
daily_data <- df %>%
  filter(parameter_code == "44201") %>%
  mutate(
    full_site_id = paste("39", county_code, site_number, sep = "-"),
    sample_measurement_ppb = sample_measurement * 1000
  ) %>%
  group_by(full_site_id, date_local) %>%
  summarise(
    daily_avg_ppb = mean(sample_measurement_ppb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date_local = as.Date(date_local))

# 3. 绘图
p <- ggplot(daily_data, aes(x = date_local, y = daily_avg_ppb, color = full_site_id)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 0.8) +
  
  facet_wrap(~full_site_id, ncol = 1, scales = "free_y") +
  
  scale_x_date(
    date_breaks = "1 month",
    date_minor_breaks = "1 week",
    date_labels = "%b",
    expand = c(0.01, 0)
  ) +
  
  labs(
    title = "2024 Daily Average Ozone Trend",
    subtitle = "Cleveland-area multi-site comparison | 2024-01-01 to 2024-12-31",
    x = "Month", y = "Ozone (ppb, daily avg)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),
    panel.grid.minor.x = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "gray70", linewidth = 0.5),
    legend.position = "none"
  )

ggsave("data_out/plots_2024_full_year_daily_avg.png",
       plot = p, width = 30, height = 40, limitsize = FALSE)

message("Successfully plotted: Full Year 2024 Daily Average")