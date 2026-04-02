library(dplyr)
library(ggplot2)

# 1. 初始化 df 
df <- readRDS("data_out/cleveland_2024_data.rds") %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
    full_site_id = paste("39", county_code, site_number, sep = "-")
  )

seasons <- list(
  Winter = c("2024-01-14", "2024-01-20"),
  Spring = c("2024-04-14", "2024-04-20"),
  Summer = c("2024-05-01", "2024-05-07"),
  Fall   = c("2024-10-13", "2024-10-19")
)

# 2. 循环绘图
for (s_name in names(seasons)) {
  
  dates <- seasons[[s_name]]
  

  week_data <- df %>%
    filter(parameter_code == "44201" & date_local >= dates[1] & date_local <= dates[2]) %>%
    mutate(
      datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
      full_site_id = paste("39", county_code, site_number, sep = "-"),
      sample_measurement_ppb = sample_measurement * 1000
    )
  

  p <- ggplot(week_data, aes(x = datetime, y = sample_measurement_ppb, color = full_site_id)) +
    geom_line(linewidth = 0.8) +  
    geom_point(size = 1.5) +        
    
    facet_wrap(~full_site_id, ncol = 1, scales = "free_y") +
    
    scale_x_datetime(
      date_breaks = "1 day",   
      date_minor_breaks = "1 hour",
      date_labels = "%d %H:00",  
      expand = c(0, 0)             
    ) +
    
    labs(title = paste("2024", s_name, "Hourly Ozone Trend"), 
         subtitle = paste("Cleveland-area multi-site comparison |", dates[1], "to", dates[2]),
         x = "Time", y = "Ozone (ppb)") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      panel.grid.minor.x = element_line(color = "gray90", linewidth = 0.2), # 
      panel.grid.major.x = element_line(color = "gray70", linewidth = 0.5), # 
      legend.position = "none"
    )
  

  ggsave(paste0("data_out/plots_2024", tolower(s_name), "_2024_", dates[1], "_to_", dates[2], ".png"), 
         plot = p, width = 25, height = 40, limitsize = FALSE)
  
  message(paste("Successfully plotted:", s_name))
}