library(dplyr)
library(ggplot2)



# 1. 读取并处理全年前体物数据
#df_year <- readRDS("data_out/cleveland_2024_CO_VOC.rds") %>%
#  mutate(
#    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
#    full_site_id = paste("39", county_code, site_number, sep = "-")
#  )
#p_year <- ggplot(df_year, aes(x = datetime, y = sample_measurement, color = parameter)) +
#  geom_line(linewidth = 0.5, alpha = 0.8) + # 全年数据点太多，建议只用线，不用点
#  facet_grid(parameter ~ full_site_id, scales = "free_y") +
  
#  scale_x_datetime(
 #   date_breaks = "1 month",   # 每月一个刻度
 #   date_labels = "%b",        # 显示月份缩写 (Jan, Feb...)
#    expand = c(0, 0)
#  ) +
  
#  labs(title = "Full Year 2024 Precursor Trends (CO & VOCs)",
 #      subtitle = "Cleveland Area Monitoring Sites | All Valid Hourly Data",
 #      x = "Month", y = "Concentration") +
 # theme_bw() +
  #theme(
#    strip.text = element_text(face = "bold", size = 9),
#    panel.grid.minor = element_blank(),
#    legend.position = "none"
 # )

# 3. 保存 (宽度设为 20 英寸以展开一年的波动)
#ggsave("data_out/plots_2024/CO_VOC_FullYear_2024.png", 
  #     plot = p_year, width = 40, height = 12)

#message("Full year plot generated: data_out/plots_2024/CO_VOC_FullYear_2024.png")

# 1. 读取前体物数据 (假设你之前保存的文件名如下)
df_pre <- readRDS("data_out/cleveland_2024_data.rds")

seasons <- list(
  Winter = c("2024-01-14", "2024-01-20"),
  Spring = c("2024-04-14", "2024-04-20"),
  Summer = c("2024-05-01", "2024-05-07"),
  Fall   = c("2024-10-14", "2024-10-20")
)

# 2. 循环绘制季节图
for (s_name in names(seasons)) {
  
  dates <- seasons[[s_name]]
  
  week_data <- df_pre %>%
    filter(date_local >= dates[1] & date_local <= dates[2]) %>%
    mutate(
      datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
      # 统一使用你建议的 ID 格式，并处理可能缺失的 state_code
      full_site_id = paste("39", county_code, site_number, sep = "-")
    )
  
  # 3. 绘图：利用 facet_grid 实现 [污染物 x 站点] 的矩阵对比
  p <- ggplot(week_data, aes(x = datetime, y = sample_measurement, color = parameter)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    # 核心改动：纵轴按污染物(parameter)分栏，横轴按站点(full_site_id)分栏
    # 如果站点太多，可以只保留 facet_wrap(~parameter + full_site_id, ncol = 1)
    facet_grid(parameter ~ full_site_id, scales = "free_y") +
    
    scale_x_datetime(
      date_breaks = "12 hours", # 7天图建议12小时一格，否则文字太挤
      date_labels = "%d %H:00",
      expand = c(0, 0)
    ) +
    
    labs(title = paste("2024", s_name, "Precursor Trends (CO, NOx, VOCs)"),
         subtitle = paste("Cleveland Area |", dates[1], "to", dates[2]),
         x = "Time", y = "Concentration (Variable Units)") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
      strip.text = element_text(face = "bold", size = 8),
      legend.position = "none"
    )
  
  # 4. 保存 (增加高度以容纳三种污染物)
  ggsave(paste0("data_out/plots_2024/CO", tolower(s_name), "_2024.png"), 
         plot = p, width = 25, height = 15, limitsize = FALSE)
  
  message(paste("Successfully plotted:", s_name))
}