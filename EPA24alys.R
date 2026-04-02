# ============================================================
# CAPSTONE: Cleveland 2024 Air Quality Analysis
# Focus: Continuous Time Series for 2024 Data
# ============================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(GGally)

# Set working directory
setwd("/Users/raygence/Documents/CWRU/Ozone_Capstone")

# 1. Load 2024 Data
data_file <- "data_out/cleveland_2024_data.rds" 
message("Loading 2024 dataset: ", data_file)
df_raw <- readRDS(data_file)

# 2. Preprocess for Continuity
message("Preprocessing 2024 data...")
df_clean <- df_raw %>%
  filter(sample_measurement >= 0) %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
    date_obj = as.Date(date_local),
    site_id = paste(county_code, site_number, sep = "-")
  ) %>%
  # Aggregate to ensure one measurement per hour per site
  group_by(datetime, date_obj, site_id, parameter_code) %>%
  summarise(sample_measurement = mean(sample_measurement, na.rm = TRUE), .groups = "drop")

# Extract Ozone (44201)
o3_data <- df_clean %>% filter(parameter_code == "44201")

# Create output directory for 2024 plots
dir.create("data_out/plots_2024", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# Task (a): Inter-site Correlation (2024)
# ============================================================
message("Generating 2024 Task (a): Correlation Matrix...")

o3_wide <- o3_data %>%
  select(datetime, site_id, sample_measurement) %>%
  pivot_wider(names_from = site_id, values_from = sample_measurement) %>%
  select(-datetime)

try({
  png("data_out/plots_2024/task_a_correlation_2024.png", width = 1200, height = 1200, res = 150)
  print(ggpairs(o3_wide,
                title = "2024 Hourly Ozone: Inter-site Correlation",
                lower = list(continuous = wrap("points", alpha = 0.1, size = 0.5)),
                diag = list(continuous = wrap("densityDiag", fill = "purple", alpha = 0.3))) +
          theme_bw())
  dev.off()
})

# ============================================================
# Task (b): Full Year Continuous Time Series (2024)
# ============================================================
message("Generating 2024 Task (b): Continuous Full Year Plot...")

plot_b <- ggplot(o3_data, aes(x = datetime, y = sample_measurement)) +
  geom_line(color = "darkgreen", linewidth = 0.1, alpha = 0.6) +
  facet_wrap(~site_id, scales = "free_y", ncol = 2) +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b") +
  labs(title = "2024 Continuous Hourly Ozone Trends",
       subtitle = "Full year visualization across all Cleveland-area sites",
       y = "Ozone (ppm)", x = "Month") +
  theme_minimal()

ggsave("data_out/plots_2024/task_b_full_year_2024.png", plot_b, width = 12, height = 10)

# ============================================================
# CAPSTONE: 2024 Absolute Hourly Axis (1-Hour Intervals)
# 要求：X轴必须显示每一个小时的标签 (00, 01, 02 ... 23)
# ============================================================

library(dplyr)
library(ggplot2)
library(lubridate)

# 1. 读取数据
setwd("/Users/raygence/Documents/CWRU/Ozone_Capstone")
df_all <- readRDS("data_out/cleveland_2024_data.rds")

# 2. 预处理 (确保有 168 个点)
o3_hourly <- df_all %>%
  filter(parameter_code == "44201") %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
    site_id = paste(county_code, site_number, sep = "-")
  ) %>%
  filter(!is.na(sample_measurement) & sample_measurement >= 0) %>%
  select(datetime, site_id, sample_measurement)

# 3. 绘图循环 (强制每小时刻度)
seasons <- list(
  Winter = c("2024-01-14", "2024-01-20"),
  Spring = c("2024-04-14", "2024-04-20"),
  Summer = c("2024-07-14", "2024-07-20"),
  Fall   = c("2024-10-13", "2024-10-19")
)

for (s_name in names(seasons)) {
  start_dt <- as.POSIXct(paste(seasons[[s_name]][1], "00:00:00"))
  end_dt   <- as.POSIXct(paste(seasons[[s_name]][2], "23:59:59"))
  
  week_data <- o3_hourly %>%
    filter(datetime >= start_dt & datetime <= end_dt)
  
  if (nrow(week_data) > 0) {
    
    plot_c <- ggplot(week_data, aes(x = datetime, y = sample_measurement, color = site_id)) +
      # 连线
      geom_line(linewidth = 0.6) +
      # 画点 (验证每小时都有数据)
      geom_point(size = 0.8) + 
      
      facet_wrap(~site_id, scales = "free_y", ncol = 1) + # 单列排布，让横轴更长，防止太挤
      
      # --- 强制每小时刻度 (Force 1-Hour Breaks) ---
      scale_x_datetime(
        date_breaks = "1 hour",      # <--- 这里强制每小时一个刻度
        date_labels = "%a %H:00",    # 显示格式：周几 + 小时 (如 Mon 06:00)
        expand = expansion(mult = 0.01)
      ) +
      
      labs(title = paste("2024", s_name, "Ozone: Every Single Hour Labeled"),
           y = "Ozone (ppm)", x = "") +
      
      theme_bw() +
      theme(
        legend.position = "none",
        # --- 关键设置：让168个标签能放得下 ---
        axis.text.x = element_text(
          angle = 90,        # 垂直旋转 90 度
          vjust = 0.5,       # 垂直居中对齐
          hjust = 1,         # 右对齐
          size = 5           # 字体必须很小，否则会重叠
        ),
        panel.grid.major.x = element_line(color = "gray80", linewidth = 0.3), # 每小时画一条竖线
        strip.background = element_rect(fill = "#d9d9d9")
      )
    
    # 保存超宽图片 (Width = 20) 以确保横轴有足够空间
    filename <- paste0("data_out/plots_2024/task_c_", tolower(s_name), "_EVERY_HOUR.png")
    ggsave(filename, plot_c, width = 20, height = 10, limitsize = FALSE, dpi = 300)
    message("Saved: ", filename)
  }
}

# ============================================================
# Task (d): 2024 Chemistry - NO Titration Effect
# ============================================================
message("Generating 2024 Task (d): Chemistry Analysis...")

nox_data <- df_clean %>% 
  filter(parameter_code == "42603") %>%
  select(datetime, site_id, nox = sample_measurement)

chem_df <- inner_join(o3_data %>% select(datetime, site_id, o3 = sample_measurement), 
                      nox_data, by = c("datetime", "site_id"))

plot_d <- ggplot(chem_df, aes(x = nox, y = o3 * 1000)) + 
  geom_hex(bins = 40) +
  scale_fill_viridis_c(option = "viridis", trans = "log10") +
  geom_smooth(method = "gam", color = "red", se = FALSE) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "2024 Chemistry Analysis: NO Titration Effect",
       x = "NOx (ppb)", y = "Ozone (ppb)") +
  theme_dark()

ggsave("data_out/plots_2024/task_d_chemistry_2024.png", plot_d, width = 9, height = 7)

message("2024 Analysis complete! Check 'data_out/plots_2024' folder.")