# ============================================================
# CAPSTONE: Cleveland 2023/2024 Air Quality Analysis (Fixed)
# ============================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(GGally)

# Set working directory
setwd("/Users/raygence/Documents/CWRU/Ozone_Capstone")

# 1. Load Data
data_file <- "data_out/cleveland_2023_data.rds" 
message("Loading dataset: ", data_file)
df_raw <- readRDS(data_file)

# 2. Preprocess and Clean
message("Preprocessing data for continuity...")
df_clean <- df_raw %>%
  filter(sample_measurement >= 0) %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
    # Convert date_local to Date object to ensure reliable filtering
    date_obj = as.Date(date_local),
    site_id = paste(county_code, site_number, sep = "-")
  ) %>%
  # IMPORTANT: Keep date_obj in the group_by so it isn't dropped
  group_by(datetime, date_obj, site_id, parameter_code) %>%
  summarise(sample_measurement = mean(sample_measurement, na.rm = TRUE), .groups = "drop")

# Extract Ozone (44201)
o3_data <- df_clean %>% filter(parameter_code == "44201")

# Create output directory
dir.create("data_out/plots", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# Task (a): Inter-site Correlation Matrix
# ============================================================
message("Generating Task (a): Correlation Matrix...")

o3_wide <- o3_data %>%
  select(datetime, site_id, sample_measurement) %>%
  pivot_wider(names_from = site_id, values_from = sample_measurement) %>%
  select(-datetime)

try({
  png("data_out/plots/task_a_correlation_matrix.png", width = 1200, height = 1200, res = 150)
  print(ggpairs(o3_wide,
                title = "Hourly Ozone: Site Correlation",
                lower = list(continuous = wrap("points", alpha = 0.1, size = 0.5)),
                diag = list(continuous = wrap("densityDiag", fill = "blue", alpha = 0.3))) +
          theme_bw())
  dev.off()
})

# ============================================================
# Task (b): Full Year Continuous Time Series
# ============================================================
message("Generating Task (b): Continuous Full Year Plot...")

plot_b <- ggplot(o3_data, aes(x = datetime, y = sample_measurement)) +
  geom_line(color = "darkblue", linewidth = 0.1, alpha = 0.6) +
  facet_wrap(~site_id, scales = "free_y", ncol = 2) +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b") +
  labs(title = "Continuous Hourly Ozone Trends (Full Year)",
       y = "Ozone (ppm)", x = "Month") +
  theme_minimal()

ggsave("data_out/plots/task_b_full_year_continuous.png", plot_b, width = 12, height = 10)

# ============================================================
# Task (c): Seasonal Representative Weeks (Continuous Flow)
# ============================================================
message("Generating Task (c): Continuous Seasonal Weeks...")

seasons <- list(
  Winter = c("2023-01-15", "2023-01-21"),
  Spring = c("2023-04-15", "2023-04-21"),
  Summer = c("2023-07-15", "2023-07-21"),
  Fall   = c("2023-10-15", "2023-10-21")
)

# Using a standard for-loop for more reliable execution
for (s_name in names(seasons)) {
  dates <- seasons[[s_name]]
  
  week_data <- o3_data %>%
    filter(date_obj >= as.Date(dates[1]) & date_obj <= as.Date(dates[2]))
  
  if (nrow(week_data) > 0) {
    plot_c <- ggplot(week_data, aes(x = datetime, y = sample_measurement, color = site_id)) +
      geom_line(linewidth = 0.8) +
      facet_wrap(~site_id) +
      scale_x_datetime(date_breaks = "1 day", date_labels = "%a\n%m/%d") +
      labs(title = paste(s_name, "Ozone: Continuous Weekly Trend"),
           y = "Ozone (ppm)", x = "Day of Week") +
      theme_bw() +
      theme(legend.position = "none")
    
    ggsave(paste0("data_out/plots/task_c_", tolower(s_name), "_continuous.png"), 
           plot_c, width = 11, height = 8)
  }
}

# ============================================================
# Task (d): Chemistry - NO Titration Effect
# ============================================================
message("Generating Task (d): Chemistry Analysis...")

nox_data <- df_clean %>% 
  filter(parameter_code == "42603") %>%
  select(datetime, site_id, nox = sample_measurement)

chem_df <- inner_join(o3_data %>% select(datetime, site_id, o3 = sample_measurement), 
                      nox_data, by = c("datetime", "site_id"))

plot_d <- ggplot(chem_df, aes(x = nox, y = o3 * 1000)) + 
  geom_hex(bins = 40) +
  scale_fill_viridis_c(option = "magma", trans = "log10") +
  geom_smooth(method = "gam", color = "cyan", se = FALSE) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "Chemistry Analysis: NO Titration Effect",
       x = "NOx (ppb)", y = "Ozone (ppb)") +
  theme_dark()

ggsave("data_out/plots/task_d_chemistry_analysis.png", plot_d, width = 9, height = 7)

message("All tasks completed successfully!")