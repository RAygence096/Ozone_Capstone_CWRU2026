# ============================================================
# CAPSTONE: Site 0060 Multi-Pollutant Continuous Comparison
# Target: O3, NOx, SO2, and VOCs (2024)
# ============================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Set working directory
setwd("/Users/raygence/Documents/CWRU/Ozone_Capstone")

# 1. Load Datasets
message("Loading pollutant datasets for site 0060...")
o3_raw <- readRDS("data_out/cleveland_2024_data.rds")
others_raw <- readRDS("data_out/site_0060_pollutants_2024.rds")

# 2. Process Ozone Data (Site 035-0060)
o3_clean <- o3_raw %>%
  mutate(site_id = paste(county_code, site_number, sep = "-")) %>%
  filter(site_id == "035-0060" & parameter_code == "44201") %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
    # Standardize to ppb (ppm * 1000) for comparison with other pollutants
    value = sample_measurement * 1000,
    pollutant = "Ozone (O3)"
  ) %>%
  select(datetime, pollutant, value)

# 3. Process NOx, SO2, and VOC Data
others_clean <- others_raw %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
    value = sample_measurement,
    pollutant = case_when(
      parameter_code == "42603" ~ "Nitrogen Oxides (NOx)",
      parameter_code == "42401" ~ "Sulfur Dioxide (SO2)",
      parameter_code == "43101" ~ "VOCs (TNMOC)",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(datetime, pollutant, value)

# 4. Combine and Remove Duplicates
full_comparison <- bind_rows(o3_clean, others_clean) %>%
  group_by(datetime, pollutant) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# Create output folder
dir.create("data_out/pollutant_comparison", showWarnings = FALSE)

# ============================================================
# Plot: Representative Summer Week (Continuous Hourly)
# ============================================================
message("Generating continuous hourly comparison plot...")

# Focus on a peak summer week for photochemical analysis
summer_range <- full_comparison %>%
  filter(datetime >= "2024-07-14" & datetime <= "2024-07-21")

plot_continuous <- ggplot(summer_range, aes(x = datetime, y = value, color = pollutant)) +
  # Continuous line without daily grouping to avoid vertical snaps
  geom_line(linewidth = 0.8) +
  # Facet by pollutant with independent Y-axes for better resolution
  facet_wrap(~pollutant, ncol = 1, scales = "free_y") +
  # Format X-axis for a continuous timeline
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a\n%m/%d") +
  labs(title = "Site 0060: Hourly Continuous Multi-Pollutant Comparison (2024)",
       subtitle = "Analysis of Ozone (O3) and its chemical precursors (NOx, VOCs)",
       y = "Concentration (ppb)", x = "Timeline") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "gray95"),
        strip.text = element_text(face = "bold"))

ggsave("data_out/pollutant_comparison/site_0060_continuous_hourly.png", 
       plot_continuous, width = 12, height = 12, dpi = 300)

message("Success! Your continuous multi-pollutant chart is ready.")