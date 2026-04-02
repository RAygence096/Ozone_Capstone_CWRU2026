# ============================================================
# CAPSTONE: Cleveland 2023 O3 & Precursors (Certified Data)
# ============================================================

library(RAQSAPI)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(GGally)

# EPA credentials
RAQSAPI::aqs_credentials(
  username = Sys.getenv("AQS_USER"),
  key = Sys.getenv("AQS_KEY")
)

# Configuration
cbsa_code <- "17460" # Cleveland-Elyria-Mentor
params <- c("44201", "42602", "42601", "42603") # O3, NO2, NO, NOx

# 1. Identify Monitors for 2023
message("Step 1: Finding 2023 monitoring sites...")
monitors <- aqs_monitors_by_cbsa(
  parameter = "44201", 
  bdate = as.Date("2023-01-01"), 
  edate = as.Date("2023-12-31"), 
  cbsa_code = cbsa_code
)

site_list <- monitors %>% 
  select(state_code, county_code, site_number) %>% 
  distinct()

# 2. Monthly Loop (Download 12 months)
months_2023 <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "month")

message("Step 2: Downloading hourly data (this may take 5-10 minutes)...")
all_data_2023 <- map_dfr(params, function(p) {
  message("\nProcessing Parameter: ", p)
  
  map_dfr(months_2023, function(m_start) {
    m_end <- rollback(m_start + months(1))
    
    map_dfr(1:nrow(site_list), function(i) {
      tryCatch({
        dat <- aqs_sampledata_by_site(
          parameter = p,
          bdate = m_start,
          edate = m_end,
          stateFIPS = site_list$state_code[i],
          countycode = site_list$county_code[i],
          sitenum = site_list$site_number[i]
        )
        if(!is.null(dat)) cat(".") # Progress indicator
        return(dat)
      }, error = function(e) return(NULL))
    })
  })
})

# 3. Clean and Save
if (nrow(all_data_2023) > 0) {
  df <- all_data_2023 %>%
    mutate(
      datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"),
      site_id = paste(county_code, site_number, sep = "-")
    ) %>%
    filter(sample_measurement >= 0)
  
  dir.create("data_out", showWarnings = FALSE)
  saveRDS(df, "data_out/cleveland_2023_data.rds")
  message("\nSuccess! Data saved to data_out/cleveland_2023_data.rds")
  message("Total rows: ", nrow(df))
} else {
  stop("\nCritical Error: No data retrieved during full download.")
}