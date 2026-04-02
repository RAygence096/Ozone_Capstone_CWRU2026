# ============================================================
# CAPSTONE: 2024 Cleveland-Elyria-Mentor O3 Data Retrieval
# ============================================================

library(RAQSAPI)
library(dplyr)
library(purrr)
library(lubridate)

# EPA credentials
RAQSAPI::aqs_credentials(
  username = Sys.getenv("AQS_USER"),
  key = Sys.getenv("AQS_KEY")
)

# CBSA 17460 for Cleveland-Elyria-Mentor, OH
cbsa_code <- "17460"

# Target parameters: O3, NO2, NO, NOx
param_list <- c("44201", "42602", "42601", "42603")

# 1. Identify all O3-measuring sites in the CBSA (Task 1)
message("Identifying O3-measuring sites...")
o3_monitors <- aqs_monitors_by_cbsa(
  parameter = "44201",
  bdate = as.Date("2024-01-01"),
  edate = as.Date("2024-12-31"),
  cbsa_code = cbsa_code
)

# Extract unique site identifiers
site_list <- o3_monitors %>%
  select(state_code, county_code, site_number) %>%
  distinct()

# 2. Define monthly intervals to prevent API timeout
months_2024 <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")

# 3. Execution of data download (Task 2 & 3)
message("Starting 2024 data download...")
all_2024_data <- map_dfr(param_list, function(p) {
  message("\nProcessing Parameter: ", p)
  
  map_dfr(months_2024, function(m_start) {
    m_end <- rollback(m_start + months(1))
    
    map_dfr(1:nrow(site_list), function(i) {
      tryCatch({
        # Use exact parameter names for current RAQSAPI version
        dat <- aqs_sampledata_by_site(
          parameter = p,
          bdate = m_start,
          edate = m_end,
          stateFIPS = site_list$state_code[i],
          countycode = site_list$county_code[i],
          sitenum = site_list$site_number[i]
        )
        if(!is.null(dat)) cat(".")
        return(dat)
      }, error = function(e) return(NULL))
    })
  })
})

# 4. Save results (Fixed variable name)
if (exists("all_2024_data") && nrow(all_2024_data) > 0) {
  dir.create("data_out", showWarnings = FALSE)
  saveRDS(all_2024_data, "data_out/cleveland_2024_data.rds")
  message("\nSuccess! 2024 data saved to data_out/cleveland_2024_data.rds")
} else {
  stop("\nDownload failed or dataset is empty.")
}