# ============================================================
# DIAGNOSTIC: Testing 2023 Data with correct argument names
# ============================================================

library(RAQSAPI)

# EPA credentials
RAQSAPI::aqs_credentials(
  username = Sys.getenv("AQS_USER"),
  key = Sys.getenv("AQS_KEY")
)

# Test site: Cleveland (Cuyahoga: 035, Site: 0060)
# Parameters for your version: stateFIPS, countycode, sitenum
message("Checking 2023 data...")
check_2023 <- aqs_sampledata_by_site(
  parameter = "44201", 
  bdate = as.Date("2023-07-01"), 
  edate = as.Date("2023-07-01"),
  stateFIPS = "39", 
  countycode = "035", 
  sitenum = "0060"
)

if (nrow(check_2023) > 0) {
  cat("\nSuccess! 2023 data found. Total rows for one day: ", nrow(check_2023), "\n")
} else {
  cat("\nNo data found. Please check your network or site ID.\n")
}