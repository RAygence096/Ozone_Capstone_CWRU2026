# Check each month of 2024 for one specific site
library(RAQSAPI)
RAQSAPI::aqs_credentials(username = Sys.getenv("AQS_USER"), key = Sys.getenv("AQS_KEY"))

# Test Cuyahoga Site 0060 (a very active site)
test_months <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")

message("Checking 2024 data availability by month...")
availability <- purrr::map_dfr(test_months, function(m) {
  dat <- aqs_sampledata_by_site(
    parameter = "44201", bdate = m, edate = m + 1, # Testing 2 days per month
    stateFIPS = "39", countycode = "035", sitenum = "0060"
  )
  data.frame(Month = format(m, "%Y-%m"), Rows = nrow(dat))
})

print(availability)