library(httr)
library(readr)

# Station (use 4-letter ICAO for hourly endpoint)
station <- "KCLE"

# Date range
start_date <- "2024-01-01"
end_date   <- "2024-12-31"

# Build URL for hourly ASOS request
base_url <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py"

resp <- GET(
  url = base_url,
  query = list(
    station = station,
    data = "all",
    year1 = substr(start_date,1,4),
    month1 = substr(start_date,6,7),
    day1 = substr(start_date,9,10),
    year2 = substr(end_date,1,4),
    month2 = substr(end_date,6,7),
    day2 = substr(end_date,9,10),
    tz = "UTC",
    format = "comma",
    latlon = "no",
    elev = "no",
    missing = "M",
    trace = "T",
    direct = "yes",
    report_type = "3"
  )
)

# Save file
writeBin(content(resp,"raw"), "KCLE_hourly_2024.csv")

cat("Download complete\n")

station <- c("KCLE","KBKL","KCGF")

for(st in station){

  resp <- GET(base_url,
    query=list(
      station=st,
      data="all",
      year1="2024", month1="01", day1="01",
      year2="2024", month2="12", day2="31",
      tz="UTC", format="comma",
      latlon="no", elev="no",
      missing="M", trace="T",
      direct="yes", report_type="3"
    ) 
  )

  writeBin(content(resp,"raw"),
           paste0(st,"_hourly_2024.csv"))

  cat("Saved:",st,"\n")
}
