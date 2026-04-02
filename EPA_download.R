# ==========================================
# EPA AQS Data Downloader for Cleveland Capstone
# ==========================================

library(RAQSAPI)
library(purrr)
library(dplyr)
library(readr)
library(lubridate)

# EPA credentials
RAQSAPI::aqs_credentials(
  username = Sys.getenv("AQS_USER"),
  key = Sys.getenv("AQS_KEY")
)
# 2. 定义参数
cbsa_code <- "17460" # Cleveland-Elyria-Mentor 的代码
year_2022 <- 2022
# 任务所需的参数代码：O3(44201), NO2(42602), NO(42601), NOx(42603), VOCs(各种)
param_codes <- c("44201", "42602", "42601", "42603") 

# --- 任务 1: 自动识别该地区测量 O3 的所有站点 ---
message("正在获取 Cleveland 地区的 O3 监测站点...")
cle_sites <- aqs_monitors_by_cbsa(
  parameter = "44201",
  bdate = as.Date("2022-01-01"),
  edate = as.Date("2022-12-31"),
  cbsa_code = cbsa_code
)

# 提取唯一的站点标识符
site_list <- cle_sites %>%
  select(state_code, county_code, site_number) %>%
  distinct()

# --- 任务 2 & 3: 循环下载 2022 年全年的小时数据 ---
# 注意：API 限制一次不能请求太多数据，我们按参数进行循环
download_epa_data <- function(p_code) {
  message(paste("正在下载参数代码:", p_code, "的数据..."))
  
  # 对每个站点进行抓取
  map_dfr(1:nrow(site_list), function(i) {
    tryCatch({
      aqs_sampledata_by_site(
        parameter = p_code,
        bdate = as.Date("2022-01-01"),
        edate = as.Date("2022-12-31"),
        state_code = site_list$state_code[i],
        county_code = site_list$county_code[i],
        site_number = site_list$site_number[i]
      )
    }, error = function(e) return(NULL)) # 如果某个站点没有该参数，跳过
  })
}

# 执行下载 (这可能需要几分钟)
all_data_2022 <- map_dfr(param_codes, download_epa_data)

# --- 数据保存与清理 ---
if (nrow(all_data_2022) > 0) {
  # 转换为当地时间
  all_data_2022 <- all_data_2022 %>%
    mutate(datetime = as.POSIXct(paste(date_local, time_local), tz = "America/New_York"))
  
  # 保存为本地文件
  dir.create("data_out", showWarnings = FALSE)
  saveRDS(all_data_2022, "data_out/cleveland_2022_hourly_data.rds")
  write_csv(all_data_2022, "data_out/cleveland_2022_hourly_data.csv")
  
  message("下载完成！数据已保存至 data_out 文件夹。")
} else {
  warning("未获取到数据，请检查 API Key 或网络连接。")
}