# 加载必备的包
library(readr)
library(dplyr)

# 1. 定义一个通用的数据处理函数
process_station <- function(filename, prefix) {
  # 读取数据并将特定字母识别为空值
  data <- read_csv(filename, skip = 5, na = c("", "NA", "M", "T"), show_col_types = FALSE) %>%
    mutate(
      hour_time = substr(valid, 1, 13),   # 截取到小时 (YYYY-MM-DD HH)
      wd = as.numeric(drct),              # 提取风向
      ws = as.numeric(sknt) * 0.514444    # 提取风速并转换为 m/s
    ) %>%
    # 剔除缺失风速或风向的行
    filter(!is.na(wd), !is.na(ws)) %>%
    # 去重：如果同一小时有多条数据，只保留第一条
    distinct(hour_time, .keep_all = TRUE)
  
  # 【终极防拔毛写法】：不使用 select，直接使用底层中括号提取这三列
  data <- data[, c("hour_time", "wd", "ws")]
  
  # 给风向和风速重命名，加上站点的后缀（如 wd_CLE, ws_CLE）
  colnames(data)[2:3] <- c(paste0("wd_", prefix), paste0("ws_", prefix))
  
  return(data)
}

# 2. 分别处理三个站点
cle <- process_station("KCLE_hourly_2024.csv", "CLE")
bkl <- process_station("KBKL_hourly_2024.csv", "BKL")
cgf <- process_station("KCGF_hourly_2024.csv", "CGF")

# 3. 两两对比（inner_join 会完美保留两边的风速风向列）
cle_bkl_shared <- inner_join(cle, bkl, by = "hour_time")
cle_cgf_shared <- inner_join(cle, cgf, by = "hour_time")
bkl_cgf_shared <- inner_join(bkl, cgf, by = "hour_time")

# 4. 导出为【全新名字】的 CSV 文件，防止电脑缓存旧文件
write_csv(cle_bkl_shared, "FINAL_wind_CLE_BKL.csv")
write_csv(cle_cgf_shared, "FINAL_wind_CLE_CGF.csv")
write_csv(bkl_cgf_shared, "FINAL_wind_BKL_CGF.csv")

# 5. 在屏幕上打印前 5 行作为检查
cat("====================================================\n")
cat("✅ 运行成功！请去文件夹里寻找以 FINAL_wind 开头的新文件。\n\n")

cat(">> CLE 和 BKL 共有数据:", nrow(cle_bkl_shared), "条\n")
print(head(cle_bkl_shared, 5))

cat("\n>> CLE 和 CGF 共有数据:", nrow(cle_cgf_shared), "条\n")
print(head(cle_cgf_shared, 5))

cat("\n>> BKL 和 CGF 共有数据:", nrow(bkl_cgf_shared), "条\n")
print(head(bkl_cgf_shared, 5))
cat("====================================================\n")