# ============================================================
# UTILITY: Convert RDS to CSV for Inspection
# ============================================================

# 设置工作目录 (确保这是你项目所在的文件夹)
setwd("/Users/raygence/Documents/CWRU/Ozone_Capstone")

# 1. 定义你要转换的文件列表
files_to_convert <- c(
  "data_out/cleveland_2024_voc_sttheo.rds"
)

# 2. 循环读取并保存为 CSV
for (file_path in files_to_convert) {
  
  if (file.exists(file_path)) {
    message("正在读取: ", file_path)
    
    # 读取 RDS
    data <- readRDS(file_path)
    
    # 生成新的 CSV 文件名 (把 .rds 换成 .csv)
    new_csv_path <- sub(".rds", ".csv", file_path)
    
    # 写入 CSV (row.names = FALSE 防止生成多余的序号列)
    write.csv(data, new_csv_path, row.names = FALSE)
    
    message("✅ 成功生成: ", new_csv_path)
    
  } else {
    message("⚠️ 跳过: 找不到文件 ", file_path)
  }
}

message("\n转换完成！请去 data_out 文件夹用 Excel 查看这些 .csv 文件。")