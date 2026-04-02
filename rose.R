# ==========================================
# KCLE 气象站风向玫瑰图绘制程序
# ==========================================

# 1. 加载必需的扩展包
library(readr)     # 用于快速、规范地读取 CSV 文件
library(dplyr)     # 用于数据清洗、筛选和修改
library(lubridate) # 用于处理日期和时间格式
library(openair)   # 用于绘制专业的环境/气象图表

# 2. 读取与数据清洗
kcle_data <- read_csv("KBKL_hourly_2024.csv", skip = 5, show_col_types = FALSE) %>%
  mutate(
    # 将字符型的时间转换为标准的时间格式，并重命名为 openair 要求的 'date'
    date = ymd_hm(valid),
    
    # 提取风向并强制转为数值型，重命名为 'wd'
    wd = as.numeric(drct),
    
    # 提取风速，并将单位从节(knots)转换为米/秒(m/s)，重命名为 'ws'
    ws = as.numeric(sknt) * 0.514444
  ) %>%
  # 剔除没有风速或风向记录的缺失数据（NA）
  filter(!is.na(ws), !is.na(wd))

# 3. 绘制风向玫瑰图
windRose(
  mydata = kcle_data, 
  ws = "ws",                          # 指定风速列
  wd = "wd",                          # 指定风向列
  angle = 22.5,                       # 设置 16 个风向区间（360/16 = 22.5度）
  breaks = c(0, 1, 3, 5, 8, 12, 20),  # 设置之前讨论好的风速区间 (m/s)
  paddle = FALSE,                     # 使用传统的披萨切片(扇形)样式
  annotate = TRUE,                    # 在中心标注静风 (Calm) 比例
  key.position = "right",             # 图例放在右侧
  main = "Cleveland (KCLE) Annual Wind Rose - 2024" # 标题
)