# ============================================================
# Task 5: Spatial Autocorrelation Analysis (2024)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)    # 5A: 用于矩阵绘图
library(geosphere) # 5C: 用于大圆距离计算

# 1. 重新读取并准备原始数据 (确保 df 已加载)
df <- readRDS("data_out/cleveland_2024_data.rds")

df_clean <- df %>%
  filter(parameter_code == "44201") %>%
  mutate(full_site_id = paste("39", county_code, site_number, sep = "-"))

# 2. (5A & 5B) 生成 9x9 矩阵图并计算 r²
# 将数据转为宽格式，确保每个站点一列
wide_o3 <- df_clean %>%
  select(date_local, time_local, full_site_id, sample_measurement) %>%
  distinct(date_local, time_local, full_site_id, .keep_all = TRUE) %>%
  pivot_wider(names_from = full_site_id, values_from = sample_measurement) %>%
  select(-date_local, -time_local)

# 绘图：显示 A vs B 散点图及相关系数
# 注意：ggpairs 显示的是 r，其平方即为 r²
p_matrix <- ggpairs(wide_o3,
                    title = "Task 5A/B: 2024 Hourly Ozone Inter-site Comparison",
                    lower = list(continuous = wrap("points", alpha = 0.2, size = 0.3)),
                    upper = list(continuous = wrap("cor", size = 3))) +
  theme_bw() +
  theme(axis.text = element_text(size = 5), strip.text = element_text(size = 7))

ggsave("data_out/plots_2024/Task_5A_B_Correlation_Matrix.png", p_matrix, width = 15, height = 15)

# 3. (5C) 计算各站点对之间的距离 (单位：km)
site_coords <- df_clean %>%
  group_by(full_site_id) %>%
  summarise(lat = mean(latitude, na.rm = TRUE),
            lon = mean(longitude, na.rm = TRUE)) %>%
  ungroup()

# 计算大圆距离矩阵
dist_mat <- distm(site_coords[, c("lon", "lat")], fun = distHaversine) / 1000
rownames(dist_mat) <- site_coords$full_site_id
colnames(dist_mat) <- site_coords$full_site_id

# 4. 汇总数据：配对 R² 与 距离
cor_mat <- cor(wide_o3, use = "pairwise.complete.obs")
r2_mat <- cor_mat^2

# 将对称矩阵展开为平铺格式
site_pairs <- expand.grid(Site1 = rownames(r2_mat), Site2 = colnames(r2_mat)) %>%
  filter(as.character(Site1) < as.character(Site2)) %>% # 仅保留唯一的 A-B 对，不含自对比
  mutate(
    r_squared = mapply(function(s1, s2) r2_mat[s1, s2], Site1, Site2),
    distance_km = mapply(function(s1, s2) dist_mat[s1, s2], Site1, Site2)
  )

# 5. (5D) 绘制 R² 随距离变化的图
plot_5d <- ggplot(site_pairs, aes(x = distance_km, y = r_squared)) +
  geom_point(size = 4, color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") + # 最佳拟合线
  labs(title = "Task 5D: Ozone Spatial Autocorrelation (2024)",
       subtitle = "Coefficient of Determination (R²) vs. Distance between Site Pairs",
       x = "Distance (km)", y = expression(R^2)) +
  theme_minimal()

ggsave("data_out/plots_2024/Task_5D_Rsq_vs_Distance.png", plot_5d, width = 10, height = 7)

# 导出原始统计数值供 Capstone 论文查阅
write.csv(site_pairs, "data_out/site_comparison_stats_2024.csv", row.names = FALSE)