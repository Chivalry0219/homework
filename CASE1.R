# 加载必要包
library(dplyr)
library(tidyr)
library(ggplot2)

#构建完整数据框
data_complete <- data.frame(
  id = 1:6,
  wt_kg = c(76, 74, 54, 58, 94, 82),
  dose_mg = c(200, 200, 150, 150, 250, 225),
  "1h" = c(18.6, 19.3, 19.3, 18.9, 19.5, 18.7),
  "2h" = c(15.6, 15.8, 14.5, 14.6, 14.7, 14.9),
  "4h" = c(12.3, 11.5, 12.5, 12.7, 12.3, 12.3),
  "8h" = c(10.1, 9.8, 10.3, 10.3, 10.7, 10.3),
  "12h" = c(7.6, 6.5, 6.9, 7.5, 6.9, 7.9),
  "24h" = c(3.2, 2.1, 3.5, 3.3, 4.1, 3.5)
)

# 转成长格式用于绘图
data_long <- data_complete %>%
  pivot_longer(
    cols = c("X1h", "X2h", "X4h", "X8h", "X12h", "X24h"),
    names_to = "Time",
    values_to = "Concentration"
  ) %>%
  mutate(
    Time = as.numeric(gsub("X", "", gsub("h", "", Time))),
    # 创建自定义标签用于分面标题
    subject_label = paste0("ID#", id, "\nWT: ", wt_kg, "kg, Dose: ", dose_mg, "mg")
  )

# 创建输出文件夹
output_dir <- "result"
if(!dir.exists(output_dir)) dir.create(output_dir)

# 绘制血药浓度-时间曲线（使用对数坐标，只显示散点）
pl_points <- ggplot(data_long, aes(x = Time, y = Concentration)) +
  # 只保留散点，取消连线
  geom_point(color = "darkred", size = 4, shape = 16, alpha = 0.8) +
  # 设置横坐标：0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25，等间距
  scale_x_continuous("Time (h)", 
                     breaks = c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25),
                     labels = c("0", "", "5", "", "10", "", "15", "", "20", "", "25"),
                     limits = c(0, 25)) +
  # 设置纵坐标：1,10,100，对数刻度，确保显示标签
  scale_y_log10("Concentration (mg/L)",
                breaks = 10^(seq(0, 2, by = 0.5)),  # 5个刻度：1, 3.16, 10, 31.6, 100
                labels = c("1", "", "10", "", "100"),  # 只显示1,10,100的标签
                limits = c(1, 100))+
  theme_bw(base_size = 14) +
  # 使用自定义标签进行分面
  facet_wrap(~subject_label, ncol = 3) +
  # 设置分面标题样式
  theme(
    strip.background = element_rect(fill = "orange", color = "black"),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    # 确保坐标轴标签显示
    axis.text.y = element_text(size = 12)
  )

print(pl_points)

# 保存图片
jpeg(filename = file.path(output_dir, "figure_conc_time_log_scale.jpg"),
     width = 3200, height = 2200, res = 300)
print(pl_points)
dev.off()

# 显示保存路径信息
cat("所有图片已保存到以下路径：\n")
cat("工作目录：", getwd(), "\n")
cat("结果文件夹：", file.path(getwd(), output_dir), "\n")
cat("包含文件：\n")
list.files(output_dir)

# 在Windows系统中自动打开结果文件夹
if (.Platform$OS.type == "windows") {
  shell.exec(output_dir)
}