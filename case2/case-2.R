library(ggplot2)
library(dplyr)

# 严格顺序（包含大标题和小组）
subgroup_order <- c(
  "Age (years)",
  "Age<70 (n=1915)", "Age≥70 (n=1278)",
  "Sex",
  "Male (n=1677)", "Famale (n=1516)",
  "AKI stage",
  "Stage 1 (n=2243)", "Stage 2 or 3 (n=950)",
  "Departments",
  "Surgical (n=1732)", "Non-surgical (n=1461)",
  "Past medical history",
  "Cancer (n=1006)", "No cancer (n=2187)",
  "Time-period",
  "First quarter (n=835)", "Second quarter (n=802)",
  "Third quarter (n=699)", "Fourth quarter (n=857)"
)

# 构建数据
forest_data <- data.frame(
  Subgroups = subgroup_order,
  HR = c(NA, 1.66, 1.56,
         NA, 1.48, 1.87,
         NA, 1.63, 1.70,
         NA, 1.40, 1.99,
         NA, 1.30, 1.83,
         NA, 1.44, 1.74, 1.69, 1.88),
  LowerCI = c(NA, 1.46, 1.36,
              NA, 1.30, 1.62,
              NA, 1.46, 1.41,
              NA, 1.23, 1.72,
              NA, 1.10, 1.63,
              NA, 1.19, 1.44, 1.38, 1.54),
  UpperCI = c(NA, 1.90, 1.80,
              NA, 1.68, 2.17,
              NA, 1.83, 2.05,
              NA, 1.59, 2.31,
              NA, 1.53, 2.07,
              NA, 1.73, 2.12, 2.08, 2.29),
  Group = c(
    "Age (years)", "Age (years)", "Age (years)",
    "Sex", "Sex", "Sex",
    "AKI stage", "AKI stage", "AKI stage",
    "Departments", "Departments", "Departments",
    "Past medical history", "Past medical history", "Past medical history",
    "Time-period", "Time-period", "Time-period", "Time-period", "Time-period"
  ),
  stringsAsFactors = FALSE
)

# 标记大标题
forest_data$is_group <- is.na(forest_data$HR)

# HR [95% CI] 标签
forest_data <- forest_data %>%
  mutate(HR_CI = ifelse(is.na(HR), "", sprintf("%.2f [%.2f–%.2f]", HR, LowerCI, UpperCI)))

# y轴顺序，保证严格按照 subgroup_order
forest_data$ypos <- seq(length(forest_data$Subgroups), 1)

# 颜色映射
group_colors <- c(
  "Age (years)" = "#FF6B6B",
  "Sex" = "#4ECDC4",
  "AKI stage" = "#FFD166",
  "Departments" = "#6A0572",
  "Past medical history" = "#1A936F",
  "Time-period" = "#118AB2"
)

# 绘图
forest_plot <- ggplot(forest_data, aes(x = HR, y = ypos)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  
  # 小组的置信区间和点
  geom_errorbarh(data = filter(forest_data, !is_group),
                 aes(xmin = LowerCI, xmax = UpperCI, color = Group),
                 height = 0.2, linewidth = 0.8) +
  geom_point(data = filter(forest_data, !is_group),
             aes(color = Group), size = 3, shape = 15) +
  
  # 左侧显示大标题和小组
  geom_text(aes(x = -0.9, label = Subgroups),  # 左侧在绘图外显示
            hjust = 0, size = 3.5,
            fontface = ifelse(forest_data$is_group, "bold", "plain")) +
  
  # 右侧显示 HR [95% CI]
  geom_text(aes(x = 2.55, label = HR_CI), hjust = 0, size = 3.5) +
  
  scale_x_continuous(limits = c(-1, 3.0),  # 绘图区域从 -1 到 2.5
                     breaks = seq(0, 2.5, 0.5)) +  # 显示刻度 0~2.5
  scale_y_continuous(breaks = forest_data$ypos, labels = NULL) +
  scale_color_manual(values = group_colors) +
  
  labs(title = "Forest Plot of Hazard Ratios by Subgroups",
       x = "Hazard Ratio (HR) with 95% CI", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right"
  )

# 显示图
forest_plot

# 输出文件夹
output_dir <- "results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 保存 PNG，背景白色
ggsave(filename = file.path(output_dir, "forest_plot.png"),
       plot = forest_plot, width = 6, height = 8, dpi = 300, bg = "white")

# 显示保存路径信息
cat("所有图片已保存到以下路径：\n")
cat("工作目录：", getwd(), "\n")
cat("结果文件夹：", file.path(getwd(), output_dir), "\n")
cat("包含文件：\n")
print(list.files(output_dir))

# Windows 系统自动打开结果文件夹
if (.Platform$OS.type == "windows") {
  shell.exec(output_dir)
}


# 输出文件夹
output_dir <- "results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 保存 PNG，背景白色
ggsave(file.path(output_dir, "forest_plot.png"),
       plot = forest_plot, width = 10, height = 6, dpi = 300, bg = "white")

# 显示路径信息
cat("所有图片已保存到以下路径：\n")
cat("工作目录：", getwd(), "\n")
cat("结果文件夹：", file.path(getwd(), output_dir), "\n")
cat("包含文件：\n")
print(list.files(output_dir))

# Windows 系统自动打开结果文件夹
if (.Platform$OS.type == "windows") {
  shell.exec(output_dir)
}
