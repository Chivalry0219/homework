# 加载必要包
library(ggplot2)
library(ggpmisc)   # 添加回归方程和R²
library(gridExtra) # 合并多图

# 构建数据框
df <- data.frame(
  subject = 1:6,
  WT = c(75, 68, 65, 98, 56, 76),
  sex = c("F", "F", "F", "M", "M", "M"),
  DOSE = c(200, 175, 175, 250, 150, 200),
  CLCR = c(102, 34, 21, 54, 65, 76),
  kel = c(0.38, 0.13, 0.10, 0.28, 0.32, 0.36),
  V = c(15.2, 13.2, 13.1, 19.4, 11.2, 15.5),
  CL = c(5.776, 1.716, 1.31, 5.432, 3.584, 5.58)
)

# 公共回归公式
formula <- y ~ x

# 回归方程显示格式，保留两位小数
eq <- function(x,y) {
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep="~~~")),
    formula = formula, parse=TRUE, 
    label.x.npc = "right", label.y.npc = 0.95,
    rr.digits = 2, eq.digits = 2, color="red"
  )
}

# 自定义主题：只保留左和下坐标轴，坐标轴加粗
my_theme <- theme_bw(base_size = 14) +
  theme(
    axis.line.x = element_line(size=1.2, color="black"),
    axis.line.y = element_line(size=1.2, color="black"),
    axis.title = element_text(face="bold"),
    panel.border = element_blank(),       # 去掉默认边框
    panel.grid = element_blank()          # 去掉网格
  )

# 图1：CLCR vs CL
p1 <- ggplot(df, aes(x = CLCR, y = CL)) +
  geom_point(color = "orange", size=3) +
  geom_smooth(method="lm", se=FALSE, linetype="dashed", color="steelblue") +
  eq(df$CLCR, df$CL) +
  my_theme +
  labs(x="CLCR (mL/min)", y="CL (L/h)", title="A) CL-CLCR")

# 图2：CLCR vs V
p2 <- ggplot(df, aes(x = CLCR, y = V)) +
  geom_point(color = "orange", size=3) +
  geom_smooth(method="lm", se=FALSE, linetype="dashed", color="steelblue") +
  eq(df$CLCR, df$V) +
  my_theme +
  labs(x="CLCR (mL/min)", y="V (L)", title="B) V-CLCR")

# 图3：WT vs CL
p3 <- ggplot(df, aes(x = WT, y = CL)) +
  geom_point(color = "orange", size=3) +
  geom_smooth(method="lm", se=FALSE, linetype="dashed", color="steelblue") +
  eq(df$WT, df$CL) +
  my_theme +
  labs(x="WT (kg)", y="CL (L/h)", title="C) CL-WT")

# 图4：WT vs V
p4 <- ggplot(df, aes(x = WT, y = V)) +
  geom_point(color = "orange", size=3) +
  geom_smooth(method="lm", se=FALSE, linetype="dashed", color="steelblue") +
  eq(df$WT, df$V) +
  my_theme +
  labs(x="WT (kg)", y="V (L)", title="D) V-WT")

# 合并四个图（2x2 网格）
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

# 创建输出文件夹
output_dir <- "result"
if(!dir.exists(output_dir)) dir.create(output_dir)

# 保存图片
jpeg(filename = file.path(output_dir, "CL_WT_Plots.jpg"),
     width = 4000, height = 3000, res = 300)
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)
dev.off()

# 显示保存路径信息
cat("所有图片已保存到以下路径：\n")
cat("工作目录：", getwd(), "\n")
cat("结果文件夹：", file.path(getwd(), output_dir), "\n")
cat("包含文件：\n")
list.files(output_dir)

# Windows系统自动打开结果文件夹
if (.Platform$OS.type == "windows") {
  shell.exec(output_dir)
}
