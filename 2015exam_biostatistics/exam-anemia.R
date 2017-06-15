a <- read.table("anemia.txt", header=T)
#画散点图
plot(a$reticulyte,a$lymphocyte)
#画拟合直线图
fit <- lm(a$lymphocyte~a$reticulyte)
#添加直线图到散点图
abline(fit)
#得到回归方程的参数
summary(fit)
#y=183.65+511.49x
#自变量的95%置信区间
confint(fit,"a$reticulyte")
#自变量因变量的相关系数
cor(a$reticulyte,a$lymphocyte)
