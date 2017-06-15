a <- read.table("lung_cancer.txt",header = T)
#将整数型hospital转换成factor
a$hospital <- factor(a$hospital)
#anova分析是否有显著性差异
b <-aov(exp_A~hospital,data=a)
#查看参数
summary(b)
#p adj即是p-value,检测具体哪两组之间差异性显著
TukeyHSD(b)
#调取一号医院的数据，两种方法
alpha <- a[a$hospital==1,]
alpha1 <- subset(a,hospital==1)
#logistic函数计算回归模型
fit <- glm(status~exp_A,data=a,family = binomial())
summary(fit)
#计算最后拟合函数的相关系数
coef(fit)
#odds=e^(-1.8693509+0.0294x)