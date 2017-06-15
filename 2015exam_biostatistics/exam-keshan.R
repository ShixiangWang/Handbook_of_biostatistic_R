a <- read.table("Keshan_disease.txt",header=T)
summary(a)
#框定访问对象位于a中
attach(a)
sd(patient)
sd(healthy)
var(patient)
var(healthy)
b <-max(patient)-min(patient)
c <-max(healthy)-min(healthy)
boxplot(a)
#t-test假设检验之前进行方差同质性分析(var.test)，决定t-test的var.equal参数
var.test(patient,healthy)$p.value
t.test(patient,healthy,var.equal = T)$p.value
#非参数检验wilcox 配对状态T/F，非配对更通用
wilcox.test(patient,healthy,paired = T)
wilcox.test(patient,healthy,paired = F)

detach(a)

