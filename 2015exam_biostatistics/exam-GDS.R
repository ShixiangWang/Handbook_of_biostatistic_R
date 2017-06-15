#导入数据
data <- read.table("GDS4013.txt", header = T)
#将数据标准化
data <- scale(data)
#检测NULL值
any(is.na(data))
data <- na.omit(data)

#计算每组方差同质性检验P-value
pval <- apply(data,1,function(x){return(var.test(x[1:10],x[11:18])$p.value)})
#单边检验两组差异表达基因P-value,同质T，不同质F，两种算出P-value
tpval <- sapply(1:length(pval),function(i){
  if(pval[i]<=0.05){
    tmp<-t.test(data[i,1:10],data[i,11:18],var.equal=F,alternative="less")$p.value}
  else{
    tmp<-t.test(data[i,1:10],data[i,11:18],var.equal=T,alternative="less")$p.value}
  return(tmp)
})
#找出有差异表达基因的具体名字
names(tpval) <- rownames(data)
tpval[tpval<0.05]
length(tpval[tpval<0.05])
sort(tpval, decreasing = F)[1:20]
# FDR矫正 去除假阳性
ad_tpval <- p.adjust(tpval,method="fdr")
length(ad_tpval[ad_tpval<0.05])
ad_tpval[ad_tpval<0.05]

# 导入正常组织的甲基化数据
methy <- read.table("methylation.txt")

nonsig_gene.name <- names(tpval[tpval>0.05])
sig_gene.name    <- names(tpval[tpval<=0.05])
set.seed(1234)
nonsig_gene      <- sample(nonsig_gene.name,200)
sig_gene         <- sample(sig_gene.name,200)

# 抽提相应数据
non.sig.data     <- methy[methy$V1 %in% nonsig_gene,2]
sig.data         <- methy[methy$V1 %in% sig_gene,2]

# 检验
var.test(non.sig.data, sig.data)
t.test(non.sig.data, sig.data, var.equal = T)

# power计算
# install.packages("pwr")
library(pwr)
# 关键是计算效应值d(这里是独立样本的效应值计算)
# 计算公式参考：https://en.wikipedia.org/wiki/Effect_size
# 这里样本数一样，总方差可以写为两个样本方差平均值
var1 <- var(non.sig.data)
var2 <- var(sig.data)
delta_all <- sqrt((var1+var2)/2)
d <- abs(mean(non.sig.data)-mean(sig.data))/delta_all
d
power <- pwr.t.test(n=200, d=d, sig.level = .05, 
                    type = "two.sample", alternative = "two.sided")
power


# PCA
# color <- rep(c("blue", "red"), c(10,8))
# pca   <- prcomp(t(data))
# summary(pca)
# compo1 <- pca$x[,1]
# compo2 <- pca$x[,2]
# plot(compo1, compo2, col=color)


