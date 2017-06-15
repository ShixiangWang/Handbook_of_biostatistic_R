tpval<-sapply(1:length(pval),function(i){
if(pval[i]<=0.05){
tmp<-t.test(data[i,1:10],data[i,11:18],var.equal=F,alternative="less")$p.value}
else{
tmp<-t.test(data[i,1:10],data[i,11:18],var.equal=T,alternative="less")}
return(tmp)
})