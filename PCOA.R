
#if (FALSE){
#  source("https://bioconductor.org/biocLite.R")
#  biocLite(c("ggplot2","vegan"))
#}
# 加载相关软件包
library("ggplot2")
library("vegan")

design = read.table("map.txt", header=T, row.names= 1, sep="\t") 

Bray_curtis = read.csv("bray.csv", sep=",", header=T, row.names = 1, check.names=F)

idx = rownames(design) %in% colnames(Bray_curtis) 
sub_design = design[idx,]
Bray_curtis = Bray_curtis[rownames(sub_design), rownames(sub_design)] # subset and reorder distance matrix 按照sub_design的rownames 的顺序，形成bray_curtis 矩阵


pcoa = cmdscale(Bray_curtis, k=3, eig=T) # k is dimension, 3 is recommended; eig is eigenvalues(特征值)


write.table(pcoa$points,"points.txt", sep=",")


points = as.data.frame(pcoa$points) #将pcoa数据集里面的points 挑选出来，作为数据框赋值给points
colnames(points) = c("x", "y", "z") #给points数据集里面每列加标题
eig = pcoa$eig
points = cbind(points, sub_design[match(rownames(points), rownames(design)), ])


p = ggplot(points, aes(x=x, y=y, color=genotype)) +
  geom_point(alpha=.7, size=2) + 
  stat_ellipse(level = 0.6) +
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="Bray_curtis PCoA") +
       geom_jitter(width = 0.00035, height = 0.00035, size = 2) +
       theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  
p
ggsave("beta_pcoa_weight_unifrac.pdf", p, width = 5, height = 3)
ggsave("beta_pcoa_weight_unifrac.png", p, width = 5, height = 3)