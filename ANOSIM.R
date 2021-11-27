library(vegan)
library(ggplot2)
otu=read.csv("SC1.csv", sep=",", header=T)
#otu <- na.omit(otu)
#head(iris)
#iris.dist <- vegdist(subset(iris, select = -Species))
otu.dist <- vegdist(subset(otu, select = -group))
m <- monoMDS(otu.dist)

#m <- monoMDS(iris.dist)
#m
plot(m$points)
dat <- as.data.frame(m$points)
#dat$gr <- iris$Species
dat$gr <- otu$group
p <- ggplot(dat, aes(MDS1, MDS2, col = gr, shape = gr)) +
   geom_point() +
   geom_jitter(width = 0.0025, height = 0.0025, size = 3)+
   theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), legend.title = element_blank())+
   theme_bw() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p


#anosim(dat,grouping, permutations = 999, distance = "bray", …)
#dat           可以是相似性矩阵，也可以是row为sample，columns为variable的dataframe
#grouing  分组信息
#permutations  置换检验的次数，默认是999次
#distance  如果dat是dataframe，计算相似性矩阵的方法，默认是bray Curtis
#iris.dist <- vegdist(subset(iris, select = -Species)) #iris 取子集，species， 用函数vegdist 计算distance
otu.dist <- vegdist(subset(otu, select = -group)) #otu 取子集，species， 用函数vegdist 计算distance
#iris.ano <- anosim(iris.dist, iris$Species, permutation = 999) #同上
otu.ano <- anosim(otu.dist, otu$group, permutation = 999) #同上
#iris.ano
otu.ano
plot(otu.ano,col = c("#00AFBB", "#E7B800", "#FC4E07"))
#plot(iris.ano)

###############################################################
