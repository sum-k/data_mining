## Install packages

install.packages("scatterplot3d")
install.packages("rgl")           


## Reading data
library(dplyr)
diabetes = read.table("diabetes.txt")
head(diabetes)

d <- diabetes %>% select(-c(V1, V7, V8))
colnames(d) <- c("RW", "FPG", "AUPGC", "AUPIC", "SSPG")
head(d)

## mean, sd

apply(d, 2, mean)
apply(d, 2, sd)


## standization

d2 = as.data.frame(scale(d))
head(d2)

attach(d2)


## Hierarchical clustering

h.clust = hclust(dist(d2), method="complete")

h.clust2 = hclust(dist(d2), method="single")

h.clust3 = hclust(dist(d2), method="average")

par(mfrow = c(1,3))
plot(h.clust, main = "complete linkage")
plot(h.clust2, main = "single linkage")
plot(h.clust3, main = "average linkage")

## cut dendrogram

h.cut = cutree(h.clust3, k=3)
h.cut

h.seg1 = d2[h.cut==1,]
h.seg2 = d2[h.cut==2,]
h.seg3 = d2[h.cut==3,]

## showing the results

pie(table(h.cut),main="number of observations in segment")
h.mean = rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean),apply(h.seg3,2,mean))
rownames(h.mean) = c(1,2,3)
h.mean
dist(h.mean, method = "euclidean", diag = TRUE)

## Boxplot

par(mfrow=c(3,2))
boxplot(h.seg1[,1],h.seg2[,1],h.seg3[,1],ylab=names(h.seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(h.seg1[,2],h.seg2[,2],h.seg3[,2],ylab=names(h.seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(h.seg1[,3],h.seg2[,3],h.seg3[,3],ylab=names(h.seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(h.seg1[,4],h.seg2[,4],h.seg3[,4],ylab=names(h.seg1)[4],xlab="segment",col="blue",names=c(1,2,3))
boxplot(h.seg1[,5],h.seg2[,5],h.seg3[,5],ylab=names(h.seg1)[5],xlab="segment",col="blue",names=c(1,2,3))


## 3D plot
library(scatterplot3d)
library(rgl)

scatterplot3d(FPG, AUPIC, SSPG, color=h.cut)
plot3d(FPG, AUPIC, SSPG, col=h.cut)
scatterplot3d(AUPGC, AUPIC, SSPG, color=h.cut)
plot3d(AUPGC, AUPIC, SSPG, col=h.cut)


## K-means clustering

k.clust = kmeans(d2, centers=3, nstart=20)
k.clust2 = kmeans(d2, centers=4, nstart=20)


## showing the results

pie(k.clust$size, main="number of observations in segment")
k.clust$centers
dist(k.clust$centers, method = "euclidean", diag = TRUE)

plot3d(FPG, AUPIC, SSPG, col=k.clust$cluster)
plot3d(FPG, AUPIC, SSPG, col=k.clust2$cluster)


## Boxplot

seg1 = d2[k.clust$cluster==1,]
seg2 = d2[k.clust$cluster==2,]
seg3 = d2[k.clust$cluster==3,]

par(mfrow=c(3,2))
boxplot(seg1[,1],seg2[,1],seg3[,1],ylab=names(seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,2],seg2[,2],seg3[,2],ylab=names(seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,3],seg2[,3],seg3[,3],ylab=names(seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,4],seg2[,4],seg3[,4],ylab=names(seg1)[4],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,5],seg2[,5],seg3[,5],ylab=names(seg1)[5],xlab="segment",col="blue",names=c(1,2,3))

