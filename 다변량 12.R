install.packages("MASS")
library(MASS)
data("eurodist")
str("eurodist")
head("eurodist")
eurodist
class(eurodist)
loc<-cmdscale(eurodist)
loc
x<-loc[,1]
y<-loc[,2]
x
y
plot(x,y,type = "n",asp=1,main = "Metric MDS")
text(x,y,rownames(loc),cex = 1.2)
abline(v=0, h=0, lwd=1)
data("swiss")
swiss
d<-dist(swiss)
d
mds<-isoMDS(d)
mds
plot(mds$points,type ="n" )
text(mds$points,labels = rownames(swiss),cex = 1.2)
abline(v=0,h=0,lty=1,lwd=1)
data("USJudgeRatings")
str(USJudgeRatings)
d2<-dist(USJudgeRatings)
loc2<-cmdscale(d2)
loc2
x<-loc2[,1]
y<-loc2[,2]
plot(x,y,type = "n",main = "MDS")
text(x,y,labels = rownames(swiss),cex =1.2 )
abline(v=0,h=0,lty=2,lwd=1)
