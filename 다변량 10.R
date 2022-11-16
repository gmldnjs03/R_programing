str(USArrests)
z <- data.frame(scale(USArrests))
head(z)
d_euc1 <-dist(z, method = "euclidean")
hc_single <-hclust(d_euc1, method = "single")
library(cluster)
cop_single <-cophenetic(hc_single)
cor(cop_single, d_euc1)
agnes(z, method = "single")$ac
hc_comp1 <-hclust(d_euc1, method="complete")
cop_comp1 <- cophenetic(hc_comp1)
cor(cop_comp1, d_euc1)
agnes(z, method="complete")$ac
hc_ave<-hclust(d_euc1, method="average")
cop_ave <- cophenetic(hc_ave)
cor(cop_ave,d_euc1)
agnes(z, method="average")$ac
hc_ward <-hclust(d_euc1, method="ward.D")
cop_ward<-cophenetic(hc_ward)
cor(cop_ward, d_euc1)
agnes(z, method="ward")$Ac
d_manh <- dist(z, method="manhattan")
hc_single <-hclust(d_euc1, method = "single")
cop_single <-cophenetic(hc_single)
cor(cop_single, d_euc1)
agnes(z, method = "single")$ac