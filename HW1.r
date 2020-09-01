rm(list = ls(all.names = TRUE))
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)


load("/Users/mrh/Downloads/LeukError.RData")
leuk_var = leuk[-c(5001)]
leuk_name = leuk$V5001

pca = prcomp(leuk_var, scale = F)
summary(pca)

pca_use = data.frame(pca$x[,1], pca$x[,2], leuk_name)
pca_use

plot(pca_use$pca.x...1., pca_use$pca.x...2., 
     xlab = 'Principle Component 1', 
     ylab = 'Principle Component 2',
     col = pca_use$leuk_name, pch=16)
text(pca_use$pca.x...1., pca_use$pca.x...2., rownames(pca_use))
pca_use[19,]
pca_use

char2 = ggplot(pca_use, aes(x=pca.x...1., y=pca.x...2., fill=leuk_name)) + geom_point(aes(color = leuk_name)) + geom_text(aes(label=rownames(pca_use)),hjust=0, vjust=0, size=2.5)
char2
charzard = char2 + xlab('Principle Component 1') + ylab('Principle Component 2')
charzard # this is the best figure I've been able to make, the one's below are just experiments!

char3 = ggplot(pca_use, aes(x=pca.x...1., y=pca.x...2., fill=leuk_name)) + geom_point(aes(color = leuk_name)) + geom_label_repel(aes(label = rownames(pca_use)),
                                                                                                                                 box.padding   = 0.35, 
                                                                                                                                 point.padding = 0.5,
                                                                                                                                 segment.color = 'grey50') +
  theme_classic()

char3

char4 = char3 + xlab('Principle Component 1') + ylab('Principle Component 2')
char4
