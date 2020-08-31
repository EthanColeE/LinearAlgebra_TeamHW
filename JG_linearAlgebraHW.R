# Juliana Gines Linear Algebra HW 8/28
#########################################
#set working directory of where your data is stored
setwd('C:/Users/gines/Downloads')
#load data in
library(ggplot2)
load('LeukError.RData')

randomColumns=sample(leuk,5001)
plot(leuk[,randomColumns],col=leuk$V5001)
leuk[2:500]
#Me messing around finding which col isn't numeric
is.na(leuk)
apply(is.na(leuk),2,which)

#Creates new df of just numeric values (last col was type of leukemia)
leuknum=leuk[,1:5000]

#Runs PCA on leuknum df, rank=5 indicates to get top 5 principal components
#but you can mess around with this to see
pcaOut=prcomp(leuknum,rank=5,scale=FALSE)
scores = pcaOut[["x"]]
cancers=leuk["V5001"]
cancercoords=cbind(pcaOut[["x"]],cancers)

ALLB =cancercoords %>% filter(V5001 == "ALL-B")

plot(x=cancercoords$PC1,y=cancercoords$PC2)
scores = pcaOut[["x"]]
cancers=leuk["V5001"]
cancercoords=cbind(pcaOut[["x"]],cancers)
graph=plot_ly(x=cancercoords$PC1,y=cancercoords$PC2,z=cancercoords$PC3,type='scatter3d',mode='markers',marker=list(color=colors))
graph

AML= cancercoords %>% filter(V5001 == "AML")
  ggplot(cancercoords, aes(x=PC1, y=PC2)) + geom_point()

 

plot(x=cancercoords$PC1,y=cancercoords$PC2)
plot(x=cancercoords$PC1,y=cancercoords$PC2)

graph=plot_ly(x=cancercoords$PC1,y=cancercoords$PC2,z=cancercoords$PC3,type='scatter3d',mode='markers',marker=list(color=colors))
graph

ALLT= cancercoords %>%filter(V5001 == "ALL-T")
plot(x=ALLB$PC1,y=ALLB$PC2)
plot(x=ALLB$PC2,y=ALLB$PC3)


print(rownames(AML))
AML$Value_Number = rownames(AML)

## ggplot scatter maybe 19 and 2 based off these plots
ggplot(AML, aes(x=PC1, y=PC2)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))
## ggplot scatter
ggplot(AML, aes(x=PC2, y=PC3)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))
##ggplot scatter
ggplot(AML, aes(x=PC1, y=PC3)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))

#ALLB leuk: point 12
print(rownames(ALLB))
ALLB$Value_Number = rownames(ALLB)
ggplot(ALLB, aes(x=PC1, y=PC2)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))
## ggplot scatter
ggplot(ALLB, aes(x=PC2, y=PC3)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))
##ggplot scatter
ggplot(ALLB, aes(x=PC1, y=PC3)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))
print(rownames(ALLT))
ALLT$Value_Number = rownames(ALLT)

## ggplot scatter: looks like #10
ggplot(ALLT, aes(x=PC1, y=PC2)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))
## ggplot scatter: looks like #6
ggplot(ALLT, aes(x=PC2, y=PC3)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))
##ggplot scatter: again looks like 6 and 10
ggplot(ALLT, aes(x=PC1, y=PC3)) + 
  geom_point() + 
  geom_text(aes(label=Value_Number))

plot(x=ALLT$PC1,y=ALLT$PC2)
plot(x=ALLT$PC2,y=ALLT$PC3)

plot(x=AML$PC1,y=AML$PC2)
plot(x=AML$PC2,y=AML$PC3)

graph=plot_ly(x=cancercoords$PC1,y=cancercoords$PC2,z=cancercoords$PC3,type='scatter3d',mode='markers',marker=list(color=colors))
graph

pcaOut=prcomp(leuknum,rank=3,scale=FALSE)
scores2 = pcaOut[["x"]]
#plot PC1vPC2 and PC2vPc3 etc
dev.off()
plot(x[,1],x[,2])
plot(pcaOut$x[,2],pcaOut$x[,3])

#3D plot of first 3 PCs, might not render on your computer
graph=plot_ly(x=pcaOut$x[,1],y=pcaOut$x[,2],z=pcaOut$x[,3],type='scatter3d',mode='markers',marker=list(color=colors))
graph

#Summary of first three PCs
summary(pcaOut)

#Calculates first 5 props of variance for PCs
sum(pcaOut$sdev[1:1]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:2]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:3]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:4]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:5]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:6]^2)/sum(pcaOut$sdev^2)

#

plot(pcaOut$x[,1:2], xlab = "Principal Component 1",
      ylab = "Principal Component 2")

head(pcaOut$rotation[order(abs(pcaOut$rotation[,1]),decreasing=T),1],10)
biplot(pcaOut)
