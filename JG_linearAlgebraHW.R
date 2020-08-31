# Juliana Gines Linear Algebra HW 8/28
#########################################
#set working directory of where your data is stored
setwd('C:/Users/gines/Downloads')
#load data in
library(ggplot2)
load('LeukError.RData')

#Creates new df of just numeric values (last col was type of leukemia)
leuknum=leuk[,1:5000]

#Runs PCA on leuknum df, rank=5 indicates to get top 5 principal components

pcaOut=prcomp(leuknum,rank=5,scale=FALSE)
#pulls out scores for each observation
scores = pcaOut[["x"]]
#make new vector of just types of leukemia
cancers=leuk["V5001"]
#combine with types of leukemia
cancercoords=cbind(pcaOut[["x"]],cancers)
#filter by ALLB 
ALLB =cancercoords %>% filter(V5001 == "ALL-B")

plot(x=cancercoords$PC1,y=cancercoords$PC2)
#filter by AML
AML= cancercoords %>% filter(V5001 == "AML")

#filter by ALLT 
ALLT= cancercoords %>%filter(V5001 == "ALL-T")
#get row names so we can print the observation numbers on the plot
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

#Summary of first three PCs
summary(pcaOut)

#Calculates first 5 props of variance for PCs
sum(pcaOut$sdev[1:1]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:2]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:3]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:4]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:5]^2)/sum(pcaOut$sdev^2)
sum(pcaOut$sdev[1:6]^2)/sum(pcaOut$sdev^2)

#Additional plots

plot(pcaOut$x[,1:2], xlab = "Principal Component 1",
      ylab = "Principal Component 2")

head(pcaOut$rotation[order(abs(pcaOut$rotation[,1]),decreasing=T),1],10)
biplot(pcaOut)
