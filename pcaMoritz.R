setwd("~/Documents/R/synKinDyn_Rcode")

source("callFunction.R")
setwd("~/Documents/R/synKinDyn_Rcode/data")
s1Data <- read.table("djtrials1.txt", header=TRUE)
s1Data = data.frame(s1Data)
colnames(s1Data, do.NULL = FALSE)
colnames(s1Data) <- c(" ", "T_yCCW24", "T_yCW24",  "T_yCCW28", "T_yCW28",  "T_yCCW32", "T_yCW32")

ind =which(!is.nan(s1Data$T_yCCW24))
s1Data = s1Data[ind,]
ind =which(!is.nan(s1Data$T_yCW24))
s1Data = s1Data[ind,]
ind =which(!is.nan(s1Data$T_yCCW28))
s1Data = s1Data[ind,]
ind =which(!is.nan(s1Data$T_yCW28))
s1Data = s1Data[ind,]
ind =which(!is.nan(s1Data$T_yCCW32))
s1Data = s1Data[ind,]
ind =which(!is.nan(s1Data$T_yCW32))
s1Data = s1Data[ind,]

mean(s1Data$T_yCW32)

x = s1Data[,2:7]
pairs(x)

f.pca <- prcomp(s1Data[2:4780,2:7], scores=TRUE, cor=TRUE, na.action = na.omit)
summary(f.pca)
biplot(f.pca)
plot(f.pca)
pairs(x)
