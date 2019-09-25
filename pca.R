setwd("~/Documents/R/synKinDyn")

source("callFunction.R")

s1Data <- read.table("djfull1.txt", header=TRUE)
dm <- dim(s1Data)[1]
dt  <- c(c(1:dm)) * 0.005
s1Data <- cbind(dt, s1Data)
ggplot(s1Data, aes(x=dt, y=fg)) + 
  geom_line(size=1) + xlab("time (s)") + ylab("F (N)") + geom_smooth(stat = "smooth", position = "identity", span = 0.9) + 
  geom_line(data= s1Data, aes(x = dt, y = max(s1Data$fg) * ptint, color = "red")) + 
  theme_bw(base_size = 18)
  
ggplot(s1Data, aes(x=dt, y=rfx)) + 
  geom_line(size=1) + xlab("time (s)") + ylab("F (N)") +
  geom_line(data= s1Data, aes(x = dt, y = (- max(s1Data$rfx) + min(s1Data$rfx)) * ptint, color = "red")) + 
  theme_bw(base_size = 18)
  

##### Reading all data files and averaging all 

for (i in 1:30){
  filename = paste('djfull', i, '.txt', sep="")
  var = read.table(filename, header=TRUE)
  assign(paste('datat', i, sep=""), var)
}

sz = c()
for (i in 1:30){
  var = get(paste('datat', i, sep=""))
  i0 = which(abs(var$ptint) > 1)
  ind1 = i0[1]-500
  ind2 = i0[length(i0)] + 500
  assign(paste('datat', i, sep=""), var[ind1:ind2,])
  sz = c(sz, dim(var[ind1:ind2,])[1])
}

for (i in 1:30){
  var = get(paste('datat', i, sep=""))
  newvar = sortData(var)
  assign(paste('nwdatat', i, sep=""), newvar)
}
md = max(sz)
for (i in 1:30){
  var = get(paste('nwdatat', i, sep=""))
  if(dim(var)[1] != md){
    cd = var$cond[1]
    r = (md - dim(var)[1])%/% 2
    m = (md - dim(var)[1])%% 2
    fillsp1 = matrix(NA, r, dim(var)[2])
    fillsp2 = matrix(NA, r+m, dim(var)[2])
    colnames(fillsp1)= colnames(var)
    colnames(fillsp2)= colnames(var)
    ncar = rbind(fillsp1, var, fillsp2)
    ncar$cond = cd
    assign(paste('nwdatat', i, sep=""), ncar)
  }
  
}

mat = rbind(nwdatat1, nwdatat2, nwdatat3, nwdatat4, nwdatat5, nwdatat6, nwdatat7, 
            nwdatat8, nwdatat9, nwdatat10, nwdatat11, nwdatat12, nwdatat13, 
            nwdatat14, nwdatat15, nwdatat16, nwdatat17, nwdatat18, nwdatat19, 
            nwdatat20, nwdatat21, nwdatat22, nwdatat23, nwdatat24, nwdatat25, 
            nwdatat26, nwdatat27, nwdatat28, nwdatat29, nwdatat30)

xcond0 = mat[mat$cond == 0,]
xcond1 = mat[mat$cond == 1,]
xcond2 = mat[mat$cond == 2,]
xcond3 = mat[mat$cond == 3,]
xcond4 = mat[mat$cond == 4,]
xcond5 = mat[mat$cond == 5,]


tr = c(rep('t1', dim(xcond0)[1]/5), rep('t2', dim(xcond0)[1]/5), rep('t3', dim(xcond0)[1]/5),
       rep('t4', dim(xcond0)[1]/5), rep('t5', dim(xcond0)[1]/5))

xcond0  = cbind(xcond0, tr)
xcond1  = cbind(xcond1, tr)
xcond2  = cbind(xcond2, tr)
xcond3  = cbind(xcond3, tr)
xcond4  = cbind(xcond4, tr)
xcond5  = cbind(xcond5, tr)

#Check plot

mcond0 = aveMat(xcond0)
xcond1[xcond1$tr == 't3',] = NA
mcond1 = aveMat(xcond1)
mcond2 = aveMat(xcond2)
mcond3 = aveMat(xcond3)
mcond4 = aveMat(xcond4)
mcond5 = aveMat(xcond5)

matplot = mc5
ggplot(matplot, aes(x=dt, y=fg)) + geom_line(size=1) + xlab("time (s)") + ylab("F (N)") + geom_smooth(stat = "smooth", position = "identity", span = 0.9) + 
  geom_line(data= matplot, aes(x = dt, y = max(matplot$fg, na.rm=T) * abs(ptint), color = 'red')) + 
  theme_bw(base_size = 18)

fulldata = rbind(mcond0, mcond1, mcond2, mcond3, mcond4, mcond5)
fulldata$cond <- as.factor(fulldata$cond)

###
ind =which(!is.nan(mcond0$dt))
mc0 = mcond0[ind,]
ind =which(!is.nan(mcond1$dt))
mc1 = mcond1[ind,]
ind =which(!is.nan(mcond2$dt))
mc2 = mcond2[ind,]
ind =which(!is.nan(mcond3$dt))
mc3 = mcond3[ind,]
ind =which(!is.nan(mcond4$dt))
mc4 = mcond4[ind,]
ind =which(!is.nan(mcond5$dt))
mc5 = mcond5[ind,]        

sz = min(c(dim(mc0)[1], dim(mc1)[1], dim(mc2)[1], dim(mc3)[1], dim(mc4)[1], dim(mc5)[1]))
mc0 = mc0[1:sz,]
mc1 = mc1[1:sz,]
mc2 = mc2[1:sz,]
mc3 = mc3[1:sz,]
mc4 = mc4[1:sz,]
mc5 = mc5[1:sz,]

xx = rbind(mc0[1000,3:dim(mc0)[2]-1], mc1[1000,3:dim(mc0)[2]-1], mc2[1000,3:dim(mc0)[2]-1], 
           mc3[1000,3:dim(mc0)[2]-1], mc4[1000,3:dim(mc0)[2]-1], mc5[1000,3:dim(mc0)[2]-1])

# joints
xxj = cbind(xx[2:70], xx$fg, xx$cond)
xnam <- paste("x", 1:69, sep="")
xnam1 <-c(xnam,'fg', 'cond')
colnames(xxj) = xnam1
j.pca <- prcomp(y,scores=TRUE, cor=F, scale=TRUE)
plot(j.pca)
summary(j.pca)

biplot(j.pca)

#plot correlation matrix
cor.matrix <- round(cor(dpi, use= "pairwise.complete.obs", method = "spearman"), digits = 2)
library("corrplot")
corrplot(abs(cor.matrix), method = "square", type="lower", addcolorlabel="b", diag=FALSE, cl.lim=c(0,1), addCoefasPercent=FALSE )


PCfit <- function (i,pcdata) 
{PCI <- pcdata$x[,i]%*%t(pcdata$rotation[,i])}

fitted <-lapply(c(1:6),PCfit,pcdata= j.pca)
for( i in c(1:6) ) {
  assign (paste("jPC",i,sep=""),fitted[[i]])}

oPC1 <- round(y - jPC1, 4)
oPC2 <- round(y - (jPC1+jPC2),3)
oPC3 <- round(y - (jPC1+jPC2+jPC3),3)
oPC4 <- round(y - (jPC1+jPC2+jPC3+jPC4),3)

#Plot mean posture and pcs values
coor <- c('x', 'y', 'z')
xyz  <- c(rep(coor, 23)) 
cond <- c("c1", "c2", "c3", "c4", "c5", "c6")
cdn<- c(1:6)
datapc <- cbind(oPC1,cond)
resMat = c()
colnames(resMat, do.NULL = FALSE)
colnames(oPC1, do.NULL = FALSE)

for (i in seq(1,69,3)){
  i1 = i+2
  var = mat[,i:i1]
  colnames(var) <-coor
  resMat = rbind(resMat, var)
}
cd = c(rep(cond, 23))
resMat <- cbind(resMat,cond, cdn)

library("scatterplot3d")
library("rgl")
plot3d(resMat[,1], resMat[,2], resMat[,3], col=resMat[,5], size=10)


#forces
f.pca <- prcomp(df, scores=TRUE, cor=F, scale=TRUE)
plot(f.pca)
summary(f.pca)
biplot(f.pca)


colnames(dpi) = xnam1
fmla <- as.formula(paste("~ ", paste(xnam, collapse= "+")))
j.pca <- prcomp(fmla, dpi, scores=TRUE, cor=F, scale=TRUE)
