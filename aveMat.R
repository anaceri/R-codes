aveMat <- function(mat){
  mat0 = c()
  for (i in 1:dim(mat)[2]){
    vv = cbind(mat[mat$tr == 't1',i], mat[mat$tr == 't2',i], 
               mat[mat$tr == 't3',i], mat[mat$tr == 't4',i],
               mat[mat$tr == 't5',i])
    
    mat0 = cbind(mat0, rowMeans(vv, na.rm=T))
  }
  if (mat$cond[1]==0){pv = 1.2}
  if(mat$cond[1]==1){pv=-1.2}
  if(mat$cond[1]==2){pv=1.4}
  if(mat$cond[1]==3){pv=-1.4}
  if(mat$cond[1]==4){pv=1.6}
  if(mat$cond[1]==5){pv=-1.6}
  
  colnames(mat0)= colnames(mat)
  mat0 = as.data.frame(mat0)
  mat0$ptint[ which(abs(mat0$ptint) > 1)] = pv
  mat0$ptint[ which(abs(mat0$ptint) <= 1)] = 0
  return(mat0)
}
