sortData <- function(mat){
  
  i0 = which(abs(mat$ptint) > 1)
  i1 = c(i0, 0) - c(0, i0)
  i2 = which(abs(i1) > 1)
  pb = i0[1] - 1
  pe = i0[length(i0)] + 1
  te = dim(mat)[1]
  
  i3 = c(i2, 0) - c(0, i2)
  i4 = sort(i3[2:6], index.return =T)
  
  p1bo = i0[1]
  p1eo = i0[i2[2]-1]
  
  p1bf = i0[i2[2]-1] + 1
  p1ef = i0[i2[2]] - 1
  
  p2bo = i0[i2[2]]
  p2eo = i0[i2[3] - 1]
  
  p2bf = i0[i2[3]-1] + 1
  p2ef = i0[i2[3]] - 1
  
  p3bo = i0[i2[3]]
  p3eo = i0[i2[4] - 1]
  
  p3bf = i0[i2[4]-1] + 1
  p3ef = i0[i2[4]] - 1
  
  p4bo = i0[i2[4]]
  p4eo = i0[i2[5] - 1]
  
  p4bf = i0[i2[5]-1] + 1
  p4ef = i0[i2[5]] - 1
  
  p5bo = i0[i2[5]]
  p5eo = i0[i2[6] - 1]
  
  ton = c(p1eo - p1bo, p2eo - p2bo, p3eo - p3bo, p4eo - p4bo, p5eo - p5bo)
  toff = c(p1ef - p1bf, p2ef - p2bf, p3ef - p3bf, p4ef - p4bf)
  
  ion  = sort(ton, index.return =T)
  ioff = sort(toff, index.return =T)
  
  
  inton  = cbind(c(p1bo, p2bo, p3bo, p4bo, p5bo), c(p1eo, p2eo, p3eo, p4eo, p5eo)) 
  intoff = cbind(c(p1bf, p2bf, p3bf, p4bf), c(p1ef, p2ef, p3ef, p4ef)) 
  
  intnon = inton[ion$ix,]
  intnoff = intoff[ioff$ix,]
  
  newix = c(c(1:pb), c(intnon[1,1]:intnon[1,2]), c(intnoff[1,1]:intnoff[1,2]), 
            c(intnon[2,1]:intnon[2,2]), c(intnoff[2,1]:intnoff[2,2]), 
            c(intnon[3,1]:intnon[3,2]), c(intnoff[3,1]:intnoff[3,2]),
            c(intnon[4,1]:intnon[4,2]), c(intnoff[4,1]:intnoff[4,2]),
            c(intnon[5,1]:intnon[5,2]), c(pe:te)) 
  ns = mat[newix,]
  
  
  dm <- dim(ns)[1]
  dt  <- c(c(1:dm)) * 0.005
  ns <- cbind(dt, ns)
  return(ns)
}
