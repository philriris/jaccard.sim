# Weighted Jaccard similarity index
# With credit to Laszlo Gadar (https://rpubs.com/lgadar/weighted-jaccard)
# This is his code, turned into a self-contained function
sim.jac <- function(data) {
  jac.mat <- matrix(0, nrow=nrow(data), ncol=nrow(data)) #Initialize empty matrix
  rownames(jac.mat) <- rownames(data) #Write row and column names
  colnames(jac.mat) <- rownames(data)
  pairs <- t(combn(1:nrow(data), 2))
  for (i in 1:nrow(pairs)){
    num <- sum(sapply(1:ncol(x), function(x)(min(data[pairs[i,1],x],data[pairs[i,2],x]))))
    den <- sum(sapply(1:ncol(x), function(x)(max(data[pairs[i,1],x],data[pairs[i,2],x]))))
    jac.mat[pairs[i,1],pairs[i,2]] <- num/den
    jac.mat[pairs[i,2],pairs[i,1]] <- num/den  
  }
  jac.mat[which(is.na(jac.mat))] <- 0 # Turn NAs into 0
  diag(jac.mat) <- 0 # Make the diagonal 0. If using loops in network data, this could be = 1
  return(jac.mat)
}
