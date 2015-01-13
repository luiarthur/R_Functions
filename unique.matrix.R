unique.matrix <- function(MM) {
  # Counts the number of unique matrices in a list.
  # MM = a list of matrices. We want the output to be:
  # 1) a list of UNIQUE matrices
  # 2) a vector of their counts

  eq.matrix <- function(A,B) {
    matrices.are.identical <- F
    if (all(dim(A) == dim(B))) {

      if((A == B) || dim(A)[2] == 0) matrices.are.identical <- T
    }
    matrices.are.identical
  }
  
  N <- length(MM)
  vec <- 1:N
  uniq.M <- list()
  
  counts <- c()
  taken <- rep(F,N)

  for (i in 1:N) {
    if (!taken[i]) {
      for (j in i:N) {
        if (!taken[j]) {
          if (eq.matrix(MM[[i]],MM[[j]])) {
            if (i==j) {
              counts <- c(counts,1)
              uniq.M[[length(uniq.M)+1]] <- ZZ[[j]]
            } else {
              counts[length(counts)] <- counts[length(counts)] + 1         
              taken[j] <- T
            }
          }
        }
      }
      taken[i] <- T
    }
  }
  
  ind <- sort(counts,index.return=T,decr=T)
  ind <- ind$ix
  list(counts[ind],uniq.M[ind])
}


