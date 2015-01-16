unique.matrix <- function(X) {
  # Counts the number of unique matrices in a list.
  # X = a list of matrices. We want the output to be:
  # 1) a list of UNIQUE matrices
  # 2) a vector of their counts

  lx <- length(X)
  UX <- unique(X) 
  lux <- length(UX)
  counts <- rep(0,lux)
  print(counts)
  for (i in 1:lux) {
    for (j in 1:lx) {
      if (identical(UX[[i]],X[[j]])) counts[i] <- counts[i] + 1
    }
  }

  ind <- sort(counts,index.return=T,decr=T)
  ind <- ind$ix
  list(counts[ind],UX[ind])
}
