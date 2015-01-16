unique.matrix <- function(X) {
  # Counts the number of unique matrices in a list.
  # X = a list of matrices. We want the output to be:
  # 1) a list of UNIQUE matrices
  # 2) a vector of their counts

  #lx <- length(X)
  #UX <- unique(X) 
  #lux <- length(UX)
  #counts <- rep(0,lux)
  #for (i in 1:lux) {
  #  for (j in 1:lx) {
  #    if (identical(UX[[i]],X[[j]])) counts[i] <- counts[i] + 1
  #    cat("\r",round((lx*(i-1)+j)/(lx*lux),4))
  #  }
  #}

  #ind <- sort(counts,index.return=T,decr=T)
  #ind <- ind$ix
  #list(counts[ind],UX[ind])
     
  N <- length(MM)   
  vec <- 1:N    
  uniq.M <- list()    
      
  counts <- c()   
  taken <- rep(F,N)   
     
  for (i in 1:N) {    
    if (!taken[i]) {    
      for (j in i:N) {    
        if (!taken[j]) {    
          if (identical(MM[[i]],MM[[j]])) {   
            if (i==j) {   
              counts <- c(counts,1)   
              uniq.M[[length(uniq.M)+1]] <- MM[[j]]   
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
