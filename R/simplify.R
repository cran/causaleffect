simplify <- 
function(P, to, G.Adj) {
  j <- 0
  while (j < length(P$sumset)) {
    j <- j + 1
    vars <- unlist(lapply(P$children, FUN = function(x) x$var))
    i <- which(vars == P$sumset[j])
    k <- 1
    n <- 1
    terms <- gather(P, i, G.Adj)
    I <- terms[[1]]
    P <- terms[[2]]
    q <- length(I)+1
    omega <- perm(I, n, i)
    J <- character()
    D <- character()    
    while (k <= q) {  
      joint <- join(J, D, P$children[[omega[k]]]$var, P$children[[omega[k]]]$cond, G.Adj)
      if (length(joint[[1]]) <= length(J)) {
        k <- 1
        n <- n + 1
        if (n > factorial(q-1)) break
        omega <- perm(I, n, q)
        J <- character()
        D <- character()
      } else {
        k <- k + 1
        J <- joint[[1]]
        D <- joint[[2]]
      }
    }
    if (k == q+1) {
      P <- factorize(J, D, P, omega, q)     
      P$children[[i]] <- NULL
      P$sumset <- P$sumset[-j]
      j <- 0
    }
  }
  return(P)       
}