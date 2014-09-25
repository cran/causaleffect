parse.expression <-
function(P) {
  P.parse <- probability(recursive = TRUE, children = list())
  j <- 1
  remove <- c()
  for (i in 1:length(P$children)) {
    if (length(intersect(P$children[[i]]$var, P$sumset)) == 0 & length(intersect(P$children[[i]]$cond, P$sumset)) == 0) {
      P.parse$children[[j]] <- P$children[[i]]
      remove <- c(remove, i)
      j <- j + 1
    }
  }
  P$children[remove] <- NULL
  if (length(P$children) == 0) return(P.parse)
  k <- 0
  j <- 0
  while (k <= length(P$sumset) & length(P$sumset) > 0 & length(P$children) > 0) {
    k <- k + 1
    count <- 0
    for (i in 1:length(P$children)) {
      if (length(intersect(P$children[[i]]$var, P$sumset[k])) == 0 & length(intersect(P$children[[i]]$cond, P$sumset[k])) == 0) {
        count <- count + 1
        j <- i
      }
    }
    if (count == 1) {
      P$sumset <- P$sumset[-k]
      P$children[[j]] <- NULL
      k <- 0
    }
  }
  if (length(P$children) == 0) return(P.parse)
  else P.parse$children[[length(P.parse$children)+1]] <- P
  return(P.parse)
}
