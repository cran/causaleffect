gather <- function(P, i, G.Adj) {
  ind <- c()
  var <- P$children[[i]]$var
  for (k in (1:length(P$children))[-i]) {
    if (var %in% P$children[[k]]$cond) {
      if (!dSep(G.Adj, P$children[[k]]$var, var, setdiff(P$children[[k]]$cond, var))) {
        ind <- c(ind, k)
      } else {
        P$children[[k]]$cond <- setdiff(P$children[[k]]$cond, var)
      }
    }
  }
  return(list(ind, P))
}