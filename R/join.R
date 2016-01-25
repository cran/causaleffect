join <- function(J, D, var, cond, G.Adj) {
  J.new <- character()
  D.new <- character()
  if (length(J) == 0) {
    J.new <- var
    D.new <- cond
    return(list(J.new, D.new))
  }

  if (length(intersect(J, cond)) > 0 && var %in% D) {
    return(list(J, D))
  }

  cond.uni <- union(D, cond)

  if (length(cond.uni) > 0) {
    ds <- powerset(cond.uni, nonempty = FALSE)
    n <- length(ds)
    for (i in 1:n) {
      a.set <- union(setdiff(ds[[i]], setdiff(D, var)), setdiff(setdiff(D, var), ds[[i]]))
      b.set <- union(setdiff(ds[[i]], setdiff(cond, J)), setdiff(setdiff(cond, J), ds[[i]]))
      if (wrap.dSep(G.Adj, J, a.set, setdiff(D, a.set)) && 
          wrap.dSep(G.Adj, var, b.set, setdiff(cond, b.set))) {
        J.new <- union(J, var)
        D.new <- ds[[i]]
        return(list(J.new, D.new))
      }
    } 
  } else {
    J.new <- union(J, var)
    D.new <- cond
    return(list(J.new, D.new))
  }

  return(list(J, D))

}
