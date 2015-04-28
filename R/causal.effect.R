causal.effect <- 
function(y, x, z = NULL, G, expr = TRUE) {
  if (!is.dag(observed.graph(G))) stop("Graph 'G' is not a DAG")
  to <- topological.sort(observed.graph(G))
  to <- get.vertex.attribute(G, "name")[to]
  if (length(setdiff(y, to)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, to)) > 0) stop("Set 'x' contains variables not present in the graph.")
  if (length(z) > 0) {
    if (length(setdiff(z, to)) > 0) stop("Set 'z' contains variables not present in the graph.")
  }
  if(length(z) == 0) { 
    res <- id(y, x, probability(), G, to)
    res <- organizeTerms(res)
  } else { 
    res <- idc(y, x, z, probability(), G, to)
    res <- organizeTerms(res)
    res2 <- res
    res2$sumset <- union(res2$sumset, y)
    res$fraction <- TRUE
    res$divisor <- res2
  }
  if(expr) res <- get.expression(res)
  return(res)
}