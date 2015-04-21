causal.effect <-
function(y, x, z = NULL, G) {
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
    res <- getExpression(res)
    return(res)
  } else {
    res <- idc(y, x, z, probability(), G, to)
    res1 <- organizeTerms(res)
    res2 <- res1
    res2$sumset <- union(res2$sumset, y)
    res <- paste0("\\frac{", getExpression(res1), "}{", getExpression(res2), "}")
    return(res)
  }
}
