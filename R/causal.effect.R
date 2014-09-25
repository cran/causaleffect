causal.effect <-
function(y, x, G) {
  if (!is.dag(observed.graph(G))) stop("Graph 'G' is not a DAG")
  to <- topological.sort(observed.graph(G))
  to <- get.vertex.attribute(G, "name")[to]
  if (length(setdiff(y, to)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, to)) > 0) stop("Set 'x' contains variables not present in the graph.")
  res <- id(y, x, probability(), G, to)
  res <- organizeTerms(res)
  res <- getExpression(res)
  return(res)
}
