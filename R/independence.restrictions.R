independence.restrictions <- function(G) {
  G.obs <- observed.graph(G)
  topo <- igraph::topological.sort(G.obs)
  v <- igraph::get.vertex.attribute(G, "name")[topo]
  cc <- c.components(G, v)
  cc.len <- length(cc)
  if (cc.len > 1) {
    indep <- setNames(vector(mode = "list", length = length(v)), v)
    for (i in 1:cc.len) {
      others <- Reduce(union, cc[-i])
      indep[cc[[i]]] <- list(others)
    }
    return(indep)
  }
  return(list())
}