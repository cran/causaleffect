idc <-
function(y, x, z, P, G, to) {
  edges.to.x <- E(G) [1:length(E(G)) %->% x]
  edges.from.z <- E(G) [1:length(E(G)) %<-% z]
  G.xz <- subgraph.edges(G, E(G)[setdiff(E(G), union(edges.to.x, edges.from.z))], delete.vertices = FALSE)
  A <- as.matrix(get.adjacency(observed.graph(G.xz)))
  for (node in z) {
    cond <- setdiff(z, node)
    if (dSep(A, y, node, union(x, cond))) {
      return(idc(y, union(x, node), cond, P, G, to))
    } 
  }
  return(id(union(y, z), x, P, G, to))
}
