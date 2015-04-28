idc <- 
function(y, x, z, P, G, to) {
  G.xz <- unobserved.graph(G)
  edges.to.x <- E(G.xz) [1:length(E(G.xz)) %->% x]
  edges.from.z <- E(G.xz) [1:length(E(G.xz)) %<-% z]
  G.xz <- subgraph.edges(G.xz, E(G.xz)[setdiff(E(G.xz), union(edges.to.x, edges.from.z))], delete.vertices = FALSE)
  A <- as.matrix(get.adjacency(G.xz))
  for (node in z) {
    cond <- setdiff(z, node)
    if (dSep(A, y, node, union(x, cond))) {
      return(idc(y, union(x, node), cond, P, G, to))
    } 
  }
  return(id(union(y, z), x, P, G, to))
}