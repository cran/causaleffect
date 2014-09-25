observed.graph <-
function(G) {
  obs.edges <- setdiff(1:length(E(G)), which(edge.attributes(G)$description == "U"))
  G.obs <- subgraph.edges(G, E(G)[obs.edges], delete.vertices = FALSE)
  return(G.obs)
}
