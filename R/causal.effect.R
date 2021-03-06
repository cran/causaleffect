causal.effect <- function(y, x, z = NULL, G, expr = TRUE, simp = FALSE, steps = FALSE, primes = FALSE, prune = FALSE, stop_on_nonid = TRUE) {
  if (length(igraph::edge.attributes(G)) == 0) {
    G <- igraph::set.edge.attribute(G, "description", 1:length(igraph::E(G)), NA)
  }
  G.obs <- observed.graph(G)
  if (!igraph::is.dag(G.obs)) stop("Graph 'G' is not a DAG")
  topo <- igraph::topological.sort(G.obs)
  topo <- igraph::get.vertex.attribute(G, "name")[topo]
  if (length(setdiff(y, topo)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, topo)) > 0) stop("Set 'x' contains variables not present in the graph.")
  if (length(z) > 0 && !identical(z, "")) {
    if (length(setdiff(z, topo)) > 0) stop("Set 'z' contains variables not present in the graph.")
  }
  if (length(intersect(x, y)) > 0) stop("Sets 'x' and 'y' are not disjoint.")
  if (length(intersect(y, z)) > 0) stop("Sets 'y' and 'z' are not disjoint.")
  if (length(intersect(x, z)) > 0) stop("Sets 'x' and 'z' are not disjoint.")
  res <- list()
  algo <- ""
  res.prob <- probability()
  if (is.null(z) || identical(z, "") || identical(z, character(0))) {
    if (prune) {
      res <- pid(y, x, probability(), G, G.obs, topo, topo, list())
      algo <- "pid"
    } else {
      res <- id(y, x, probability(), G, G.obs, topo, topo, list())
      algo <- "id"
    }
    res.prob <- res$P
  } else {
    res <- idc(y, x, z, probability(), G, G.obs, topo, topo, list(), prune)
    res.num <- res$P
    res.den <- res.num
    res.den$sumset <- union(res.den$sumset, y)
    res.prob$fraction <- TRUE
    res.prob$num <- res.num
    res.prob$den <- res.den
    algo <- "idc"
  }
  res.tree <- res$tree
  if (res$tree$call$id) {
    if (simp) {
      G.unobs <- unobserved.graph(G)
      res.prob <- deconstruct(res.prob, probability(), topo)
      res.prob <- parse.expression(res.prob, topo, G.unobs, G, G.obs)
    }
    attr(res.prob, "algorithm") <- algo
    attr(res.prob, "query") <- list(y = y, x = x, z = z)
    if (expr) res.prob <- get.expression(res.prob, primes)
    if (steps) return(list(P = res.prob, steps = res.tree, id = TRUE))
    return(res.prob)
  } else {
    if (stop_on_nonid) stop("Not identifiable.", call. = FALSE)
    res.prob <- probability()
    attr(res.prob, "algorithm") <- algo
    attr(res.prob, "query")<- list(y = y, x = x, z = z)
    if (steps) return(list(P = res.prob, steps = res$tree, id = FALSE))
    if (expr) return("")
    return(NULL)
  }
}
