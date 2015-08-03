probability <-
function(var = c(), cond = c(), sumset = c(), do = c(), recursive = FALSE, children = list(), fraction = FALSE, divisor = list(), star = FALSE, domain = NULL) {
  p <- list(var = var, cond = cond, sumset = sumset, do = do, recursive = recursive, children = children, fraction = fraction, divisor = divisor, star = star, domain = domain)
  class(p) = "probability"
  return(p)
}
