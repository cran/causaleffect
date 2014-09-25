probability <-
function(var = c(), cond = c(), sumset = c(), recursive = FALSE, children = list(), fraction = FALSE, divisor = list()) {
  p <- list(var = var, cond = cond, sumset = sumset, recursive = recursive, children = children, fraction = fraction, divisor = divisor)
  class(p) = "probability"
  return(p)
}
