parse.joint <-
function(P, v, cond, var) {
  P.num <- P
  P.num$sumset <- c(union(P$sumset, setdiff(var, union(v, cond))))
  P.num <- parse.expression(P.num)
  if (length(cond) > 0) {
    P.den <- P
    P.den$sumset <- c(union(P$sumset, setdiff(var, cond)))
    P.den <- parse.expression(P.den)
    i <- 1
    k <- 0
    while (i <= length(P.num$children) & length(P.num$children) > 0 & length(P.den$children) > 0) {
      is.element <- FALSE
      for (j in 1:length(P.den$children)) {
        if (identical(P.num$children[[i]], P.den$children[[j]])) {
          is.element <- TRUE
          k <- j
          break
        }
      }
      if (is.element) {
        P.den$children[[k]] <- NULL
        P.num$children[[i]] <- NULL
        i <- 0
      }
      i <- i + 1
    }
    if (length(P.den$children) > 0) {
      P.num$fraction <- TRUE
      P.num$divisor <- P.den
    }
  }
  return(P.num)
}
