deconstruct <- function(P, P.res) {
  if (!P$recursive) {
    P.temp <- probability(var = P$var, cond = P$cond, sumset = P$sumset)
    P.res$children[[length(P.res$children)+1]] <- P.temp
  } 
  if (P$recursive & length(P$sumset) > 0) {
    P.temp <- probability(recursive = TRUE, children = P$children, sumset = P$sumset)
    P.res$children[[length(P.res$children)+1]] <- P.temp
  }
  if (P$fraction) {
    P.res$divisor <- deconstruct(P$divisor, P.res$divisor)
  }
  if (P$recursive & length(P$sumset) == 0) {
    for (i in 1:length(P$children)) {
      P.res <- deconstruct(P$children[[i]], P.res)
    }
  }
  return(P.res)
}

# tt <- deconstruct(gg, probability(recursive = TRUE, children = list()))