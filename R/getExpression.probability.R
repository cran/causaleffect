getExpression.probability <-
function(x) {
  P <- ""
  s.print <- length(x$sumset) > 0
  sum.string <- ""
  cond.string <- ")"
  if (x$fraction) P <- "\\frac{"
  if (s.print) {
    sum.string <- paste(tolower(x$sumset), sep = "", collapse = ",")
    P <- paste(P, "\\sum_{", sum.string, "}[", sep = "", collapse = "")
  }
  if (x$recursive) {
    for (i in 1:length(x$children)) P <- paste(P, getExpression(x$children[[i]]), sep = "", collapse = ",")
  } else {
    var.string <- paste(tolower(x$var), sep = "", collapse = ",")
    P <- paste(P, "P(", var.string, sep = "", collapse = "")
    if (length(x$cond) > 0) {
      cond.string <- paste(tolower(x$cond), sep = "", collapse = ",")
      cond.string <- paste("\\vert ", cond.string, ")", sep = "", collapse = ",")
    }
    P <- paste(P, cond.string, sep = "", collapse = ",")
  }
  if (s.print) P <- paste(P, "]", sep = "", collapse = ",")
  if (x$fraction) { 
    P <- paste0(P, "}{")
    P <- paste(P, getExpression(x$divisor), sep = "", collapse = ",")
    P <- paste0(P, "}")
  }
  return(P)
}
