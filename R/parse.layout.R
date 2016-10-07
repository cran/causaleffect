parse.layout <-
function(file) {
  doc <- xmlParse(file, useInternalNodes = TRUE)
  top <- xmlRoot(doc)
  graph <- top[["graph"]]
  nsy <- c(nsy = "http://www.yworks.com/xml/graphml")
  layout.nodes <- getNodeSet(doc, "//nsy:Geometry[@x]", nsy)
  layout <- matrix(0, length(layout.nodes), 2)
  for (i in 1:length(layout.nodes)) {
    layout[i,1] <- as.numeric(xmlGetAttr(layout.nodes[[i]], "x"))
    layout[i,2] <- -as.numeric(xmlGetAttr(layout.nodes[[i]], "y"))
  }
  free(doc)
  return(layout)
}