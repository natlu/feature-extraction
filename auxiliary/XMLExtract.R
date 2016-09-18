#' Gets XML information
#' @import XML
#' @import data.table
#' @export
xmlExtract <- function(xmlLink) {
  xmlAsText <- suppressWarnings(readLines(xmlLink))
  xmlAsText[[2]] <- '<document version="1.0">'
  x <- xmlParse(xmlAsText)
  
  x <- xmlRoot(x)
  y <- xmlChildren(x)
  
  ### Rename the list name for y
  ### page 1 will be y[['page1']] etc
  # Get the pages of the document
  pages <- which(names(y) == "page")
  z <- names(y)
  z[pages] <- paste0('page', 1:length(pages))
  names(y) <- z
  document_data <- y[[1]]
  y <- y[2:length(y)]

  
  # Getting the elemId and columCount from documentData
  document_data.streams <- xpathApply(document_data, "//stream")
  mainText <- lapply(document_data.streams, function(x) xpathApply(x, ".//mainText", xmlAttrs))
  elemId <- lapply(document_data.streams, function(x) xpathApply(x, ".//elemId", xmlAttrs))
  mainText <- lapply(mainText, data.table)
  elemId <- lapply(elemId, data.table)
  columnCount <- mapply(data.table, elemId = elemId, numColumn = mainText, SIMPLIFY = F)
  columnCount <- rbindlist(columnCount)
  if (nrow(columnCount) == 0) {
    columnCount <- data.table(elemId.V1 = "NA",
                              numColumn.V1 = 0)
  }
  setnames(columnCount, "elemId.V1", "pageElemId")
  setnames(columnCount, "numColumn.V1", "numColumn")
  
  columnCount[, pageElemId := as.character(pageElemId)]
  columnCount[, numColumn := as.numeric(numColumn)]
  
  setkey(columnCount, "pageElemId")
  
  # Get area of pages
  pageArea <- lapply(y, xmlAttrs)
  
  # Get text blocks
  textBlocks.count <- lapply(y, function(x) length(getNodeSet(x, ".//block[@blockType = 'Text']")))
  textBlocks.attrs <- lapply(y, function(x) xpathApply(x, ".//block[@blockType = 'Text']", xmlAttrs))
  
  # z <- lapply(textBlocks.attrs, function(x) lapply(x, function(x) t(data.table(x))))
  # lapply(y, function(x) xpathApply(x, ".//block[contains(@blockType, 'Picture') or contains(@blockType, 'Text')]", xmlAttrs))
  ###
  # textBlocks.attrs <- lapply(y, function(x) xpathApply(x, ".//block[@blockType]", xmlAttrs))
  xpath_str <- ".//block[contains(@blockType, 'Picture') or contains(@blockType, 'Text') or contains(@blockType, 'Table')]"
  textBlocks.attrs <- lapply(y, function(x) xpathApply(x, xpath_str, xmlAttrs))
  textBlocks.attrs <- lapply(textBlocks.attrs, function(x) lapply(x, as.list))
  ###
  
  
  textBlocks <- lapply(textBlocks.attrs, function(x) data.table(rbindlist(x, fill = TRUE)))
  
  for(j in 1:length(textBlocks)) {
    if (! "pageElemId" %in% names(textBlocks[[j]])) {
      textBlocks[[j]][, "pageElemId" := "NA"]
    }
  }
  
  numcols <- c("l", "r", "t", "b") # columns to be numeric
  textBlocks <- lapply(textBlocks, function(x) x[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols])
  
  
  # Setting keys for all pages
  textBlocks <- lapply(textBlocks, function(x) setkey(x, "pageElemId"))
  # Adding in number of columns from the data table columnCount
  textBlocks <- lapply(textBlocks, function(x) columnCount[x])

  textBlocks <- lapply(textBlocks, function(x) x[, width  := r - l])
  textBlocks <- lapply(textBlocks, function(x) x[, height := b - t])
  textBlocks <- lapply(textBlocks, function(x) x[, area := height * width])
  textBlocks <- lapply(textBlocks, function(x) x[, ratio := width / height])
  
  # A better metrix than l, t, r, b might be to actually label which quadrant/section
  # the box is in. So for each box, the upper left and lower right corners should
  # suffice. 
  # This may be hard to do with SVM. So instead, have features such as
  # "1 sections", "2 sections" ...
  # 2 sections would mean that the box is in 2 sections. All we need to figure out
  # which sections a box appears in is the locatioin of the upper left corner, and
  # the location of the lower right corner.
  # To adjust this thought to the SVM, we need to consolidate all information from
  # one page into one row. This can be done by just specifying all combinations of
  # crossings between all sections. So if we have quadrants, the feature names are:
  # "q1 only", "q2 only" , ..., "q1 and q2", ..., "q1, q2, q3 and q4" (no triples
  # as this is impossible for quadrants)
  # In the end, we will have a count of how many boxes cross what sections.
  # A better solution that won't produce too many features is: instead of using a
  # binary flag for the features mentioned, we could use the area (or pecentage
  # of the area) in each quadrant a box takes up.
  # This will give us the percentage of the area filled in each section.
  # Might just do both because I don't know which yields better results and
  # because I can.
  # For the sections, will need to make sure that for the vertical splits,
  # I make enough splits so that we can capture the middle section properly because
  # this would be the key differentiator forif a page is split or not.
  
  totalPageArea <- lapply(pageArea, function(x) as.numeric(x[1]) * as.numeric(x[2]))
  totalPageArea <- sum(unlist(totalPageArea))
  
  textBlocks <- rbindlist(mapply(cbind, textBlocks, totalPageArea, SIMPLIFY = F))
  
  textBlocks <- NaModify(textBlocks)

  outputFeatures <- data.table(numColumn = max(textBlocks$numColumn, na.rm = TRUE),
                               hasText = length(which(textBlocks$blockType == "Text")),
                               hasTable = length(which(textBlocks$blockType == "Table")),
                               hasPicture = length(which(textBlocks$blockType == "Picture")),
                               areaPercent = sum(textBlocks$area) / totalPageArea)
  
  return(outputFeatures)
}



submatrixDim <- lapply(pageArea,
                       function(x) data.table(sectionWidth = as.numeric(x[1]) / 5,
                                              sectionLength = as.numeric(x[2]) / 7))

textBlocks <- mapply(data.table, textBlocks, submatrixDim, SIMPLIFY=FALSE)

lapply(textBlocks, function(x) x[, tll := ceiling(t / sectionLength)])
lapply(textBlocks, function(x) x[, tlw := ceiling(l / sectionWidth)])
lapply(textBlocks, function(x) x[, brl := ceiling(b / sectionLength)])
lapply(textBlocks, function(x) x[, brw := ceiling(r / sectionWidth)])


# lapply(textBlocks, function(x) x[, tlw := NULL])
# lapply(textBlocks, function(x) x[, tll := NULL])
# lapply(textBlocks, function(x) x[, brw := NULL])
# lapply(textBlocks, function(x) x[, brl := NULL])


# Going to go with 7 x 5 sub matrices
# names of the sub matrics (r, c)
subMatNames <- as.vector(t(outer(1:7, 1:5, "paste")))

# Add sub matrix names to data tables
# lapply(textBlocks, function(x) x[, (subMatNames) := 0])

lapply(textBlocks, function(x) x[, seql := t(mapply(seq, tll, brl, SIMPLIFY = TRUE))])
lapply(textBlocks, function(x) x[, seqw := t(mapply(seq, tlw, brw, SIMPLIFY = TRUE))])

lapply(textBlocks, function(x) apply(x, 1, function(y) expand.grid(y$seql, y$seqw)))


apply(foo, 1, function(x) expand.grid(x$seql, x$seqw))


abc <- lapply(textBlocks, function(y) expand.grid(y$seql[[1]], y$seqw[[1]]))




foofunc <- function(y) {
  apply(y, 1, function(z) seq(z[1], z[3]))
  apply(y, 1, function(z) seq(z[2], z[4]))
  
  hits <- apply(y, 1, function(z) expand.grid(seq(z[1], z[3]), seq(z[2], z[4])))
  hits <- as.list(lapply(hits, function(x) (table(apply(x, 1, paste, collapse = " ")))))
  lapply(hits, function(x) {
    foo <- as.data.table(x)
    foofoo <- data.table(t(foo$N))
    setnames(foofoo, names(foofoo), foo$V1)
    
    return(foofoo)
  }) -> hits
  
  hits <- rbindlist(hits, fill = TRUE, use.names = T)
  NaModify(hits)
  # hits <- apply(y, 1, function(z) outer(seq(z[1], z[3]), seq(z[2], z[4]), "paste"))
  
  return(hits)
}



textBlocks <- lapply(textBlocks, function(x) cbind(x, foofunc(x[, .(tll, tlw, brl, brw)])))

lapply(textBlocks, function(x) {
  foo <- setdiff(subMatNames, names(x))
  x[, (foo) := 0]
})
# i think the warning messages here happen when all sub matrices are already filled.




