#' getXMLFeatures
#'
#' @import data.table
#' @import handy
#' @import magrittr
#' @export
#' @docType methods
#' @rdname getXMLFeatures-methods
setGeneric(name="getXMLFeatures",
           def=function(feature_obj)
           {
             standardGeneric("getXMLFeatures")
           }
)

#' @rdname getXMLFeatures-methods
#' @aliases getXMLFeatures,ANY,ANY-method
setMethod(f="getXMLFeatures",
          signature="foo_class",
          definition=function(feature_obj)
          {
            xmlFeatures <- feature_obj@dat[, .(idx, xmlFiles)]

            # Although its probably not best practice, I am assuming that all the idxs
            # generated in rbindlist are the same idxs in dat
            # So I am not going to pass the idxs through the function
            lapply(xmlFeatures$xmlFiles, xmlExtract) %>%
              rbindlist(use.names = TRUE, idcol = "idx") -> output

            feature_obj@xmlFeatures <- output

            return(feature_obj)
          }
)

#' @import XML
#' @import magrittr
#' @export
xmlExtract <- function(xmlLink) {
  # Read in the XML ------------------------------------------------------------
  xmlAsText <- suppressWarnings(readLines(xmlLink))
  xmlAsText[[2]] <- '<document version="1.0">'
  xmlAsText %>%
    xmlParse %>%
    xmlRoot %>%
    xmlChildren -> xmlDat

  # Get the different pages of the document
  pages <- which(names(xmlDat) == "page")
  z <- names(xmlDat)
  z[pages] <- 1:length(pages)
  names(xmlDat) <- z
  # Get the document data of the document
  document_data <- xmlDat[['documentData']]
  if (length(document_data) > 0)
    xmlDat <- xmlDat[2:length(xmlDat)]

  # Document Data extraction ---------------------------------------------------
  # Getting the elemId and columCount from documentData
  document_data.streams <- xpathApply(document_data, "//stream")

  document_data.streams %>%
    lapply(function(x) xpathApply(x, ".//mainText", xmlAttrs)) %>%
    lapply(data.table) -> mainText

  document_data.streams %>%
    lapply(function(x) xpathApply(x, ".//elemId", xmlAttrs)) %>%
    lapply(data.table) -> elemId

  mapply(data.table, elemId = elemId, numColumn = mainText, SIMPLIFY = F) %>%
    rbindlist -> columnCount

  # Make an empty table if elemId and mainText data doesn't exist
  if (nrow(columnCount) == 0)
    columnCount <- data.table(elemId.V1 = "NA", numColumn.V1 = 0)

  setnames(columnCount, "elemId.V1",    "pageElemId")
  setnames(columnCount, "numColumn.V1", "numColumn")
  columnCount[, pageElemId := as.character(pageElemId)]
  columnCount[, numColumn  := as.numeric(numColumn)]

  setkey(columnCount, "pageElemId")

  # Page Data extraction -------------------------------------------------------
  xpath_str <- strwrap(".//block[contains(@blockType, 'Picture') or
                                 contains(@blockType, 'Text') or
                                 contains(@blockType, 'Table')]",
                       width = 1000, simplify = TRUE)

  xmlDat %>%
    lapply(function(x) xpathApply(x, xpath_str, xmlAttrs)) %>%
    lapply(function(x) lapply(x, as.list)) -> xmlBlocks.attrs

  xmlBlocks <- lapply(xmlBlocks.attrs,
                       function(x) data.table(rbindlist(x, fill = TRUE)))

  # If there is no pageElemId column, add one in with value NA
  for(j in 1:length(xmlBlocks)) {
    if (! "pageElemId" %in% names(xmlBlocks[[j]]))
      xmlBlocks[[j]][, "pageElemId" := "NA"]
  }

  # which columns to be numeric
  numcols <- c("l", "r", "t", "b")
  xmlBlocks %>%
    lapply(function(x) x[, (numcols) := lapply(.SD, as.numeric),
                         .SDcols = numcols]) -> xmlBlocks

  xmlBlocks <- lapply(xmlBlocks, function(x) setkey(x, "pageElemId"))

  # Adding in number of columns from the data table columnCount with
  # key as `pageElemId`
  xmlBlocks %>%
    lapply(setkey, "pageElemId") %>%
    lapply(function(x) columnCount[x]) -> xmlBlocks

  # creating other metrics for the pages
  xmlBlocks %>%
    lapply(function(x) x[, width  := r - l]) %>%
    lapply(function(x) x[, height := b - t]) %>%
    lapply(function(x) x[, area := height * width]) %>%
    lapply(function(x) x[, ratio := width / height])

  # Sub Matrix extraction ------------------------------------------------------
  pageArea <- lapply(xmlDat, xmlAttrs)
  pageArea %>%
    lapply(function(x) as.numeric(x[1]) * as.numeric(x[2])) %>%
    unlist %>%
    sum -> totalPageArea

  pageArea %>%
    lapply(function(x) data.table(sectionWidth = as.numeric(x[1]) / 5,
                                  sectionLength = as.numeric(x[2]) / 7)) ->
    submatrixDim

  xmlBlocks <- mapply(data.table, xmlBlocks, submatrixDim, SIMPLIFY = FALSE)

  xmlBlocks %>%
    lapply(function(x) x[, tll := ceiling(t / sectionLength)]) %>%
    lapply(function(x) x[, tlw := ceiling(l / sectionWidth)]) %>%
    lapply(function(x) x[, brl := ceiling(b / sectionLength)]) %>%
    lapply(function(x) x[, brw := ceiling(r / sectionWidth)])

  # names of the sub matrics (r, c)
  subMatNames <- as.vector(t(outer(1:7, 1:5, "paste")))

  xmlBlocks %>%
    lapply(function(x) cbind(x, wheresWally(x[, .(tll, tlw, brl, brw)]))) ->
    xmlBlocks

  # Add in column of zeros for missing sub matrix names
  lapply(xmlBlocks, function(x) {
    foo <- setdiff(subMatNames, names(x))
    if (length(foo) != 0) x[, (foo) := 0]
  })

  xmlBlocks <- rbindlist(xmlBlocks,
                         use.names = TRUE,
                         fill = TRUE,
                         idcol = "page")

  xmlBlocks[, page := as.numeric(page)]

  # filtering and ordering the columns
  cols <- c("page", "numColumn", "blockType",
            "width", "height", "area", "ratio",
            "tll", "tlw", "brl", "brw",
            "1 1", "1 2", "1 3", "1 4", "1 5",
            "2 1", "2 2", "2 3", "2 4", "2 5",
            "3 1", "3 2", "3 3", "3 4", "3 5",
            "4 1", "4 2", "4 3", "4 4", "4 5",
            "5 1", "5 2", "5 3", "5 4", "5 5",
            "6 1", "6 2", "6 3", "6 4", "6 5",
            "7 1", "7 2", "7 3", "7 4", "7 5")

  return(xmlBlocks[, cols, with = FALSE])
}

# Function to find which submatrices have something in it ----------------------
#' @import data.table
#' @import magrittr
#' @export
wheresWally <- function(dat) {
  dat %>%
    apply(1, function(z) expand.grid(seq(z[1], z[3]), seq(z[2], z[4]))) %>%
    lapply(function(x) table(apply(x, 1, paste, collapse = " "))) %>%
    as.list -> foundYa

  foundYa <- lapply(foundYa, function(x) {
    foo <- as.data.table(x)
    foofoo <- data.table(t(foo$N))
    setnames(foofoo, names(foofoo), foo$V1)
    return(foofoo)
  })

  foundYa <- rbindlist(foundYa, fill = TRUE, use.names = T)
  NaModify(foundYa)

  return(foundYa)
}
