#' initialiseData
#'
#' @import data.table
#' @import handy
#' @import magrittr
#' @export
#' @docType methods
#' @rdname initialiseData-methods
setGeneric(name="initialiseData",
           def=function(feature_obj)
           {
             standardGeneric("initialiseData")
           }
)

#' @rdname initialiseData-methods
#' @aliases initialiseData,ANY,ANY-method
setMethod(f="initialiseData",
          signature="foo_class",
          definition=function(feature_obj)
          {
            dat <- feature_obj@dat

            # Giving the pdf's an idex
            dat[, idx := 1:nrow(dat)]

            # File information for easier directory navigation
            dat[, directory := str_extract(pdfFiles, "^(.+)/")]
            dat[, name  := str_extract(pdfFiles, "([^/]+).(?=.pdf$)")]

            # Getting location of xml files
            dat[, xmlFiles := gsub(".pdf$", ".xml", pdfFiles)]

            feature_obj@dat <- dat

            return(feature_obj)
          }
)

