# Classes are defined here

#' foo_class class
#'
#' @import handy
#' @import data.table
#' @exportClass foo_class
setClass(
  "foo_class",

  representation(
    dat           = "data.table",
    imageFeatures = "data.table",
    xmlFeatures   = "data.table",
    textFeatrues  = "data.table"
  ),

  prototype(
    dat           = data.table(),
    imageFeatures = data.table(),
    xmlFeatures   = data.table(),
    textFeatures  = data.table()
  ),

  validity=function(object)
  {
    errors <- character()
################################################################################
    # Make sure we have the relevant file names to run process
    reqCols <- c("pdfFiles", "Job.Name")
    matchedCols <- reqCols %in% names(object@dat)
    if (! any(matchedCols)) {
      msg <- sprintf("Missing columns: %s", reqCols[which(! matchedCols)])
      errors <- c(errors, msg)
    }
#     # Check if names assigned to image feature generation
#     nullName <- any(is.null(names(object@imageFeaturesArgs)))
#     if (nullName) {
#       errors <- c(errors, "All image Feature Arguments must have names assigned")
#     }
#     # Correct arguments for the image feature generation
#     matchedArgs <- names(object@imageFeaturesArgs) %in% c("smallSubMat")
#     if (any(! matchedArgs)) {
#       msg <- sprintf("Invalid arguments to image feature generation: %s",
#                      names(object@imageFeaturesArgs)[! matchedArgs])
#       errors <- c(errors, msg)
#     }

    if (length(errors) == 0) TRUE else errors
  }
)
