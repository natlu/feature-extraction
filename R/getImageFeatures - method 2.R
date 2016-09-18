# Functions for extracting image features
# Notes: For the histogram/density, to consolidate all pages in a document,
# the densities at each bin will be averaged. The reasoning behind this is
# that I am assuming all pages in a document will be of similar format such
# as having two columns, lots of tables etc even though they aren't always
# in the same format.
# Another option is to run another classification through it and label it
# with things like "article", "tables" etc.

#' getImageFeatures
#'
#' @import data.table
#' @import handy
#' @import snow
#' @import snowfall
#' @import magrittr
#' @export
#' @docType methods
#' @rdname getImageFeatures-methods
setGeneric(name="getImageFeatures",
           def=function(feature_obj, parallel, cores)
           {
             standardGeneric("getImageFeatures")
           }
)

#' @rdname getImageFeatures-methods
#' @aliases getImageFeatures,ANY,ANY-method
setMethod(f="getImageFeatures",
          signature="foo_class",
          definition=function(feature_obj, parallel = TRUE, cores = 3)
          {
            # imageFeatures <- dat[, .(pdfFiles)]
            imageFeatures <- feature_obj@dat[, .(pdfFiles)]

            # Get locations of png files
            imageFeatures[, directory := str_extract(pdfFiles, "^(.+)/")]
            imageFeatures[, imgRegex  := str_extract(pdfFiles, "([^/]+).(?=.pdf$)")]
            imageFeatures[, imgRegex  := paste0(imgRegex, "(-[0-9]+|).png")]

            imageFeatures[, idx := 1:nrow(imageFeatures)] ###

            ########### Now setting parallel and cores based on input
            ### need to add check in so that data is not empty
            if (parallel && nrow(imageFeatures) == 1) {
              message("Number of pdf files is 1.")
              message("No point in running multiple cores.")
              message("Setting parallel to FALSE")
              parallel <- FALSE
            } else if (parallel && cores > nrow(imageFeatures)) {
              message(cores, " cores specified.")
              message(nrow(imageFeatures), " pdf files in data.")
              message("Don't have to use so many cores.")
              message("Setting number of cores used to ", nrow(imageFeatures))
              cores <- nrow(imageFeatures)
            }

            imageFeatures[, .(idx, list.files(path = directory, pattern = imgRegex, full.names = TRUE)),
                          by = pdfFiles] %>%
              setnames("V2", "pngFiles") -> imageFeatures

            # Function to be used to get output in desired format
            foofunc <- function(x) {
              output <- vector("list", length(x))
              for (i in 1:length(x)) {
                output[[i]] <- getImgFeatures(x[i])
              }
              output <- rbindlist(output)
              return(output)
            }

            # png100 <- split(imageFeatures$pngFiles, ceiling(seq_along(imageFeatures$pngFiles)/100))

            pngsplit <- split(imageFeatures, imageFeatures$idx) ###

            if (parallel && length(pngsplit) > 1) {
              if (cores > parallel::detectCores()) {
                message(cores, " cores!")
                message("A little optimistic there aren't we? Only have ", parallel::detectCores(), " cores.")
                message("Setting number of cores to ", parallel::detectCores())
                cores <- parallel::detectCores()
              }

              require(snow)
              sfInit(parallel = TRUE, cpus = cores)
              sfExport("data.table", namespace = "data.table")
              sfExport("rbindlist", namespace = "data.table")
              sfExport("%>%", namespace = "magrittr")
              # sfExport("foreach", namespace = "foreach")
              # sfExport("%do%", namespace = "foreach")
              sfExport("matsplitter", namespace = "handy")
              sfExport("readPNG", namespace = "png")
              sfExport("getImgFeatures", namespace = "foo_class")
              sfExport("imageMatrix", namespace = "foo_class")
              sfExport("subMatrixDensity", namespace = "foo_class")
              sfExport("imageDistr", namespace = "foo_class")
              sfExport("img_to_colorramp", namespace = "foo_class")
              sfExport("brightness", namespace = "foo_class")
              sfExport("as.raster", namespace = "grDevices")
              sfExport("col2rgb", namespace = "grDevices")
              sfExport("gray", namespace = "grDevices")
              sfExportAll()

              output <- sfLapply(pngsplit, foofunc)

              sfStop()
            } else {
              if (parallel) message("Data size is small. Not using parallel processing.")
              output <- foofunc(imageFeatures$pngFiles)
            }

            feature_obj@imageFeatures <- output

            return(feature_obj)
          }
)


#---------- Wrapper function to get features ----------#
#' @import data.table
#' @export
getImgFeatures <- function(pngFile) {
  imgMatrix <- imageMatrix(pngFile)

  imgDistrCols <- imageDistr(imgMatrix, by.col = TRUE)
  names(imgDistrCols) <- paste0("coldistr", names(imgDistrCols))
  imgDistrRows <- imageDistr(imgMatrix, by.col = FALSE)
  names(imgDistrRows) <- paste0("roldistr", names(imgDistrRows))

  # Getting the accentuated column distributions
  # I am hoping this will catch where there are long black vertical lines
  imgDistrColsA <- imgDistrCols
  names(imgDistrColsA) <- paste0(names(imgDistrColsA), "A")
  distrQuantiles <- quantile(imgDistrColsA)
  a <- which(imgDistrColsA <= distrQuantiles[4] & imgDistrColsA > distrQuantiles[3])
  b <- which(imgDistrColsA <= distrQuantiles[3] & imgDistrColsA > distrQuantiles[2])
  c <- which(imgDistrColsA <= distrQuantiles[2])
  imgDistrColsA[a] <- imgDistrColsA[a] / 2
  imgDistrColsA[b] <- imgDistrColsA[b] / 4
  imgDistrColsA[c] <- imgDistrColsA[c] / 8

#   > dim(imgMatrix)
#   [1] 1402  993
# 1402/7 = 200, 993/5 = 199
  imgSubMat <- matsplitter(imgMatrix, 200, 199, FALSE)

  subMatDensity <- subMatrixDensity(imgSubMat)
  names(subMatDensity) <- paste0("subMatDen", 1:length(subMatDensity))

  output <- data.table(pngFiles = pngFile, t(c(imgDistrCols, imgDistrRows, imgDistrColsA, subMatDensity)))

  return(output)
}


#---------- Function to turn a png file to a matrix ----------#
#' @import png
#' @export
imageMatrix <- function(imageDir, cutoff = 0.9) {
  imgMatrix <- readPNG(imageDir)

  # Arbitrary cutoff of 0.9 to make 'light gray' areas 'white'
  # Done so end result has more defined lines
  for (i in 1:dim(imgMatrix)[3]) {
    foo <- imgMatrix[, , i]
    foo[foo > cutoff] <- 1
    imgMatrix[, , i] <- foo
  }

  imgMatrix <- as.raster(imgMatrix) # Create raster object in order to change to gray scale
  imgMatrix <- img_to_colorramp(imgMatrix) # Change to gray scale

  imgMatrix[imgMatrix != "#FFFFFFFF"] <- 1
  imgMatrix[imgMatrix == "#FFFFFFFF"] <- 0

  imgMatrix <- matrix(as.numeric(imgMatrix), nrow(imgMatrix), ncol(imgMatrix))

  return(imgMatrix)
}

#---------- Functions for conversion into grey scale ----------#
#' @export
# Settings for gray scale
brightness <- function(hex) {
  v <- col2rgb(hex)
  sqrt(0.299 * v[1,]^2 + 0.587 * v[2,]^2 + 0.114 * v[3,]^2) / 255
}

#' @export
# Convert to gray scale
img_to_colorramp <- function(img) {
  cv <- as.vector(img)
  b <- brightness(cv)
  g <- gray(b, alpha = TRUE)
  # a <- substr(cv, 8, 9)  # get alpha values
  # ga <- paste0(g, a)     # add alpha values to new colors
  img.gray <- matrix(g, nrow(img), ncol(img), byrow = TRUE)
}

#---------- Function to get histograms of 'blackness' ----------#
#' @import handy
#' @export
imageDistr <- function(imgMatrix, binWidth = 10, normalise = TRUE, by.col) {
  # Shouldn't get any NA values
  stopifnot(! any(is.na(imgMatrix)))

  sumBy <- ifelse(by.col, "colSums(imgMatrix)", "rowSums(imgMatrix)")

  distr <- eval(parse(text = sumBy))
  distr <- tapply(distr, (seq_along(distr)-1) %/% binWidth, sum)

  if (normalise)
    distr <- distr / sum(imgMatrix)

  return(distr)
}



#---------- Function to get densities of submatrices ----------#
#' @import handy
#' @export
subMatrixDensity <- function(mats, normalise = TRUE) {
  if (length(dim(mats)) < 3)
    stop("mats doesn't have required sub matrix structure")

  subMatDensity <- apply(mats, 3, sum, na.rm = T)

  if (normalise)
    subMatDensity <- subMatDensity / sum(mats, na.rm = T)

  return(subMatDensity)
}
