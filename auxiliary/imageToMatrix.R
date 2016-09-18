#' Extract features
#' @import stringr
#' @export
featureExtract.image <- function(imageDir) {
  
  imageBase <- str_extract(imageDir, "([^/]+)$")
  imageBase <- str_replace(imageBase, ".pdf$", "")
  
  dirBase <- str_extract(imageDir, "^(.+)/")
  
  imageDir <- paste0(dirBase, imageBase, "/pdf")
  
  imageFile <- lapply(imageDir, function(x) list.files(x, pattern = ".png$", full.names = TRUE))
  
  avgDensity <- lapply(imageFile, getDensity)
  avgDensity <- data.table(avgDensity)
  
  return(avgDensity)
}

#' Get Densities
#' @export
getDensity <- function(imageFile) {
  if (length(imageFile) == 0) {
    avgDensity <- 0
  } else {
    #     densityList <- vector("list", length(imageFile))
    #     names(densityList) <- imageFile
    imageMatrices <- lapply(imageFile, imageMatrix)
    densities <- lapply(imageMatrices, function(x) sum(colSums(x)) / (dim(x)[1] * dim(x)[2]))
    avgDensity <- mean(unlist(densities))
  }
  return(avgDensity)
}

#' @import png
#' @import data.table
#' @import handy
#' @export
imageMatrix <- function(imageDir) {
  img <- readPNG(imageDir)
  
  # Arbitrary cutoff of 0.9 to make 'light gray' areas 'white'
  # Done so end result has more defined lines
  for (i in 1:dim(img)[3]) {
    foo <- img[, , i]
    foo[foo > 0.9] <- 1
    img[, , i] <- foo
  }
  
  img <- as.raster(img) # Create raster object in order to change to gray scale
  img <- img_to_colorramp(img) # Change to gray scale
  
  img[img != "#FFFFFFFF"] <- "1"
  img[img == "#FFFFFFFF"] <- "0"
  
  img <- data.table(img)
  img <- applyToClass(img, "character", as.numeric)
  
  invisible(img)
}

# Settings for gray scale
#' @export
brightness <- function(hex) {
  v <- col2rgb(hex)
  sqrt(0.299 * v[1]^2 + 0.587 * v[2]^2 + 0.114 * v[3]^2) /255
}

# Convert to gray scale
#' @export
img_to_colorramp <- function(img, ramp=gray) {
  cv <- as.vector(img)
  b <- sapply(cv, brightness)
  g <- ramp(b)
  a <- substr(cv, 8,9)     # get alpha values
  ga <- paste0(g, a)       # add alpha values to new colors
  img.gray <- matrix(ga, nrow(img), ncol(img), byrow = TRUE)  
}