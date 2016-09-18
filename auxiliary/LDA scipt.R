################################################################################
# LDA
################################################################################

#' LDA with arguments changes [from the default]
#' @import topicmodels
#' @export
genLDA <- function(k)
  LDA(dtm.new, k = k, method = "Gibbs", control = list(keep = 50))

#' function to find harmonic mean used for determining k, the number of topics
#' @import Rmpfr
#' @export
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  mpfrLLH <- mpfr(logLikelihoods, prec = precision)
  as.double(llMed - log(mean(exp(-mpfrLLH + llMed))))
}