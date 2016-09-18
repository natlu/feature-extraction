#' getTextFeatures
#'
#' @import data.table
#' @import handy
#' @import magrittr
#' @export
#' @docType methods
#' @rdname getTextFeatures-methods
setGeneric(name="getTextFeatures",
           def=function(feature_obj)
           {
             standardGeneric("getTextFeatures")
           }
)

#' @rdname getTextFeatures-methods
#' @aliases getTextFeatures,ANY,ANY-method
setMethod(f="getTextFeatures",
          signature="foo_class",
          definition=function(feature_obj)
          {
            textFeatures <- feature_obj@dat[, .(idx, xmlFiles)]

            xmlText <- lapply(textFeatures$xmlFiles, getText)

            # Get percentage of lower vs upper case

            xmlText <- lapply(xmlText, tolower)

            return(feature_obj)
          }
)


# Function to find which submatrices have something in it ----------------------

#' Gets text from XML files
#' @import XML
#' @import stringi
#' @export
getText <- function(xmlLink) {
  xmlAsText <- suppressWarnings(readLines(xmlLink))
  xmlAsText[[2]] <- '<document version="1.0">'
  xmlDat <- xmlParse(xmlAsText)

  textDoc <- paste(unlist(xpathApply(xmlDat, "//formatting", xmlValue)), collapse = " ")
  textDoc <- stri_trans_general(textDoc, "Latin-ASCII") # transliterate the non-ASCII characters
  # textDoc <- iconv(textDoc, Encoding(textDoc), "ASCII", sub = "") # Remove non ascii
  textDoc <- iconv(textDoc, to = "ASCII", sub = "") # Remove non ascii
  textDoc <- superTrim(textDoc)
  return(textDoc)
}

#' Extract basic text features that might not be captured by models (LDA, RBM)
#' This includes features that are removed in the cleaning process list punctuation
#' @import stringr
#' @import tm
#' @import handy
#' @export
textExtract.RawBasic <- function(xmlText) {
  # Need total number of words

  # Get number of strings that are in brackets
  # May also get the number of words in brackets
  # Should I clean text first because there are punctuations inside brackets
  # Also need to add in different types on parentheses
  parentheses <- str_extract_all(xmlText, "(?<=\\().*?(?=\\))")
  parentheses_count <- sapply(parentheses, length)
  parentheses <- lapply(parentheses, removePunctuation)
  # Do not know why it is x > 1 here. Just works out.
  sapply(gregexpr("[[:alpha:]]+", parentheses), function(x) sum(x > 1))

  # Get number of [english] stop words
  stopWord <- c(stopwords(), str_replace_all(stopwords(), "[[:punct:]]", ""))
  stopWord <- unique(stopWord)
  stopWord_regex <- paste0("\\b", stopWord, "\\b")
  stopWord_regex <- paste(stopWord_regex, collapse = "|")
  stopwords_en <- str_extract_all(xmlText, stopWord_regex)
  stopwords_en_count <- sapply(stopwords_en, length)

  # Get numerics
  numerics <- str_extract_all(xmlText, "[0-9]+")
  numerics_count <- sapply(numerics, length)
  # Could get numerics of length 4 as this could be years

  # Get (month) dates
  allDates <- tolower(c(month.name, month.abb))
  allDates_regex <- paste0("\\b", allDates, "\\b")
  allDates_regex <- paste(allDates_regex, collapse = "|")
  monthDates <- str_extract_all(xmlText, allDates_regex)
  monthDates_count <- sapply(monthDates, length)

  # Get stand alone letters
  standalone_char <- str_extract_all(xmlText, "\\b[a-zA-Z]\\b")
  standalone_char_count <- sapply(standalone_char, length)
  # Stand alone letters that aren't I and A is another option

  # Get questions marks
  questionMark <- str_extract_all(xmlText, "\\?")
  questionMark_count <- sapply(questionMark, length)

  # Get emails (for common gTLDs)
  gTLD <- c("com", "org", "net", "edu", "gov")
  email_regex <- sprintf("[[:alnum:].-]+@[[:alnum:]]+.(%s)(.[[:alnum:]]+|)",
                         paste(gTLD, collapse = "|"))
  emails <- str_extract_all(xmlText, email_regex)
  emails_count <- sapply(emails, length)

  ### Features to compare with textExtract.CleanBasic
  # Word count
  # Total number of characters (excl. spacing, numerics, punctuation)
  # "Longest" word (use 0.8 rather than 1 for "max" length)
  # Average length
  # Letter frequency
  # Worth length interval counts
}


#' Extract features on the properties of words
#' @import stringr
#' @import tm
#' @import magrittr
#' @import handy
#' @export
textExtract.alphabet <- function(textDoc) {
  textDoc %>%
    lapply(str_replace_all, "[\\/\\,\\.\\!\\?\\-\\_\\|\\\\]+", " ") %>%
    lapply(removeNumbers) %>%
    lapply(removePunctuation) %>%
    lapply(removeWords, stopwords("english")) %>%
    lapply(str_replace_all, "\\b[a-zA-Z]\\b", " ") %>%
    lapply(superTrim) -> foo # Also need tolower

  # Word count
  numWords <- sapply(gregexpr("[[:alpha:]]+", foo), function(x) sum(x > 0))

  # Total number of characters (excl. spacing, numerics, punctuation)
  numChar_total <- nchar(str_replace_all(foo, " ", ""))

  # Average characters per word
  meanChar <- numChar_total/numWords

  # Prep for word length metrics
  foo %>%
    lapply(function(x) unlist(strsplit(x, " "))) %>%
    lapply(nchar) -> wordLengths
  # "Longest" word (using 85% percentile rather than true maximum)
  longestWord <- sapply(wordLengths, function(x) quantile(x, 0.85))
  # Average length (trimmed mean with trim = 0.1)
  meanLength <- sapply(wordLengths, function(x) mean(x, 0.1))

  # Letter frequency
  letterCount <- function(X) table(c(letters, strsplit(tolower(X), "")[[1]])) - 1
  foo %>%
    sapply(letterCount) %>%
    t %>%
    data.table -> letterFreq

  # Worth length interval counts
  intervalLabels <- paste(as.character(seq(0,20,by=4))) # 20 represents 20+
  ncharInterval <- function(x) table(cut(x,breaks=c(seq(0,20,by=4), Inf), labels = intervalLabels))
  wordIntervalLength <- t(sapply(wordLengths, ncharInterval))
}
