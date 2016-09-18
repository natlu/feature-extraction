library(tm)
library(Rmpfr)
library(XML)
library(SnowballC)
library(topicmodels)
library(stringi)
library(stringr)

# Run getdatafortrainandtext.R or read in data elsewhere
dat <- readRDS("Ancillary/dat2.rds")

# add in the xml file links
dat[, xmlFiles := str_replace(pdfFiles, ".pdf$", ".xml")]

# system.time(y <- cbind(dat, textExtract.run(dat$xmlFiles)))
# y <- cbind(dat[1:10], textExtract.run(dat$xmlFiles[1:10]))

# Prepare text for corpus > get from xml
# textDoc <- lapply(dat$xmlFiles, getText)
# Get something a little (much) more computationally friendly
textDoc <- lapply(dat$xmlFiles[1:10], getText)
textDoc <- lapply(textDoc, tolower)


# Create corpus. This is going to be used for the LDA model
# Note: by this step, should have already cleaned most of
# the words. This includes punctuations, numeric etc.
corp <- Corpus(VectorSource(textDoc))
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp , stemDocument)


# Document term matrix
# word is between 4 and 20 characters
# word appears in at least 3 documents
DTM_control <- list(wordLengths = c(4, 20),
                    bounds = list(global = c(3,Inf)))
dtm <- DocumentTermMatrix(corp, control = DTM_control)
# arbitrary sparse argument
dtms <- removeSparseTerms(dtm, 0.95)

# Removing empty rows/documents
rowTotals <- apply(dtms, 1, sum)
dtm.new <- dtms[rowTotals > 0, ]

# Adjusting the corpus for the rows removed
empty.rows <- dtms[rowTotals == 0, ]$dimnames[1][[1]]
corp <- corp[-as.numeric(empty.rows)]

#------- start optimising k via harmonic means ---------------------------------
# generate numerous topic models with varying k
seqk <- seq(5, 15, by = 1) # Takes a hella'va long time for a few values of k
system.time(fitted_many <- lapply(seqk, genLDA))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks)

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
plot(seqk, hm_many, type = "l")

# Determine the optimum number of topics
optk <- seqk[which.max(hm_many)]
#------- End optimising k via harmonic means -----------------------------------

# fit model with calculated optimum k
lda_fitted <- genLDA(optk)

### CODE FOR PREDICTION ###
# apply(posterior(lda_fitted, TEST_DAT)$topics, 1 ,which.max)

################################################################################
#------- Functions / Scripts ---------------------------------------------------
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




#' wrapper function for textExtractin process
#' @export
textExtract.run <- function(xmlLink) {
  textDoc <- lapply(xmlLink, getText)
  textDoc <- lapply(textDoc, tolower)
  
  cat("textExtract.basic: ", "started...")
  basicTable <- textExtract.basic(textDoc)
  cat("finished", "\n")
  
  textDoc %>%
    lapply(str_replace_all, "[\\/\\,\\.\\!\\?\\-\\_\\|\\:\\\\]+", " ") %>% 
    lapply(removeNumbers) %>% 
    lapply(removePunctuation) %>% 
    lapply(removeWords, stopwords("english")) %>% 
    lapply(str_replace_all, "\\b[a-zA-Z]\\b", " ") %>%
    lapply(superTrim) -> textDoc
  
  cat("textExtract.alphabet: ", "started...")
  alphabetTable <- textExtract.alphabet(textDoc)
  cat("finished", "\n")
  
  outTable <- cbind(basicTable, alphabetTable)
  return(outTable)
}

#' Gets text from XML files
#' @import XML
#' @import stringi
#' @import stringr
#' @export
getText <- function(xmlLink) {
  cat("...", str_sub(xmlLink, -70), "\n")
  
  xmlAsText <- suppressWarnings(readLines(xmlLink))
  xmlAsText[[2]] <- '<document version="1.0">'
  x <- xmlParse(xmlAsText)
  
  textDoc <- paste(unlist(xpathApply(x, "//formatting", xmlValue)), collapse = " ")
  textDoc <- stri_trans_general(textDoc, "Latin-ASCII") # transliterate the non-ASCII characters
  # textDoc <- iconv(textDoc, Encoding(textDoc), "ASCII", sub = "") # Remove non ascii
  textDoc <- iconv(textDoc, to = "ASCII", sub = "") # Remove non ascii
  textDoc <- superTrim(textDoc)
  return(textDoc)
}

#' Extract basic text features that might not be captured by models (LDA, RBM)
#' @import stringr
#' @import tm
#' @import handy
#' @import data.table
#' @export
textExtract.basic <- function(textDoc) {
  # Get number of strings in brackets
  # Note: Might do a classification on just bracketed words
  # !!! Need to add in different types on parentheses
  parentheses <- str_extract_all(textDoc, "(?<=\\().*?(?=\\))")
  parentheses_count <- sapply(parentheses, length)
  
  # Get number of [english] stop words
  stopWord <- c(stopwords(), str_replace_all(stopwords(), "[[:punct:]]", ""))
  stopWord <- unique(stopWord)
  stopWord_regex <- paste0("\\b", stopWord, "\\b")
  stopWord_regex <- paste(stopWord_regex, collapse = "|")
  stopwords_en <- str_extract_all(textDoc, stopWord_regex)
  stopwords_en_count <- sapply(stopwords_en, length)
  
  # Get numerics
  numerics <- str_extract_all(textDoc, "[0-9]+")
  numerics_count <- sapply(numerics, length)
  
  # Get (month) dates
  allDates <- tolower(c(month.name, month.abb))
  allDates_regex <- paste0("\\b", allDates, "\\b")
  allDates_regex <- paste(allDates_regex, collapse = "|")
  monthDates <- str_extract_all(textDoc, allDates_regex)
  monthDates_count <- sapply(monthDates, length)
  
  # Get standalone letters
  # Get rid of standalone 'a' and 'i'
  standalone_char <- str_extract_all(textDoc, "\\b[a-zA-Z]\\b")
  standalone_char <- lapply(standalone_char, function(x) x[grep("[^ai]", x)])
  standalone_char_count <- sapply(standalone_char, length)
  
  # Get questions marks
  questionMark <- str_extract_all(textDoc, "\\?")
  questionMark_count <- sapply(questionMark, length)
  
  # Get emails (for common gTLDs)
  gTLD <- c("com", "org", "net", "edu", "gov")
  email_regex <- sprintf("[[:alnum:].-]+@[[:alnum:]]+.(%s)(.[[:alnum:]]+|)",
                         paste(gTLD, collapse = "|"))
  emails <- str_extract_all(textDoc, email_regex)
  emails_count <- sapply(emails, length)
  
  basicTable <- 
    data.table(numParentheses  = parentheses_count,
               numStopWords    = stopwords_en_count,
               numNumerics     = numerics_count,
               numMonths       = monthDates_count,
               numStandalone   = standalone_char_count,
               numQuestionMark = questionMark_count,
               numEmail        = emails_count)
  
  ### May remove features generated after this message
  dirtyAlphabet <- textExtract.alphabet(textDoc)
  setnames(dirtyAlphabet, old = names(dirtyAlphabet), new = paste0("dirty_", names(dirtyAlphabet)))
  
  outTable <- cbind(basicTable, dirtyAlphabet)
  return(outTable)
}

#' Extract features on the properties of words
#' @import stringr
#' @import tm
#' @import magrittr
#' @import handy
#' @export
textExtract.alphabet <- function(textDoc) {
  # Word count
  numWords <- sapply(gregexpr("[[:alpha:]]+", textDoc), function(x) sum(x > 0))
  
  # Total number of characters (excl. spacing, numerics, punctuation)
  numChar_total <- nchar(str_replace_all(textDoc, " ", ""))
  
  # Average characters per word
  meanChar <- numChar_total/numWords
  
  # Prep for word length metrics
  textDoc %>% 
    lapply(function(x) unlist(strsplit(x, " "))) %>% 
    lapply(nchar) -> wordLengths
  # "Longest" word (using 85% percentile rather than true maximum)
  longestWord <- sapply(wordLengths, function(x) quantile(x, 0.85))
  # Average length (trimmed mean with trim = 0.1)
  meanLength <- sapply(wordLengths, function(x) mean(x, 0.1))
  
  # Letter frequency
  letterCount <- function(x) {
    charSplit <- strsplit(tolower(x), "")[[1]]
    charSplit <- charSplit[which(charSplit %in% letters)]
    return(table(c(letters, charSplit)) - 1)}
    
  textDoc %>% 
    sapply(letterCount) %>%
    t %>% 
    data.table -> letterFreq
  
  # Worth length interval counts
  intervalLabels <- paste0(as.character(seq(0,20,by=4)), "-") # 20 represents 20+
  ncharInterval <- function(x) table(cut(x,breaks=c(seq(0,20,by=4), Inf), labels = intervalLabels))
  wordIntervalLength <- t(sapply(wordLengths, ncharInterval))
  
  charTable <-
    data.table(numWords = numWords,
               totalChar = numChar_total,
               maxChar = longestWord,
               meanChar = meanLength)
  outTable <- cbind(charTable, letterFreq, wordIntervalLength)
  
  return(outTable)
}