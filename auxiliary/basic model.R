foo <-  data.table(numColumn = numeric(0),
                   hasText = vector('numeric'),
                   hasTable = vector('numeric'),
                   hasPicture = vector('numeric'),
                   areaPercent = numeric(0))

for(i in 1:nrow(dat)) {
  cat("\r", "completed: ", i, " remaining: ", nrow(dat) - i)
#   a <- paste(rep("-", i%%20), sep = "")
#   b <- paste(rep("-", 19 - i%%20), sep = "")
#   cat("\r", "Remaining: ", nrow(dat) - i, " ", a,">>", b,  " Completed: ", i, sep = "")
  foo <- rbindlist(list(foo, xmlExtract(gsub(".pdf$", ".xml", dat[i, pdfFiles]))))
}


dat <- cbind(dat, foo)


train <- sample(nrow(dat), floor(0.8 * nrow(dat)))

dat2 <-  dat[train, .(File.Size, Pages, Copies,
                      numColumn, hasText, hasTable,
                      hasPicture, areaPercent, `Published?`)]
setnames(dat2, "Published?", "pub")

dat3 <-  dat[-train, .(File.Size, Pages, Copies,
                       numColumn, hasText, hasTable,
                       hasPicture, areaPercent, `Published?`)]
setnames(dat3, "Published?", "pub")

# dat2[, hasText := ifelse(hasText == TRUE, 1, 0)]
# dat2[, hasText := as.factor(hasText)]
# dat2[, hasTable := ifelse(hasTable == TRUE, 1, 0)]
# dat2[, hasTable := as.factor(hasTable)]
# dat2[, hasPicture := ifelse(hasPicture == TRUE, 1, 0)]
# dat2[, hasPicture := as.factor(hasPicture)]

# initial svm param
svmfit <- svm(pub~., data = dat2,
              kernel = "radial", gamma = 1, cost = 1, class.weights = c("1" = 1.5, "2" = 1))

predictResults <- table(true = dat3[, pub], pred = predict(svmfit, new = dat3))
svmAccuracy <- (predictResults[1,1] + predictResults[2,2]) / sum(predictResults)


# tuning svm
# default as 10-fold cross validation
tuneCost <- c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 20, 30, 40, 50, 100)
tuneGamma <- c(0.1, 0.25, 0.5, 1, 1.5, 2, 3, 5, 7, 10, 13, 17, 25, 30, 50)
tuneWeights <- list(c("1" = 1, "2" = 1),
                    c("1" = 1.1, "2" = 1),
                    c("1" = 1.2, "2" = 1),
                    c("1" = 1.5, "2" = 1),
                    c("1" = 1.7, "2" = 1),
                    c("1" = 2, "2" = 1))
tunesvm<- tune(svm, pub~., data = dat2, kernel = "radial",
               ranges = list(cost = tuneCost, gamma = tuneGamma, class.weights = tuneWeights))

predictResults <- table(true = dat3[, pub], pred = predict(tunesvm$best.model, new = dat3))
svmAccuracy <- (predictResults[1,1] + predictResults[2,2]) / sum(predictResults)




tuneCost <- c(0.01, 0.1, 1, 5)
tuneGamma <- c(0.1, 0.25, 0.5, 1)
tunesvm<- tune(svm, pub~., data = dat2, kernel = "radial",
               ranges = list(cost = tuneCost, gamma = tuneGamma),
               tune = tune.control(nrepeat = 10))