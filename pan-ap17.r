install.packages("XML")
install.packages("qdap")
install.packages("tm")
install.packages("splitstackshape")
install.packages("caret")
install.packages("kernlab")
install.packages("stringi")
install.packages("randomForest")

library(stringi)
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(kernlab)
library(caret)
library(randomForest)

GenerateVocabulary <- function(path, n = 2000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE, sw = NULL) {
  setwd(path)
  
  files = list.files(pattern="*.xml")
  
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))       
  }
  
  corpus.preprocessed <- corpus.raw
  
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }       
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")               
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")        {
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  if (swlang!="") {
    if (verbose) print("Eliminando los acentos...")
    corpus.preprocessed <- stri_trans_general(corpus.preprocessed,"Latin-ASCII")
  }
  
  
  corpus.preprocessed <- removeWords(corpus.preprocessed, sw)
  
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

GenerateBoW <- function(path, vocabulary, n = 2000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
  setwd(path)
  
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  files = list.files(pattern="*.xml")
  for (file in files) {
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      if (verbose) print("Eliminando los acentos...")
      txtdata <- stri_trans_general(txtdata,"Latin-ASCII")
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      }
      line <- paste(line, ",", thefreq, sep="")
    }
    if (class=="variety") {
      #line <- paste(line, ",", variety,  sep="")
      line <- paste(variety, ",", line,  sep="")
    } else {
      #line <- paste(line, ",", gender, sep="")
      line <- paste(gender, ",", line,  sep="")
    }
    
    
    bow <- rbind(bow, line)
    
    i <- i + 1
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}

n <- 2000
path_training <- "/home/masango3@alumno.upv.es/Escritorio/text/pan-ap17-bigdata/training"        # Your training path
path_test <- "/home/masango3@alumno.upv.es/Escritorio/text/pan-ap17-bigdata/test"                        # Your test path
sw <- readLines("/home/masango3@alumno.upv.es/Escritorio/text/pan-ap17-bigdata/stopwords.es.txt",encoding="UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")

vocabulary <- GenerateVocabulary(path_training, n, swlang="es", sw = sw)


bow_training <- GenerateBoW(path_training, vocabulary, n, class="variety")
bow_test <- GenerateBoW(path_test, vocabulary, n, class="variety")

training <- concat.split(bow_training, "V1", ",")
test <- concat.split(bow_test, "V1", ",")

training.prep <- training[,-1]
training.prep <- training.prep[,-2]
names(training.prep)[1] <- "class"

test.prep <- test[,c(-1,-2,-3)]

set.seed(123)
train_control <- trainControl(method="none")
model_SVM <- train( class~., data= training.prep, trControl = train_control,  ntree =100,method = "rf")
print(model_SVM)

truth  <- unlist(test[,2])

pred_SVM <- predict(model_SVM, test.prep)
confusionMatrix(pred_SVM, truth)