library(proxy)
library(recommenderlab)
library(reshape2)

# Data files
books <- read.csv("dataset/books.csv", header = TRUE, stringsAsFactors = FALSE)
ratings <- read.csv("dataset/ratingsBooks.csv", header = TRUE)
booksScores <- read.csv("dataset/booksScores.csv", header = TRUE)
newBooks <- books[-which((books$bookId %in% ratings$bookId) == FALSE), ]

# Function to get the books recommendation
bookRecommendation <- function(input) {
  rowNum <- which(newBooks[, 2] == input)
  userSelect <- matrix(NA, 200)
  userSelect[rowNum] <- 2
  userSelect <- t(userSelect)
  
  ratingmat <- dcast(ratings, userId~bookId, value.var = "rating", na.rm = FALSE)
  ratingmat <- ratingmat[, -1]
  colnames(userSelect) <- colnames(ratingmat)
  newRatingmat <- rbind(userSelect, ratingmat)
  newRatingmat <- as.matrix(newRatingmat)
  newRatingmat <- as(newRatingmat, "realRatingMatrix")
  
  recommenderModel <- Recommender(
    newRatingmat,
    method = "UBCF",
    param = list(
      method = "Cosine",
      nn = 30
    )
  )
  recom <- predict(recommenderModel, newRatingmat[1], n = 10)
  recomList <- as(recom, "list")
  noResult <- data.frame(matrix(NA, 1))
  recomResult <- data.frame(matrix(NA, 10))
  
  if (as.character(recomList[1]) == 'character(0)'){
    noResult[1, 1] <- "Sorry, there is not enough information in our database on the books you've selected. Try to select different books you like."
    
    colName <- c("No results", "")
    colnames(noResult) <- colName
    
    return(noResult) 
  } else {
    for (i in c(0:9)) {
      recomResult[i,1] <- as.character(
        subset(books, books$bookId == as.integer(recomList[[1]][i + 1]))$title
      )
      
      maxScore = maxScore()
      recomResult[i,2] <- as.character(
        subset(booksScores, booksScores$bookId == as.integer(recomList[[1]][i + 1]))$Score / maxScore * 10 / 2
      )
    }
    
    colName <- c("Title", "Scores")
    colnames(recomResult) <- colName
    return(recomResult)
  }
}

# Function to get the maximun score of the books
maxScore <- function() {
  maxScore = booksScores[1, 2]
  
  for(i in 1:nrow(booksScores)){
    if(maxScore < booksScores[i, 2]) {
      maxScore = booksScores[i, 2]
    }
  }
  
  return(maxScore)
}
