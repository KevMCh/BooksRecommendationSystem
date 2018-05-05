library(proxy)
library(recommenderlab)
library(reshape2)

# Data files
books <- read.csv("dataset/books.csv", header = TRUE, stringsAsFactors = FALSE)
ratings <- read.csv("dataset/ratingsBooks.csv", header = TRUE)
books.scores <- read.csv("dataset/booksScores.csv", header = TRUE)
new.books <- books[-which((books$bookId %in% ratings$bookId) == FALSE), ]

# Get the user selection
UserSelect <- function(input) {
  row.num <- which(new.books[, 2] == input)
  user.select <- matrix(NA, 200)
  user.select[row.num] <- 2
  user.select <- t(user.select)
  
  user.select
}

# Calculate the rating
CalculateRating <- function(user.select, ratings) {
  rating.mat <- dcast(ratings, userId~bookId, value.var = "rating", na.rm = FALSE)
  rating.mat <- rating.mat[, -1]
  colnames(user.select) <- colnames(rating.mat)
  new.rating.mat <- rbind(user.select, rating.mat)
  new.rating.mat <- as.matrix(new.rating.mat)
  new.rating.mat <- as(new.rating.mat, "realRatingMatrix")
  
  new.rating.mat
}

# Get the solution
GetSolution <- function(recom.list, recom.result, no.result) {
  if (as.character(recom.list[1]) == 'character(0)') {
    no.result[1, 1] <- "Sorry, there is not enough information in our database on the books you've selected. Try to select different books you like."
    
    colName <- c("No results", "")
    colnames(no.result) <- colName
    
    return(no.result) 
  } else {
    for (i in c(0:9)) {
      recom.result[i, 1] <- as.character(
        subset(books, books$bookId == as.integer(recom.list[[1]][i + 1]))$title
      )
      
      max.score = max(books.scores$Score)
      recom.result[i, 2] <- as.character(
        subset(books.scores, books.scores$bookId == as.integer(recom.list[[1]][i + 1]))$Score / max.score * 10 / 2
      )
    }
    
    colName <- c("Title", "Scores")
    colnames(recom.result) <- colName
    return(recom.result)
  }
}

# Function to get the books recommendation
BookRecommendation <- function(input) {
  user.select <- UserSelect(input)
  
  rating.mat <- CalculateRating(user.select, ratings)
  
  recommender.model <- Recommender(
    rating.mat,
    method = "UBCF",
    param = list(
      method = "Cosine",
      nn = 30
    )
  )
  recom <- predict(recommender.model, rating.mat[1], n = 10)
  recom.list <- as(recom, "list")
  no.result <- data.frame(matrix(NA, 1))
  recom.result <- data.frame(matrix(NA, 10))
  
  GetSolution(recom.list, recom.result, no.result)
}
