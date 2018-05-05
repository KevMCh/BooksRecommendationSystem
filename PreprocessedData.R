library(recommenderlab)
library(ggplot2)
install.packages("data.table")
library(reshape2)
library(data.table)

GENRE.LIST <- c(
  "Adventure",
  "Romantica",
  "Youth",
  "History",
  "Narrative",
  "Theater",
  "Comics",
  "Terror",
  "Poetry",
  "Classics",
  "ScienceFiction"
)

# Get the genres.
GetGenre <- function() {
  genre.matrix <- matrix(0, 201, 11)
  
  genre.matrix[1, ] <- GENRE.LIST
  
  for (i in 1:nrow(genres)) {
    for (c in 1:ncol(genres)) {
      gen.mat.col = which(genre.matrix[1, ] == genres[i, c])
      genre.matrix[i + 1, gen.mat.col] <- 1
    }
  }
  
  new.genre.matrix <- as.data.frame(genre.matrix[-1, ], stringsAsFactors=FALSE)
  
  for (c in 1:ncol(new.genre.matrix)) {
    new.genre.matrix[, c] <- as.integer(new.genre.matrix[, c])
  }
  
  search.matrix <- cbind(books[, 1], books[, 2], new.genre.matrix)
  colnames(search.matrix) <- c("bookId", "title", GENRE.LIST)
  
  write.csv(search.matrix, "dataset/search.csv")
  
  search.matrix <- read.csv("dataset/search.csv", stringsAsFactors=FALSE)
  
  subset(search.matrix, Adventure == 1)$title
  
  new.genre.matrix
}

# Creating a user profile
CreateUserProfile <- function(ratings, genre.matrix) {
  binary.ratings <- ratings
  
  new.binary.ratings <- dcast(binary.ratings, bookId~userId, value.var = "rating", na.rm = FALSE)
  
  for (i in 1:ncol(new.binary.ratings)){
    new.binary.ratings[which(is.na(new.binary.ratings[, i]) == TRUE), i] <- 0
  }
  
  new.binary.ratings = new.binary.ratings[, -1]
  
  bookIds <- length(unique(books$bookId)) #200
  
  rating.book.ids <- length(unique(ratings$bookId)) #200
  
  new.books <- books[-which((books$bookId %in% ratings$bookId) == FALSE),]
  rownames(new.books) <- NULL
  
  final.genre.matrix <- genre.matrix[-which((books$bookId %in% ratings$bookId) == FALSE), ]
  rownames(final.genre.matrix) <- NULL
  
  result = matrix(0, 11, 800) # 11 = número de géneros, 800 = número de usuarios valoradores, 
  for (c in 1:ncol(new.binary.ratings)){
    for (i in 1:ncol(final.genre.matrix)){
      result[i, c] <- sum((final.genre.matrix[,i]) * (new.binary.ratings[,c]))
    }
  }
  
  rating.mat <- dcast(ratings, userId~bookId, value.var = "rating", na.rm=FALSE)
  rating.mat <- as.matrix(rating.mat[, -1])
  rating.mat <- as(rating.mat, "realRatingMatrix")
  
  rating.mat
}

# Nearest neighbors
NearestNeightBors <- function (rating.mat) {
  rating.mat <- as(rating.mat, "realRatingMatrix")
  
  similarity.users <- similarity(rating.mat[1:4, ],
                                 method = "cosine", 
                                 which = "users")
  
  as.matrix(similarity.users)
  
  image(as.matrix(similarity.users))
  
  similarity.items <- similarity(rating.mat[, 1:4], method =
                                   "cosine", which = "items")
  as.matrix(similarity.items)
  image(as.matrix(similarity.items), main = "Item similarity")
  
  vector.ratings <- as.vector(rating.mat@data)
  unique(vector.ratings)
  
  table.ratings <- table(vector.ratings)
  table.ratings
  
  vector.ratings <- vector.ratings[vector.ratings != 0]
  vector.ratings <- factor(vector.ratings)
  
  qplot(vector.ratings) + ggtitle("Distribution of the ratings")
  
  readings.per.book <- colCounts(rating.mat)
  
  table.readings <- data.frame(book = names(readings.per.book),
                               readings = readings.per.book)
  
  table.readings <- table.readings[order(table.readings$readings,
                                         decreasing = TRUE), ]
  
  ggplot(table.readings[1:6, ],
    aes(x = book, y = readings)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("Number of readings of the top books")
  
  rating.mat
}

# Normalizing the data
NormalizeData <- function(rating.mat) {
  rating.mat.norm <- normalize(rating.mat)
  
  image(rating.mat[rowCounts(rating.mat) > quantile(rowCounts(rating.mat), 0.99),
                   colCounts(rating.mat) > quantile(colCounts(rating.mat), 0.99)], 
        main = "Heatmap of the top users and books")
  
  rating.mat.norm
}

# Creating the recommendatory model
CreateRecommendatoryModel <- function(rating.mat.norm) {
  recommender.model <- Recommender(rating.mat.norm, 
                                   method = "UBCF", 
                                   param = list(method = "Cosine", nn = 30))
  
  model.details <- getModel(recommender.model)
  model.details$data
  
  recom <- predict(recommender.model, 
                   rating.mat[1], 
                   n = 10)
  recom
  recom.list <- as(recom, "list")
  
  recom.result <- matrix(0, 10)
  for (i in 1:10){
    recom.result[i] <- as.character(
      subset(books, books$bookId == as.integer(recom.list[[1]][i]))$title)
  }
  
  recom.result
}

# Scores ranking
CreateScoresRanking <- function(ratings) {
  books.scores <- matrix(0, 200, 2)
  
  colnames(books.scores) <- c("bookId", "Score")
  
  for(i in 1:nrow(books)){
    books.scores[i, 1] <- books[i, 1]
    for(j in 1: nrow(ratings)) {
      if(books[i, 1] == ratings[j, 2]){
        books.scores[i, 2] = (ratings[j, 3] + books.scores[i, 2])
      }
    }
  }
  
  write.csv(books.scores, "dataset/booksScores.csv")
}

Main <- function() {
  books <- read.csv("./dataset/books.csv", stringsAsFactors = FALSE)
  genres <- as.data.frame(books$genre, stringsAsFactors = FALSE)
  
  genre.matrix <- GetGenre()
  
  ratings <- read.csv("dataset/ratingsBooks.csv", stringsAsFactors = FALSE)
  rating.mat <- CreateUserProfile(ratings, genre.matrix)
  
  NearestNeightBors(rating.mat)
  
  rating.mat.norm <- NormalizeData(rating.mat)
  
  CreateRecommendatoryModel(rating.mat.norm)
  
  CreateScoresRanking(ratings)
}

Main()