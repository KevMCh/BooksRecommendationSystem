books <- read.csv("./dataset/books.csv", stringsAsFactors = FALSE)

library(recommenderlab)
library(ggplot2)
install.packages("data.table")
library(data.table)

genres <- as.data.frame(books$genre, stringsAsFactors = FALSE)

genreList <- c(
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

genreMatrix <- matrix(0, 201, 11)

genreMatrix[1, ] <- genreList

for (i in 1:nrow(genres)) {
  for (c in 1:ncol(genres)) {
    genMatCol = which(genreMatrix[1, ] == genres[i, c])
    genreMatrix[i + 1, genMatCol] <- 1
  }
}

newGenreMatrix <- as.data.frame(genreMatrix[-1, ], stringsAsFactors=FALSE)

for (c in 1:ncol(newGenreMatrix)) {
  newGenreMatrix[, c] <- as.integer(newGenreMatrix[, c])
}

searchMatrix <- cbind(books[, 1], books[, 2], newGenreMatrix)
colnames(searchMatrix) <- c("bookId", "title", genreList)

write.csv(searchMatrix, "dataset/search.csv")

searchMatrix <- read.csv("dataset/search.csv", stringsAsFactors=FALSE)

subset(searchMatrix, Adventure == 1)$title


# Creating a user profile
ratings <- read.csv("dataset/ratingsBooks.csv", stringsAsFactors = FALSE)

binaryRatings <- ratings

library(reshape2)

newBinaryRatings <- dcast(binaryRatings, bookId~userId, value.var = "rating", na.rm = FALSE)

for (i in 1:ncol(newBinaryRatings)){
  newBinaryRatings[which(is.na(newBinaryRatings[, i]) == TRUE), i] <- 0
}

newBinaryRatings = newBinaryRatings[, -1]

bookIds <- length(unique(books$bookId)) #200

ratingbookIds <- length(unique(ratings$bookId)) #200

newBooks <- books[-which((books$bookId %in% ratings$bookId) == FALSE),]
rownames(newBooks) <- NULL

finalGenreMatrix <- newGenreMatrix[-which((books$bookId %in% ratings$bookId) == FALSE), ]
rownames(finalGenreMatrix) <- NULL

result = matrix(0, 11, 800) # 11 = número de géneros, 800 = número de usuarios valoradores, 
for (c in 1:ncol(newBinaryRatings)){
  for (i in 1:ncol(finalGenreMatrix)){
    result[i,c] <- sum((finalGenreMatrix[,i]) * (newBinaryRatings[,c]))
  }
}

ratingmat <- dcast(ratings, userId~bookId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[, -1])

# Nearest neighbors
ratingmat <- as(ratingmat, "realRatingMatrix")

similarityUsers <- similarity(ratingmat[1:4, ],
                              method = "cosine", 
                              which = "users")

as.matrix(similarityUsers)

image(as.matrix(similarityUsers))

similarityItems <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarityItems)
image(as.matrix(similarityItems), main = "Item similarity")

vectorRatings <- as.vector(ratingmat@data)
unique(vectorRatings)

tableRatings <- table(vectorRatings)
tableRatings

vectorRatings <- vectorRatings[vectorRatings != 0]
vectorRatings <- factor(vectorRatings)

qplot(vectorRatings) + ggtitle("Distribution of the ratings")

readingsPerBook <- colCounts(ratingmat)

tableReadings <- data.frame(book = names(readingsPerBook),
                            readings = readingsPerBook)

tableReadings <- tableReadings[order(tableReadings$readings,
                                     decreasing = TRUE), ]


ggplot(tableReadings[1:6, ],
       aes(x = book, y = readings)) +
  geomBar(stat="identity") + 
  theme(axis.text.x = elementText(angle = 45, hjust = 1)) + 
  ggtitle("Number of readings of the top books")

# Normalizing the data
ratingmatNorm <- normalize(ratingmat)

image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)], 
      main = "Heatmap of the top users and books")

# Creating the recommendatory model
recommenderModel <- Recommender(ratingmatNorm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

modelDetails <- getModel(recommenderModel)
modelDetails$data

recom <- predict(recommenderModel, 
                 ratingmat[1], 
                 n=10)
recom
recomList <- as(recom, "list")

recomResult <- matrix(0,10)
for (i in 1:10){
  recomResult[i] <- as.character(
    subset(books, books$bookId == as.integer(recomList[[1]][i]))$title)
}

recomResult

# Scores ranking
booksScores <- matrix(0, 200, 2)

colnames(booksScores) <- c("bookId", "Score")

for(i in 1:nrow(books)){
  booksScores[i, 1] <- books[i, 1]
  for(j in 1: nrow(ratings)) {
    if(books[i, 1] == ratings[j, 2]){
      booksScores[i, 2] = (ratings[j, 3] + booksScores[i, 2])
    }
  }
}

write.csv(booksScores, "dataset/booksScores.csv")