source("recommender.R")

search <- read.csv("dataset/search.csv", stringsAsFactors = FALSE)
ratings <- read.csv("dataset/ratingsBooks.csv", header = TRUE)
search <- search[-which((search$bookId %in% ratings$bookId) == FALSE), ]

formatInput <- function(v, a){
  c(v, a)
}

shinyServer(function(input, output, session) {
  observe({
    if (is.null(input$inputGenre))
      return()
    
    listBooks <- c()
    
    # Genre Selector
    switch(input$inputGenre,
           "Adventure" = listBooks <- sort(subset(search, Adventure == 1)$title),
           "Romantica" =  listBooks <- sort(subset(search, Romantica == 1)$title),
           "Youth" = listBooks <- sort(subset(search, Youth == 1)$title),
           "History" =  listBooks <- sort(subset(search, History == 1)$title),
           "Narrative" = listBooks <- sort(subset(search, Narrative == 1)$title),
           "Theater" = listBooks <- sort(subset(search, Theater == 1)$title),
           "Comics" = listBooks <- sort(subset(search, Comics == 1)$title),
           "Terror" = sort(subset(search, Terror == 1)$title),
           "Poetry" = listBooks <- sort(subset(search, Poetry == 1)$title),
           "Horror" = listBooks <- sort(subset(search, Horror == 1)$title),
           "Classics" = listBooks <- sort(subset(search, Classics == 1)$title),
           "ScienceFiction" = listBooks <- sort(subset(search, ScienceFiction == 1)$title)
    )
    
    updateSelectInput(
      session,
      "inputBooks",
      choices = listBooks
    )
  })
  
  # Recommendation table
  output$table <- renderTable({
    if(isTruthy(input$inputBooks)) {
      bookRecommendation(input$inputBooks)
    }
  })
})
