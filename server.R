source("recommender.R")

search <- read.csv("dataset/search.csv", stringsAsFactors = FALSE)
ratings <- read.csv("dataset/ratingsBooks.csv", header = TRUE)
search <- search[-which((search$bookId %in% ratings$bookId) == FALSE), ]

shinyServer(function(input, output, session) {
  observe({
    if (is.null(input$inputGenre))
      return()
    
    list.books <- c()
    
    # Genre Selector
    switch(input$inputGenre,
           "Adventure" = list.books <- sort(subset(search, Adventure == 1)$title),
           "Romantica" =  list.books <- sort(subset(search, Romantica == 1)$title),
           "Youth" = list.books <- sort(subset(search, Youth == 1)$title),
           "History" =  list.books <- sort(subset(search, History == 1)$title),
           "Narrative" = list.books <- sort(subset(search, Narrative == 1)$title),
           "Theater" = list.books <- sort(subset(search, Theater == 1)$title),
           "Comics" = list.books <- sort(subset(search, Comics == 1)$title),
           "Terror" = sort(subset(search, Terror == 1)$title),
           "Poetry" = list.books <- sort(subset(search, Poetry == 1)$title),
           "Horror" = list.books <- sort(subset(search, Horror == 1)$title),
           "Classics" = list.books <- sort(subset(search, Classics == 1)$title),
           "ScienceFiction" = list.books <- sort(subset(search, ScienceFiction == 1)$title)
    )
    
    updateSelectInput(
      session,
      "inputBooks",
      choices = list.books
    )
  })
  
  # Recommendation table
  output$table <- renderTable({
    if(isTruthy(input$inputBooks)) {
      BookRecommendation(input$inputBooks)
    }
  })
})
