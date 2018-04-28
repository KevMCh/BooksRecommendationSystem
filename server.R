source("recommender.R")

search <- read.csv("dataset/search.csv", stringsAsFactors = FALSE)
ratings <- read.csv("dataset/ratingsBooks.csv", header = TRUE)
search <- search[-which((search$bookId %in% ratings$bookId) == FALSE), ]

formatInput <- function(v, a){  
  c(v, a)
}

shinyServer(function(input, output) {
  output$ui <- renderUI({
    if (is.null(input$inputGenre))
      return()
    
    # Genre Selector
    switch(input$inputGenre,
           "Adventure" = selectInput("select", "Book of Genre",
                                     choices = sort(subset(search, Adventure == 1)$title),
                                     selected = sort(subset(search, Adventure == 1)$title)[1]),
           "Romantica" =  selectInput("select", "Book of Genre",
                                      choices = sort(subset(search, Romantica == 1)$title),
                                      selected = sort(subset(search, Romantica == 1)$title)[1]),
           "Youth" =  selectInput("select", "Book of Genre",
                                  choices = sort(subset(search, Youth == 1)$title),
                                  selected = sort(subset(search, Youth == 1)$title)[1]),
           "History" =  selectInput("select", "Book of Genre",
                                    choices = sort(subset(search, History == 1)$title),
                                    selected = sort(subset(search, History == 1)$title)[1]),
           "Narrative" =  selectInput("select", "Book of Genre",
                                      choices = sort(subset(search, Narrative == 1)$title),
                                      selected = sort(subset(search, Narrative == 1)$title)[1]),
           "Theater" =  selectInput("select", "Book of Genre",
                                    choices = sort(subset(search, Theater == 1)$title),
                                    selected = sort(subset(search, Theater == 1)$title)[1]),
           "Comics" =  selectInput("select", "Book of Genre",
                                   choices = sort(subset(search, Comics == 1)$title),
                                   selected = sort(subset(search, Comics == 1)$title)[1]),
           "Terror" =  selectInput("select", "Book of Genre",
                                   choices = sort(subset(search, Terror == 1)$title),
                                   selected = sort(subset(search, Terror == 1)$title)[1]),
           "Poetry" =  selectInput("select", "Book of Genre",
                                   choices = sort(subset(search, Poetry == 1)$title),
                                   selected = sort(subset(search, Poetry == 1)$title)[1]),
           "Horror" =  selectInput("select", "Book of Genre",
                                   choices = sort(subset(search, Horror == 1)$title),
                                   selected = sort(subset(search, Horror == 1)$title)[1]),
           "Classics" =  selectInput("select", "Book of Genre",
                                     choices = sort(subset(search, Classics == 1)$title),
                                     selected = sort(subset(search, Classics == 1)$title)[1]),
           "ScienceFiction" =  selectInput("select", "Book of Genre",
                                           choices = sort(subset(search, ScienceFiction == 1)$title),
                                           selected = sort(subset(search, ScienceFiction == 1)$title)[1])
    )
  })
  
  # Recommendation table
  output$table <- renderTable({
    bookRecommendation(input$select)
  })
  
  # Print
  output$dynamicValue <- renderPrint({
    c(input$select,input$select2)
  })
})
