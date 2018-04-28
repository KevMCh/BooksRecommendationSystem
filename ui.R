# List of genres
genreList <- c("Adventure",
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

shinyUI(fluidPage(
  includeMarkdown('README.md'),
  
  fluidRow(
    column(
      3,
      h3("Select Book Genres You Prefer:"),
      wellPanel(
        selectInput("inputGenre", "Genre",
                    genreList)
      )
    ),
    
    column(
      6,
      h3("Select Books You Like of these Genres:"),
      wellPanel(
        uiOutput("ui")
      )
    ),
    
    column(
      3,
      img(src="books.jpg", width="50%")
    ),
    
    column(
      12,
      h3("Recommended books:"),
      tableOutput("table")
    )
  )
))