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

shinyUI(htmlTemplate(
  "www/startbootstrap-freelancer-gh-pages/index.html",
  title = "Recommender System",
  content = tabsetPanel(
    tabPanel(
      "Doc",
      style = "margin-top: 30px",
      includeMarkdown('README.md')
    ),
    
    tabPanel(
      "Recommender",
      style = "margin-top: 30px",
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
          img(src="images/books.jpg", width="50%")
        ),
          
        column(
          12,
          h3("Recommended books:"),
          tableOutput("table")
        )
      )
    )
  )
))