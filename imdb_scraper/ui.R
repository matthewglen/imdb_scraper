library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("IMDB Ratings"),

    textInput("series_name",
              "Series name:",
              value = "",
              width = NULL,
              placeholder = NULL),
    submitButton("Update View", icon("refresh")),
    
    plotlyOutput("plot", height = "75%")
    )
)
