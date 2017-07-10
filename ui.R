#ui.R
library(shiny)
movies2 <- read.csv("ml-latest-small/movies.csv", header = TRUE, stringsAsFactors=FALSE)

shinyUI(fluidPage(
  titlePanel("Movie Recommendation Engine"),
  fluidRow(
    column(5,
           selectInput("select", label = h3("Choose Three Movies You Like"),
                       choices = as.character(movies2$title[1:1000])),
           
           selectInput("select2", label = NA,
                       choices = as.character(movies2$title[1:1000])),
           
           selectInput("select3", label = NA,
                       choices = as.character(movies2$title[1:1000])),
           
           submitButton("Submit")
           
    ),
    
    column(7,
           h3("You Might Like These Too!"),
           tableOutput("table"))
  )
))