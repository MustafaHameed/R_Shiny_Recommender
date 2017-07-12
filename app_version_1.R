rm(list = ls())

library(shinysky)
library(shiny)
library(dplyr)
library(readr)

lfm_art <- read_delim("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-643/master/FP/artists.dat", delim = "\t") %>% select(id, name)
lfm_art$name <- iconv(lfm_art$name, from = "UTF-8", to = "ASCII//TRANSLIT")

art_sim <- as.matrix(read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/art_sim.csv", check.names = FALSE,
                              header=TRUE, sep = ",", stringsAsFactors = FALSE) )

# set rownames to values in V1
row.names(art_sim) <- as.numeric(art_sim[,1])

# now truncate matrix to eliminate col 1
art_sim <- art_sim[,2:ncol(art_sim)]



autocomplete_list <- c(lfm_art$name)

ui <- shinyUI(
  fluidPage(tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
            tags$style(type="text/css","#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px 
                       !important; color: blue;font-size: 20px;font-style: italic;}"),         
            
            titlePanel("Movie Recommendation Engine"),
            fluidRow(
              column(5,
              # one way of doing it
              textInput.typeahead(id="search",
                                  placeholder="Type an artist",
                                  local=data.frame(name=c(autocomplete_list)),
                                  valueKey = "name",
                                  tokens=c(1:length(autocomplete_list)),
                                  template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
              )
            )),
            fluidRow(
              column(7,
                     br(),
                     textOutput('text'),
                     br(),
                     tableOutput("table")
              )
            )
  )
)

server <- function(input, output) {
  
  output$text<- renderText({
    paste("Processing recommended artists for ",input$search)
    
  })
  output$table <- renderTable({
    artist_recommendation(input$search)
  })
  
  artist_recommendation <- function(artist) {
    n_recommended <- 5
    # get name of artist from artist list
    a_val <- lfm_art[lfm_art$name == artist,]$id
    
    # fetch their recommendations: this returns a named vector sorted by similarity
    # the names of the items are the artist IDs
    arecs <- sort(art_sim[as.character(a_val),], decreasing = TRUE)[1:n_recommended]
    
    # extract the artist IDs and convert to numeric
    arecs_IDs <- as.numeric(names(arecs))
    
    # create list of artist names from artist ID's in list
    arec_names <- lfm_art[lfm_art$id %in% arecs_IDs,]$name
    
    results<-cbind(arecs,arec_names)
    return(results)
  }
  
}
shinyApp(ui = ui, server = server)