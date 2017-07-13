library(shiny)
library(dplyr)
library(readr)

# load list of last.fm artists: drop columns containing URL's since they aren't needed
lfm_art <- read_delim("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-643/master/FP/artists.dat", delim = "\t") %>% select(id, name)

# cleanup foreign characters in artist names: most will be converted to '?'
lfm_art$name <- iconv(lfm_art$name, from = "UTF-8", to = "ASCII//TRANSLIT")

# load required matrices  

ag_mat <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/ag_mat.csv")  

art_sim <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/art_sim.csv")  

last_sm <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/last_sm.csv")  

tenrecs <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/user_tenrecs.csv")  

# ____ui____

ui <- shinyUI(fluidPage(
  
  titlePanel(h1(style = "font-family: Arial Black", "last.fm Artist Recommender")),  
  
  sidebarLayout(
    sidebarPanel(
      h4("Select A Recommendation Method:"),
      
      radioButtons("Rec_Choices", label=h4("Rec Choices"),
                   choices = list("Previous Listens" = "last_sm", 
                                  "Similar Artists" = "art_sim", 
                                  "By Genre" = "ag_mat", 
                                  "Top Ten Artists" = "tenrecs"),
                   selected = "last_sm"),
      
      submitButton("Update Choice")
      
    ),
    
    mainPanel(
      fluidRow(
        column(12, verbatimTextOutput("summary"))
        )
      )
    ))
  
)

# ____server____

server <- shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    switch(input$Rec_Choices,
           "ag_mat" = ag_mat,
           "art_sim" = art_sim,
           "last_sm" = last_sm,
           "tenrecs" = tenrecs)
  })
  
  output$summary <- renderPrint({ 
    dataset <- datasetInput()
    dim(dataset)
  })
  
})
  
  


shinyApp(ui, server)