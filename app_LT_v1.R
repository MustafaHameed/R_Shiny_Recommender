library(shiny)
library(dplyr)
library(readr)

# load list of last.fm artists: drop columns containing URL's since they aren't needed
lfm_art <- read_delim("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-643/master/FP/artists.dat", delim = "\t") %>% select(id, name)

# cleanup foreign characters in artist names: most will be converted to '?'
lfm_art$name <- iconv(lfm_art$name, from = "UTF-8", to = "ASCII//TRANSLIT")

# load last.fm tags.dat file to get access to genre names
lfm_tags <- read_delim("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-643/master/FP/tags.dat", delim = "\t")


# load required matrices  

# ------------------------
# load artist-genre matrix
ag_mat <- as.matrix(read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/ag_mat.csv", check.names = FALSE,
                             header=TRUE, sep = ",", stringsAsFactors = FALSE) )

# set rownames to values in V1
row.names(ag_mat) <- as.numeric(ag_mat[,1])

# now truncate matrix to eliminate col 1
ag_mat <- ag_mat[,2:ncol(ag_mat)]
# --------------------------


# ----------------------------
# load artist similarity matrix
art_sim <- as.matrix(read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/art_sim.csv", check.names = FALSE,
                              header=TRUE, sep = ",", stringsAsFactors = FALSE) )

# set rownames to values in V1
row.names(art_sim) <- as.numeric(art_sim[,1])

# now truncate matrix to eliminate col 1
art_sim <- art_sim[,2:ncol(art_sim)]
# ----------------------------

last_sm <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/last_sm.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE)

tenrecs <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/user_tenrecs.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE)


###### Get the top 815 Artists IDS ---------------

# extract the 815 top artist IDs from art_sim matrix and convert to numeric
artistIDs <- as.numeric(rownames(art_sim) )

# then get associated genre names from lfm_art data frame
a_names <- lfm_art[lfm_art$id %in% artistIDs,]$name
#------------------------------------------------


###### Get the list of the top 200 genre names ----

# extract the genre tagIDs from artist-genre matrix and convert to numeric
tagIDs <- as.numeric(colnames(ag_mat))

# then get associated genre names from lfm_tags data frame
g_names <- lfm_tags[lfm_tags$tagID %in% tagIDs,]$tagValue
# --------------------------------------------------



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