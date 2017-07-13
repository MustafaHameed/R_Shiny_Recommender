rm(list = ls())

library(shinysky)
library(shiny)
library(dplyr)
library(readr)

# load artists.data file - has artists names in it
lfm_art <- read_delim("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-643/master/FP/artists.dat", delim = "\t") %>% select(id, name)
lfm_art$name <- iconv(lfm_art$name, from = "UTF-8", to = "ASCII//TRANSLIT")

# load last.fm tags.dat file - has list of genre names
lfm_tags <- read_delim("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-643/master/FP/tags.dat", delim = "\t")


# ----------------------------
# load artist similarity matrix
art_sim <- as.matrix(read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/art_sim.csv", check.names = FALSE,
                              header=TRUE, sep = ",", stringsAsFactors = FALSE) )

# set rownames to values in V1
row.names(art_sim) <- as.numeric(art_sim[,1])

# now truncate matrix to eliminate col 1
art_sim <- art_sim[,2:ncol(art_sim)]
# -----------------------------


# ------------------------
# load artist-genre matrix
ag_mat <- as.matrix(read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/ag_mat.csv", check.names = FALSE,
                             header=TRUE, sep = ",", stringsAsFactors = FALSE) )

# set rownames to values in V1
row.names(ag_mat) <- as.numeric(ag_mat[,1])

# now truncate matrix to eliminate col 1
ag_mat <- ag_mat[,2:ncol(ag_mat)]
# --------------------------

# load UBCF recs for each user (10 artists recommendations per user)
user_tenrecs <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/user_tenrecs.csv", 
                         header=TRUE, sep = ",", stringsAsFactors = FALSE)


# load user-artist data frame: to be used for displaying list of artists
# a user has listened to.
last_sm <- read.csv("https://raw.githubusercontent.com/RobertSellers/R_Shiny_Recommender/master/R-Obj-CSVs/last_sm.csv", 
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

# initialize autocomplete list for artist names
autocomplete_list <- c(a_names)

ui <- shinyUI(
  fluidPage(tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
            tags$style(type="text/css","#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px 
                       !important; color: blue;font-size: 20px;font-style: italic;}"),         
            
            titlePanel("Musical Artist Recommender"),
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