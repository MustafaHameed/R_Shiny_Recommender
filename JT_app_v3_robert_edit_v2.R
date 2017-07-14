rm(list = ls())

library(shiny)
library(dplyr)
library(readr)
library(DT)
library(data.table)

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

# remove artist names that start with '?' characters since they cause problems
# when trying to later retrieve the associated artist ID

a_names <- a_names[a_names != '????']
a_names <- a_names[a_names != '?????']
a_names <- a_names[a_names != '??????']

###### Get the list of the top 200 genre names ----

# extract the genre tagIDs from artist-genre matrix and convert to numeric
tagIDs <- as.numeric(colnames(ag_mat))

# then get associated genre names from lfm_tags data frame
g_names <- lfm_tags[lfm_tags$tagID %in% tagIDs,]$tagValue
# --------------------------------------------------

##### Get a list of distinct userID's
userIDs <- unique(last_sm$userID)

u_hdr <- paste("Select a User ID (", min(userIDs), " - ", max(userIDs), " )" )


# ____ui____

ui <- shinyUI(fluidPage(

  ###################  CSS   ###################  
  HTML('<footer>2017 - James Topor, Logan Thompson, Robert Sellers</footer>'),
  tags$style(type="text/css", "footer{
    position:absolute;
    bottom:0;
    width:100%;
    height:50px; /* Height of the footer */
    color: white;
    padding: 10px;
    background-color: black;
    z-index: 1000;}"),
  tags$style(type="text/css", "#table th {
    display: none;}"), 
  tags$style(type="text/css", "#profileTable th {
    display: none;"), 
  tags$style(type="text/css", "#profileTable {
    max-height: 200px; 
    overflow-y: auto;}"),
  ###############################################

  titlePanel(h1(style = "font-family: Arial Black", "last.fm Artist Recommender")),  
  
  sidebarLayout(
    sidebarPanel(

      selectInput("d_userID", u_hdr,
                   choices = c(Enter_User_ID='', userIDs )),
      
      radioButtons("Rec_Choices", label=strong("Select A Recommendation Method:"),
                   choices = list("By Similar Artists (Top 5)" = "art_sim", 
                                  "By Genre (Top 5)" = "ag_mat", 
                                  "10 Artists Recommended by Similar Users" = "tenrecs"),
                   selected = "tenrecs"),
      strong("User Top Artists"),
      DT::dataTableOutput("profileTable"),
      strong("Select an Artist to run Artist Similarity")
      
    ), # end sidebarPanel

    mainPanel(
        h3(textOutput("text")),
        br(),
        uiOutput("selectedItem"),
        br(),
        tableOutput("table"),
        tableOutput("tableSelection")
      ) # end mainPanel

    )) # end sidebarLayout
  )

############################################################################
############################################################################
# ____server____

server <- shinyServer(function(input, output) {

#############################################
# Function to create dynamic drop down containing appropriate list to choose from
  
  output$selectedItem <- renderUI({
    
    if (input$Rec_Choices == "ag_mat") {
      selectInput("d_genre", "Select Genre:",
                  choices = sort(g_names) )
      
    } else if (input$Rec_Choices == "art_sim") {
      selectInput("d_artsim", "Select Artist:",
                  choices = sort(a_names) )
      
    } 
  })
  
##############################################
# Function to generate heading for main panel
  
  output$text<- renderText({
    
    if (input$Rec_Choices == "ag_mat") {
      paste("Top 5 Artists in Selected Genre")
      
    } else if (input$Rec_Choices == "art_sim") {
      paste("Top 5 Artists Similar to Selected Artist", input$selectedItem)
      
    } else if (input$Rec_Choices == "tenrecs") {
      paste("10 Artists You May Like")
      
    } # end if
    
  })

##############################################    
# function to generate list of recommended artists depending on
# the method selected by the user
  
  output$table <- renderTable({
    
    if (input$Rec_Choices == "ag_mat") {
      # Top 5 Artists in Selected Genre
      
      # set number of artists to recommend
      n_recommended <- 5
      
      # get tagID of genre
      g_tag <- lfm_tags[lfm_tags$tagValue == input$d_genre,]$tagID
      
      # fetch the top N artists:
      # the names of the items are the artist IDs
      g_arecs <- sort(ag_mat[,as.character(g_tag)], decreasing = TRUE)[1:n_recommended]
      
      # extract the artist IDs and convert to numeric
      g_arecs_IDs <- as.numeric(names(g_arecs))
      
      # create list of artist names from artist ID's in list
      g_arec_names <- lfm_art[lfm_art$id %in% g_arecs_IDs,]$name
      
      return(g_arec_names)
    
    ############################################
      
    } else if (input$Rec_Choices == "art_sim") {
      # Top 5 Artists Similar to Selected Artist
      
      n_recommended <- 5
      
      # get name of artist from artist list
      a_val <- lfm_art[lfm_art$name == input$d_artsim,]$id

      a_val <- as.numeric(sort(a_val))

      
      # fetch their recommendations: this returns a named vector sorted by similarity
      # the names of the items are the artist IDs
      arecs <- sort(art_sim[as.character(a_val),], decreasing = TRUE)[1:n_recommended]
      
      # extract the artist IDs and convert to numeric
      arecs_IDs <- as.numeric(names(arecs))
      
      # create list of artist names from artist ID's in list
      arec_names <- lfm_art[lfm_art$id %in% arecs_IDs,]$name
      
      return(arec_names)
      
    #############################################
      
    } else if (input$Rec_Choices == "tenrecs") {
      # Get 10 Artists You May Like based on similar users
      
      # fetch their recommendations
      urecs <- sort(as.vector(subset(tenrecs, userID == input$d_userID)[2:11]) )
      
      # create list of artist names from artist ID's in list
      rec_names <- subset(lfm_art, id %in% urecs)$name
      
      return(rec_names)
      
    } # end if
    
    
  }) # end renderTable

  output$tableSelection <-renderTable({
    return (filteredTable_selected())
  })
  
  filteredTable_selected <- reactive({
    id <- input$profileTable_rows_selected
    filteredTable_data()[id,]
  })
  
  filteredTable_data <- reactive({
    # get list of previously listened artists for userID
    user_arts <- last_sm$artistID[last_sm$userID == input$d_userID]
    
    # create list of artist names from artist ID's in list
    ul_names <- lfm_art[lfm_art$id %in% user_arts,]$name
    
    # remove any artists that start with ? character
    ul_names <- ul_names[ul_names != '????']
    ul_names <- ul_names[ul_names != '?????']
    ul_names <- ul_names[ul_names != '??????']
    ret <- data.table(ul_names)
  })
  
  output$profileTable <- DT::renderDataTable({
        user_arts <- last_sm$artistID[last_sm$userID == input$d_userID]
    
    # create list of artist names from artist ID's in list
    ul_names <- lfm_art[lfm_art$id %in% user_arts,]$name
    
    # remove any artists that start with ? character
    ul_names <- ul_names[ul_names != '????']
    ul_names <- ul_names[ul_names != '?????']
    ul_names <- ul_names[ul_names != '??????']
    ret <- data.table(ul_names)
    ret <- data.table(ul_names)
    datatable(filteredTable_data(), rownames = FALSE, colnames=NULL, selection="single",options = list(dom = 't'))
  })
  
  callback = "function(table) {
    table.on('click.dt', 'tr', function() {
    table.$('tr.selected').removeClass('selected');
    $(this).toggleClass('selected');            
    Shiny.onInputChange('rows',
    table.rows('.selected').data()[0][0]);
    });
    }"
  
  
}) # end server
  
  


shinyApp(ui, server)