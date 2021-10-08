library(shiny)

# Contenu de l'interface
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #fileInput("file1", "Choose CSV File",
      
      # TBD !!!
      # Make modifications to get the good rendering!
      # 'file1' ==> 'getFile'
      # ----
      fileInput("getFile", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
  
)

# Commandes à executer
server <- function(input, output){
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$getFile
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header, check.names = FALSE)
  })
  
}

# Association interface & commandes
shinyApp(ui = ui, server = server)
