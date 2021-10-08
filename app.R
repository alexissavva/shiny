library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(DataExplorer)
library(modeldata)
library(tidyverse)

fifa19_init <- read.csv(file = "final_fifa.csv")
fifa19_final <- fifa19_init[-c(1)]
fifa19_cor <- fifa19_init[-c(1,9,10,11,12,17,18)]
students <- read.csv(file = "students.csv")
sleep <- read.csv(file = "sleepStudy.csv")
secteur <- read.csv(file = "secteursActivite.csv")
result <- read.csv(file = "resultatsDEUG.csv")
chien <- read.csv(file = "chiens.csv")
exemple <- read.csv(file = "exempleNuages.csv")

data_list = list("Fifa 19" = fifa19_cor,
                 "Students" = students,
                 "Sleep Study" = sleep,
                 "Secteurs Activite" = secteur,
                 "Resultat" = result,
                 "Chien" = chien,
                 "Exemple" = exemple)

ui <- dashboardPage(
  dashboardHeader(title = "MyPage"),
  dashboardSidebar(sidebarMenu(
    #Création des onglets, le champ tabName est son identifiant et le champ icon permet d'ajouter des icones deja def
    menuItem("Page d'accueil",
             tabName = "accueil",
             icon = icon("home")),
    menuItem(
      "Page pour variables univarié",
      tabName = "univarie",
      icon = icon("poll")
    ),
    menuItem(
      "Page pour variables bivarié",
      tabName = "bivarie",
      icon = icon("poll")
    ),
    menuItem(
      "Page de prédiction",
      tabName = "prediction",
      icon = icon("poll")
    ),
    menuItem(
      "Page de corrélation",
      tabName = "correlation",
      icon = icon("poll")
    )
  )),
  
  dashboardBody(tabItems(
    #Page d'accueil
    tabItem(
      tabName = "accueil",
      titlePanel("Bienvenue sur la page d'accueil"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h2("Présentation", align = "center"),
          p("Voici notre page présentant nos résultats et nos modèles", align = "center"),
          br(),
          HTML(
            '<center><img src="https://lh4.googleusercontent.com/NcyHaUFCg7TiSIZR391geNW-BXJUGd0TGZ-gsMezwFwPt9vTPIdyMWvWeG06w27f_M682uxnrxeMLJArGDIsHPWww4o4H6ZPGOo8_Xr3FM5bIq99irwLTr5D7P70Owmjiw=w1280" width="300"
     height="300"></center>'
          ),
          br(),
          br(),
          "Shiny is a product of ",
          span("RStudio", style = "color:blue")
        ),
        mainPanel(
          h1("Introduction"),
          p(
            "Shiny is a new package from RStudio that makes it ",
            em("incredibly easy "),
            "to build interactive web applications with R."
          ),
          br(),
          p(
            "For an introduction and live examples, visit the ",
            a("kaggle dataset",
              href = "https://www.kaggle.com/karangadiya/fifa19")
          ),
          br(),
          p(
            "For an introduction and live examples, visit the ",
            a("datasets de départ",
              href = "https://www.kaggle.com/karangadiya/fifa19/download")
          ),
          br(),
          h2("Features"),
          p(
            "- Build useful web applications with only a few lines of code—no JavaScript required."
          ),
          p(
            "- Shiny applications are automatically 'live' in the same way that ",
            strong("spreadsheets"),
            " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser."
          )
        )
      )
    ),
    
    #Visualisation univarie
    tabItem(tabName = "univarie",
            titlePanel("Ozone level!"),
            
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                # Input: Slider for the number of bins ----
                sliderInput(inputId = "bins",
                            label = "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
                
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                
                # Output: Histogram ----
                plotOutput(outputId = "distPlot")
                
              )
            )),
            # fluidRow(box(
            #   width = 2,
            #   #Valeur max 12
            #   h1("TEST"),
            #   fileInput(
            #     inputId = "dataFile1",
            #     label = "Choose CSV File",
            #     accept = c("text/plain", ".csv"),
            #     buttonLabel = "Browse...",
            #     placeholder = "No file selected"
            #   )
            # ))),
    
    # Visualisation bivarie
    tabItem(tabName = "bivarie",
            fluidRow(column(
              5,
              offset = 7,
              h1("Test 2"),
              fileInput(
                inputId = "dataFile2",
                label = "Choose CSV File",
                accept = c("text/plain", ".csv"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              )
            ))),
    
    # Prediction
    tabItem(tabName = "prediction",
            fluidRow(
              column(
                2,
                #offset = 1,
                h1("TEST"),
                fileInput(
                  inputId = "dataFile3",
                  label = "Choose CSV File",
                  accept = c("text/plain", ".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                )
              ),
              column(4,
                     h2("summary"),
                     verbatimTextOutput(outputId = "summary"))
            ),
            fluidRow(column(
              2,
              offset = 1,
              h1("TEST"),
              tableOutput(outputId = "contents")
            ))),
    
    tabItem(tabName = "correlation",
            tabPanel(
              title = "Explore",
              sidebarLayout(
                sidebarPanel(
                  with = 3,
                  h1("Explore data"),
                  shiny::selectInput(
                    inputId = "dataset_choice",
                    label = "Dataset Lable",
                    choices = c("Fifa 19","Students","Sleep Study","Secteurs Activite","Resultat","Chien","Exemple")
                  )
                ),
                mainPanel(h1("Correlation"),
                          plotlyOutput("corrplot", height = 700))
              )
            ))
    
  ))
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x    <- airquality$Ozone
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Ozone level")
    
  })
  
  rv <- reactiveValues()
  
  observe({
    rv$data_set <- data_list %>% pluck(input$dataset_choice)
  })
  
  output$corrplot <- renderPlotly({
    g <- DataExplorer::plot_correlation(rv$data_set)
    plotly::ggplotly(g)
    
  })
  # data <- reactive({
  #   # Initialement, class(input$file1) = NULL
  #   # Après chargement, class(input$file1) = data.frame
  #   # avec les colonnes 'size', 'type', and 'datapath' columns.
  #   inFile <- input$dataFile3
  #   if (is.null(inFile))
  #     return(NULL)
  #   read.csv(inFile$datapath, nrows = 5, header = FALSE)
  # })
  #
  # # Commande pour le calcul du summary
  # output$summary <- renderPrint({
  #   t(summary(data()))
  # })
  # # Commande pour le chargement de données dans 'output'
  # output$contents <- renderTable({
  #   data()
  # })
  #
  # output$boiteMoustaches <- renderPlot({
  #   # Boîte à moustaches
  #   boxplot(
  #     data(),
  #     col = grey(0.8),
  #     main = "Age des salariés",
  #     ylab = "Age",
  #     las = 1
  #   )
  #   # Affichage complémentaires en Y des différents âges
  #   rug(data()[, 1], side = 2)
  # })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)