library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(DataExplorer)
library(modeldata)
library(tidyverse)
library(ggplot2)
library(fmsb)
library(rgdal)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rworldmap)


fifa19_init <- read.csv(file = "../final_fifa_stat.csv")
fifa19_final <- fifa19_init[-c(1)]
fifa19_cor <- fifa19_init[-c(1, 9, 10, 11, 12, 17, 18, 19, 20, 21, 22, 23, 24)]
fifa19_stat <- fifa19_init[c(17, 18, 19, 20, 21, 22)]
fifa19_bivarie <- as.data.frame(fifa19_init[-c(1,2,17,18,19,20,21,22,23,24)])

names_quantitatives <- list("Age", "Overall", "Potential", "Value", "Wage", "Contract.Valid.Until", "Height", "Weight", "Release.Clause") 
names_qualitatives <- list("Nationality", "Preferred.Foot", "Weak.Foot", "Skill.Moves", "Position")

students <- read.csv(file = "../students.csv")
sleep <- read.csv(file = "../sleepStudy.csv")
secteur <- read.csv(file = "../secteursActivite.csv")
result <- read.csv(file = "../resultatsDEUG.csv")
chien <- read.csv(file = "../chiens.csv")
exemple <- read.csv(file = "../exempleNuages.csv")
world_map <- read.csv(file = "country.csv",sep = ";")

name_player <- fifa19_final[c(1)]
name_player <- head(name_player, 25)
name_player <- head(name_player, 25)
names(name_player) <- NULL
fifa19_stat <- head(fifa19_stat, 25)
names(fifa19_stat) <- NULL
name_player <- unlist(name_player)

data_list = list(
  "Fifa 19" = fifa19_cor,
  "Students" = students,
  "Sleep Study" = sleep,
  "Secteurs Activite" = secteur,
  "Resultat" = result,
  "Chien" = chien,
  "Exemple" = exemple
)

ui <- dashboardPage(
  dashboardHeader(title = "MyPage"),
  dashboardSidebar(
    sidebarMenu(
      #CrÃ©ation des onglets, le champ tabName est son identifiant et le champ icon permet d'ajouter des icones deja def
      menuItem("Page d'accueil",
               tabName = "accueil",
               icon = icon("home")),
      menuItem(
        "RÃ©partition des joueurs dans le monde",
        tabName = "repartition",
        icon = icon("globe-europe")
      ),
      menuItem(
        "Page pour variables univariÃ©",
        tabName = "univarie",
        icon = icon("poll")
      ),
      menuItem(
        "Page pour variables bivariÃ©",
        tabName = "bivarie",
        icon = icon("poll")
      ),
      menuItem(
        "Page de prÃ©diction",
        tabName = "prediction",
        icon = icon("poll")
      ),
      menuItem(
        "Page de corrÃ©lation",
        tabName = "correlation",
        icon = icon("poll")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      #Page d'accueil
      tabItem(
        tabName = "accueil",
        titlePanel("Bienvenue sur la page d'accueil"),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h2("PrÃ©sentation", align = "center"),
            p("Voici notre page prÃ©sentant nos rÃ©sultats et nos modÃ¨les", align = "center"),
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
              a("datasets de dÃ©part",
                href = "https://www.kaggle.com/karangadiya/fifa19/download")
            ),
            br(),
            h2("Features"),
            p(
              "- Build useful web applications with only a few lines of codeâno JavaScript required."
            ),
            p(
              "- Shiny applications are automatically 'live' in the same way that ",
              strong("spreadsheets"),
              " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser."
            )
          )
        )
      ),
      tabItem(tabName = "repartition",
              mainPanel(
                fluidRow(
                  plotOutput(outputId = "map")
                )
              )
      ),
      
      #Visualisation univarie
      tabItem(
        tabName = "univarie",
        titlePanel("Static Panel"),
        sidebarLayout(
          sidebarPanel(
            h1("Dataset"),
            shiny::selectInput(
              inputId = "dataset_choice_label",
              label = "Dataset Lable",
              choices = c("Fifa 19")
            ),
            sliderInput(
              inputId = "overall",
              label = "Overall",
              min = 46,
              max = 94,
              value = c(80, 82)
              
            ),
            shiny::selectInput(
              inputId = "dataset_players",
              label = "Select Player",
              choices = name_player
            ),
            checkboxInput("compare_player", "Compare players", FALSE),
            shiny::selectInput(
              inputId = "player2",
              label = "Other player",
              choices = name_player
            )
          ),
          
          mainPanel(
            fluidRow(plotOutput(outputId = "distPlot")),
            fluidRow(plotOutput(outputId = "distPlot2")),
            fluidRow(plotOutput(outputId = "distPlot3")),
            fluidRow(plotOutput(outputId = "distPlot4")),
            fluidRow(plotOutput(outputId = "distPlot5")),
          )
        )
      ),
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
              titlePanel("Static Panel 2"),
              sidebarLayout(
                sidebarPanel(h1("Dataset"),
                             shiny::selectInput(
                               inputId = "choice_bivar_1",
                               label = "Feature 1",
                               choices = colnames(fifa19_bivarie )  #      [-c(input$choice_bivar2)])
                             ),
                             shiny::selectInput(
                               inputId = "choice_bivar_2",
                               label = "Feature 2",
                               choices = colnames(fifa19_bivarie) #         [-c(input$choice_bivar1)])
                             ))
                ,
                mainPanel(fluidRow(plotOutput(outputId = "plotBivarie")))
              )
      ),
      
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
                      choices = c(
                        "Fifa 19",
                        "Students",
                        "Sleep Study",
                        "Secteurs Activite",
                        "Resultat",
                        "Chien",
                        "Exemple"
                      )
                    )
                  ),
                  mainPanel(h1("Correlation"),
                            plotlyOutput("corrplot", height = 700))
                )
              ))
      
    )
  )
)

server <- function(input, output) {
  age <- fifa19_final[c(2)]
  rv <- reactiveValues()
  overall <- fifa19_final[c('Overall')]
  over_slider <- fifa19_final[c('Overall','Age')]
  
  create_beautiful_radarchart <- function(data,
                                          color = "#F5A42F",
                                          vlabels = colnames(data),
                                          vlcex = 0.7,
                                          caxislabels = NULL,
                                          title = NULL,
                                          ...) {
    radarchart(
      data,
      axistype = 1,
      # Personnaliser le polygone
      pcol = color,
      pfcol = scales::alpha(color, 0.5),
      plwd = 2,
      plty = 1,
      # Personnaliser la grille
      cglcol = "grey",
      cglty = 1,
      cglwd = 0.8,
      # Personnaliser l'axe
      axislabcol = "grey",
      # Ãtiquettes des variables
      vlcex = vlcex,
      vlabels = vlabels,
      caxislabels = caxislabels,
      title = title
    )
  }
  
  observe({
    rv$data_set <- data_list %>% pluck(input$dataset_choice)
  })
  
  output$distPlot3 <- renderPlot({
    plot(
      table(age),
      type = "h",
      col = "green4",
      xlab = "Age",
      ylab = "Effectifs",
      main = "Distribution des effectifs pour l'Ã¢ge"
    )
  })
  
  
  
  bivarie <- reactiveValues()
  
  observe({
    if ((input$choice_bivar_1 %in% names_quantitatives ) && ( input$choice_bivar_2 %in% names_quantitatives )) {
      bivarie$type <- 'quant_quant'
    }
    else if ((input$choice_bivar_1 %in% names_qualitatives ) && ( input$choice_bivar_2 %in% names_quantitatives )) {
      bivarie$type <- 'quali_quant'
    }
    else if ((input$choice_bivar_1 %in% names_quantitatives ) && ( input$choice_bivar_2 %in% names_qualitatives ) ) 
    {
      bivarie$type <- 'quant_quali' 
    }
    else {
      bivarie$type <- 'quali_quali'
    }
  })
  
  data <- eventReactive(input$go, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = TRUE)
  })
  
  output$plotBivarie <- renderPlot({
    
    options(scipen=999)
    x.var = input$choice_bivar_1; 
    y.var = input$choice_bivar_2;
    
    if ( bivarie$type == 'quant_quant') 
      {
      
        #plot(x = fifa19_bivarie[, x.var], y = fifa19_bivarie[, y.var], col = "blue" ) 
        
        scatter_plot_quanti = ggplot(fifa19_bivarie, aes(x=x.var, y=y.var)) + geom_point(size=2, shape=23)
        plot(scatter_plot_quanti)
      
      }
    else if ( bivarie$type == 'quant_quali') 
      {
      boxplot(fifa19_bivarie[, x.var] ~ fifa19_bivarie[, y.var] , col="grey", xlab = y.var, ylab = x.var)
      }
    else if ( bivarie$type == 'quali_quant') 
      {
      boxplot(fifa19_bivarie[, y.var] ~ fifa19_bivarie[, x.var] , col="grey", xlab = x.var, ylab = y.var)
      }
    else
      {
      barplot_quali_quali = ggplot(fifa19_bivarie, aes(x = Weak.Foot, fill = Skill.Moves)) + geom_bar(position = "fill")
      plot(barplot_quali_quali)
      }
    
    options(scipen=0)

  })
  
  output$distPlot2 <- renderPlot({
    ggplot(fifa19_final, aes(Overall)) + geom_boxplot()
  })
  
  output$distPlot2 <- renderPlot({
    updated_data <- over_slider[over_slider$Overall>=input$overall[1]&over_slider$Overall<=input$overall[2],]
    plot(updated_data$Overall,updated_data$Age,xlab = "Overall", ylab = "Age of player", main = "test")
  })
  
  output$map <- renderPlot({
    visitedMap <- joinCountryData2Map(world_map, 
                                      joinCode = "NAME",
                                      nameJoinColumn = "country",
                                      verbose = TRUE)
    mapParams <- mapCountryData(visitedMap, 
                                nameColumnToPlot="Freq",
                                oceanCol = "azure2",
                                catMethod = "categorical",
                                missingCountryCol = gray(.8),
                                colourPalette = c("coral",
                                                  "coral2",
                                                  "coral3", "orangered", 
                                                  "orangered3", "orangered4"),
                                addLegend = F,
                                mapTitle = "",
                                border = NA)
    # do.call(addMapLegendBoxes, c(mapParams,
    #                              x = 'bottom',
    #                              title = "No. of visits",
    #                              horiz = TRUE,
    #                              bg = "transparent",
    #                              bty = "n"))
  })
  
  output$distPlot <- renderPlot({
    skill_player <- data.frame(
      row.names = name_player,
      pac = unlist(fifa19_stat[1]),
      sho = unlist(fifa19_stat[2]),
      pas = unlist(fifa19_stat[3]),
      drib = unlist(fifa19_stat[4]),
      def = unlist(fifa19_stat[5]),
      phy = unlist(fifa19_stat[6])
    )
    
    max_min <- data.frame(
      pac = c(100, 0),
      sho = c(100, 0),
      pas = c(100, 0),
      drib = c(100, 0),
      def = c(100, 0),
      phy = c(100, 0)
    )
    rownames(max_min) <- c("Max", "Min")
    df <- rbind(max_min, skill_player)
    op <- par(mar = c(1, 2, 2, 1))
    if (!input$compare_player) {
      stat_player <- df[c("Max", "Min", input$dataset_players), ]
      create_beautiful_radarchart(
        stat_player,
        caxislabels = c(0, 25, 50, 75, 100),
        color = c("#F5A42F")
      )
    }
    else{
      stat_player1 <- df[c("Max", "Min", input$dataset_players), ]
      stat_player2 <- df[c(input$player2), ]
      stat_players = rbind(stat_player1, stat_player2)
      create_beautiful_radarchart(
        stat_players,
        caxislabels = c(0, 25, 50, 75, 100),
        color = c("#F5A42F", "#00AFBB")
      )
      legend(
        x = "right",
        legend = rownames(stat_players[-c(1, 2),]),
        horiz = TRUE,
        bty = "n",
        pch = 20 ,
        col = c("#F5A42F", "#00AFBB"),
        text.col = "black",
        cex = 1,
        pt.cex = 1.5
      )
    }
    par(op)
  })
  
  output$corrplot <- renderPlotly({
    g <- DataExplorer::plot_correlation(rv$data_set)
    plotly::ggplotly(g)
    
  })
  
}
# Association interface & commandes
shinyApp(ui = ui, server = server)
