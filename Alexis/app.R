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
library(rpart)
library(randomForest)
library(maps)
library(Metrics)
library(dummies)
library(mgcv)
library(e1071)
library(gridExtra)
library(grid)
library(cowplot)
library(RColorBrewer)
library(shinyjs)
library(hash)

variance_expliquee<- function(obs,prev)
{
  VarE = 1 - var(prev - obs) / var(obs)
  return(VarE)
}

store_results <- function( res, idx_model, obs, pred ) 
{
  res[1,idx_model] = mae(obs, pred)
  res[2,idx_model] = rmse(obs, pred)
  res[3,idx_model] = variance_expliquee(obs, pred)
  res[4,idx_model] = cor(obs, pred)^2
  
  return(res)
}

Theme.GridPlot = theme(
  legend.position = "none",
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size=10),
  axis.text.y = element_text(size=10),
  axis.title.y = element_text(size=12)
)


obs_prev <- function( df, obs, prev )
{
  return(ggplot(df, aes( x= obs, y = prev))+
           geom_point(color = "darkblue" ) +
           Theme.GridPlot +
           xlab("Observés")+
           ylab("Prévus")+
           #lims(x= c(35,90), y = c(35,90))+
           geom_abline(intercept = 0, slope = 1, size=0.75)
  )
}


nuage_res <- function( df, obs, prev )
{
  return(ggplot(df, aes( x= obs, y = prev - obs ))+
           geom_point(color = "darkblue" ) +
           Theme.GridPlot +
           xlab("Observés")+
           ylab("Résidus")+
           #lims(x= c(35,90), y = c(-22,22))+
           geom_abline(intercept = 0, slope = 0, size=0.75)
  )
}

#options(warn = -1)

fifa19_init <- read.csv(file = "../final_fifa_stat.csv")

fifa19_init$Weak.Foot <- factor(fifa19_init$Weak.Foot, levels=c("S", "A", "B", "C", "D"), ordered=TRUE)
fifa19_init$Skill.Moves <- factor(fifa19_init$Skill.Moves, levels=c("S", "A", "B", "C", "D"), ordered=TRUE)

fifa19_final <- fifa19_init[-c(1)]
fifa19_cor <- fifa19_init[-c(1, 9, 10, 11, 12, 17, 18, 19, 20, 21, 22, 23, 24)]
fifa19_stat <- fifa19_init[c(17, 18, 19, 20, 21, 22)]
fifa19_bivarie <- as.data.frame(fifa19_init[-c(1,2,17,18,19,20,21,22,23,24)])
fifa19_pred <- as.data.frame(fifa19_init[-c(1,2,17,18,19,20,21,22,23,24)])

names_quantitatives <- list("Age", "Overall", "Potential", "Value", "Wage", "Contract.Valid.Until", "Height", "Weight", "Release.Clause") 
names_qualitatives <- list("Nationality", "Preferred.Foot", "Weak.Foot", "Skill.Moves", "Position")

fifa19_pred_quantitatives <- as.data.frame(scale(fifa19_pred[unlist(names_quantitatives)] ,  center = TRUE, scale = TRUE))
fifa19_pred_qualitatives <- as.data.frame(fifa19_pred[unlist(names_qualitatives)])

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
  dashboardHeader(title = "Projet Shiny sur Fifa19"),
  dashboardSidebar(
    sidebarMenu(
      #Creation des onglets, le champ tabName est son identifiant et le champ icon permet d'ajouter des icones deja def
      menuItem("Page d'accueil et statistique",
               tabName = "accueil",
               icon = icon("home")),
      menuItem(
        "Repartition des joueurs dans le monde",
        tabName = "repartition",
        icon = icon("globe-europe")
      ),
      menuItem(
        "Correlations",
        tabName = "correlation",
        icon = icon("poll")
      ),
      menuItem(
        "Analyses univariees",
        tabName = "univarie",
        icon = icon("poll")
      ),
      menuItem(
        "Analyses bivariees",
        tabName = "bivarie",
        icon = icon("poll")
      ),
      menuItem(
        "Prediction",
        tabName = "prediction",
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
            h2("Presentation", align = "center"),
            p("Voici notre page presentant nos resultats et nos modeles", align = "center"),
            br(),
            #        HTML(
            #          '<center><img src="https://lh4.googleusercontent.com/NcyHaUFCg7TiSIZR391geNW-BXJUGd0TGZ-gsMezwFwPt9vTPIdyMWvWeG06w27f_M682uxnrxeMLJArGDIsHPWww4o4H6ZPGOo8_Xr3FM5bIq99irwLTr5D7P70Owmjiw=w1280" width="300"
            # height="300"></center>'
            #),
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
              a("datasets de depart",
                href = "https://www.kaggle.com/karangadiya/fifa19/download")
            ),
            br(),
            h2("Features"),
            p(
              "- Build useful web applications with only a few lines of JavaScript required."
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
              leafletOutput(outputId = "map")
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
              label = "Select Player (Orange)",
              choices = name_player
            ),
            checkboxInput("compare_player", "Compare players", FALSE),
            shiny::selectInput(
              inputId = "player2",
              label = "Other player (Blue)",
              choices = name_player
            ),
            shiny::selectInput(
              inputId = "choice_univar",
              label = " variable ",
              choices = colnames(fifa19_bivarie) #         [-c(input$choice_bivar1)])
            )
          ),
          
          mainPanel(
            fluidRow(plotOutput(outputId = "distPlot")),
            fluidRow(plotOutput(outputId = "distPlot2")),
            fluidRow(plotOutput(outputId = "distPlot4")),
            fluidRow(plotOutput(outputId = "distPlot5"))
          )
        )
      ),
      
      # Visualisation bivarie
      tabItem(tabName = "bivarie",
              titlePanel("Static Panel 2"),
              sidebarLayout(
                sidebarPanel(h1("Dataset"),
                             shiny::selectInput(
                               inputId = "choice_bivar_1",
                               label = "Feature 1",
                               choices = colnames(fifa19_bivarie ) 
                             ),
                             shiny::selectInput(
                               inputId = "choice_bivar_2",
                               label = "Feature 2",
                               choices = colnames(fifa19_bivarie) 
                             ))
                ,
                mainPanel(fluidRow(plotOutput(outputId = "plotBivarie1"))) #,br(),plotOutput(outputId = "plotBivarie2")))
              )
      ),
      
      # Prediction
      tabItem(tabName = "prediction",
              useShinyjs(),
              fluidRow(align = "center",
                       column(12,
                              h1('Prediction de la variable "Salaire"')
                       )),
              br(),
              br(),
              fluidRow(align = "center",
                       column(12,
                              h2('Construction des modeles')
                       )),
              br(),
              fluidRow( 
                column(6,  
                       checkboxGroupInput("variable_pred", 
                                          "Variable(s) utilisee(s) :",
                                          choiceNames = list("Age","General","Potentiel","Valeur","Pied Fort","Pied Faible", "Technique", "Position", "Taille", "Poids","Clause") ,
                                          choiceValues = colnames(fifa19_pred)[-c(2,6,11)]
                       )
                ),
                column(6,   sliderInput("app_rate",
                                        "Pourcentage de donnees d'apprentissage :",
                                        min = 50,
                                        max = 95,
                                        value = 80)
                       ,
                       br(),
                       checkboxGroupInput("models_pred", 
                                          "Modele(s) utilise(s) :",
                                          choiceNames = list("Regression Lineaire Multiple","Arbre CART", "GAM", "SVR","RandomForest") ,
                                          choiceValues = c("lm","cart","gam","svr","rf")
                       )
                )),
              fluidRow(   
                align = "center",
                actionButton("launch_pred", "Lancer la prediction")
              ),
              br(),
              fluidRow(
                align = "center",
                h2('Resultats de la prediction'),
                br(),
                column(6,
                       mainPanel( align = "left",
                                  radioButtons("metric", 
                                                     "Metriques",
                                                     choiceNames = list("MAE / RMSE", "R2 / EV") ,
                                                     choiceValues = list("mae","ev")
                                  ),
                                  radioButtons("set_left", 
                                                     "Jeu",
                                                     choiceNames = list("Entrainement", "Test") ,
                                                     choiceValues = list("train","test")
                                  ),
                                  br(),
                                  plotOutput("barplot_metrics", width = "80%")),
                  ),
                
                column(6,
                       mainPanel( align = "left",
                                  radioButtons("model_res", 
                                                     "Modele",
                                                     choiceNames = list("Regression Lineaire Multiple","Arbre CART", "GAM", "SVR","RandomForest") ,
                                                     choiceValues = c("lm","cart","gam","svr","rf")
                                  ),
                                  radioButtons("set_right", 
                                                     "Jeu",
                                                     choiceNames = list("Entrainement", "Test") ,
                                                     choiceValues = list("train","test")
                                  ),
                                  br(),
                                  plotOutput("obs_prev_lm", width = "100%"))
                       )
              ),
              
                      
              ),
      
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

server <- function(input, output, session) {
  age <- fifa19_final[c(2)]
  rv <- reactiveValues()
  overall <- fifa19_final[c(4)]
  over_slider <- fifa19_final[c('Overall','Age')]
  first_pred <- TRUE
  
  create_beautiful_radarchart <- function(data,
                                          color = "#F5A42F",
                                          vlabels = colnames(data),
                                          vlcex = 0.7,
                                          caxislabels = NULL,
                                          title = NULL) {
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
      # Etiquettes des variables
      vlcex = vlcex,
      vlabels = vlabels,
      caxislabels = caxislabels,
      title = title
    )
  }
  
  observe({
    rv$data_set <- data_list %>% pluck(input$dataset_choice)
  })
  
  observe({
    rv$data_set <- data_list %>% pluck(input$dataset_choice)
  })
  
  univarie <- reactiveValues()
  
  observe({
    if (first_pred) 
    { 
      hide("obs_prev_lm")
      hide("barplot_metrics")
    }
  })
  
  observe({
    if (input$choice_univar %in% names_quantitatives ) {
      univarie$type <- 'quant'
    }
    else if (input$choice_univar %in% names_qualitatives){
      univarie$type <- 'qualit'
    } 
  })
  
  output$distPlot4 <- renderPlot({
    if ( univarie$type == 'quant'){
      plot(
        table(fifa19_final[c(input$choice_univar)]),
        type = "h",
        col = "green4",
        xlab = input$choice_univar,
        ylab = "Number of player",
        main = paste("Distribution des" , input$choice_univar)
      )}
    else {
      effectifs <- table(fifa19_final[c(input$choice_univar)])
      barplot(effectifs, main = "Categories Socioprofessionnelles", 
              ylab="Effectifs", las = 2,
              names.arg = substr(names(effectifs), 1, 4))
      
    }
    
  })
  
  output$distPlot5 <- renderPlot({
    if ( univarie$type == 'quant'){
      boxplot(fifa19_final[, input$choice_univar],main = paste("boxplot de " , input$choice_univar) , col="grey", xlab = input$choice_univar)
    }
    else {
      effectifs <- table(fifa19_final[c(input$choice_univar)])
      pie(effectifs, labels = substr(names(effectifs), 1, 4), 
          main = "Categories Socioprofessionnelles", col=c())
      
    }
    
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
  
  output$plotBivarie1 <- renderPlot({
    
    options(scipen=999)
    x.var = input$choice_bivar_1; 
    y.var = input$choice_bivar_2;
    
    if ( bivarie$type == 'quant_quant') 
    {
      
      scatter_plot_quanti = ggplot(fifa19_bivarie, aes_string(x=x.var, y=y.var)) + 
        geom_point(size=2, shape=23) + 
        ggtitle(paste("Graphe de " , y.var , " en fonction de ", x.var ))  +
        theme(plot.title = element_text(hjust = 0.5, size= 20, face = "bold") ) +
        xlab(x.var) + 
        ylab(y.var) +
        theme(axis.text.x=element_text(size=15), axis.title.x=element_text(size=12)) + #, margin = margin(t = 0, r = 0, b = 0, l = 0)
        theme(axis.text.y=element_text(size=15), axis.title.y=element_text(size=12)) #+ , margin = margin(t = 0, r = 20, b = 0, l = 0)
      #theme( plot.background = element_rect( fill = "lightgreen", colour = "white", size = 10 ) )
      plot(scatter_plot_quanti)
      
    }
    else if ( bivarie$type == 'quant_quali') 
    {
      boxplot(fifa19_bivarie[, x.var] ~ fifa19_bivarie[, y.var] , col="grey", xlab = y.var, ylab = x.var, main=paste("Boxplot de " , y.var , " en fonction de ", x.var ))
    }
    else if ( bivarie$type == 'quali_quant') 
    {
      
      boxplot(fifa19_bivarie[, y.var] ~ fifa19_bivarie[, x.var] , col="orange", border="red", xlab = x.var, ylab = y.var, main=paste("Boxplot de " , y.var , " en fonction de ", x.var ))
    }
    else
    {
      barplot_quali_quali = ggplot(fifa19_bivarie,  aes_string(x=x.var, fill=y.var)) +
        geom_bar(position = "fill")+ 
        ggtitle(paste("Barplot de " , y.var , " en fonction de ", x.var ))  +
        theme(plot.title = element_text(hjust = 0.5, size= 20, face = "bold") ) +
        xlab(x.var) + 
        ylab(y.var) +
        theme(axis.text.x=element_text(size=15), axis.title.x=element_text(size=12)) + # , margin = margin(t = 0, r = 0, b = 0, l = 0)
        theme(axis.text.y=element_text(size=15), axis.title.y=element_text(size=12))   # , margin = margin(t = 0, r = 20, b = 0, l = 0)
      
      plot(barplot_quali_quali)
    }
    
    options(scipen=0)
    
  }, 
  height = 600, width = 950 )
  
  
  
  ## Prediction
  
  pred <- reactiveValues(data_pred = NULL, models = NULL)
  
  res_app_global <- NULL
  res_test_global <- NULL
  plots_test_list_global <- NULL
  plots_train_list_global <- NULL
  nb_model <- NULL
  dict_index <- NULL
  
  observeEvent(input$launch_pred, {
    
    # Recuperation des variables 
    pred$data_pred <- input$variable_pred
    pred$models <- input$models_pred
    
    # DF de predicteurs + Variable cible
    fifa19_pred <- fifa19_init[pred$data_pred]
    target_pred <- fifa19_init$Wage
    
    # Decoupage Train / Test
    size_data = nrow(fifa19_pred)
    napp_pred = round(0.8*size_data)
    indices_fifa = sample(1:size_data, napp_pred , replace=FALSE)
    
    data_fifa_train = fifa19_pred[indices_fifa,]
    data_fifa_test = fifa19_pred[-indices_fifa,]
    wage_train = target_pred[indices_fifa]
    wage_test = target_pred[-indices_fifa]
    
    # Variables Quantitatives et Qualitatives choisies
    quantitatives_chosen = intersect(as.list(pred$data_pred), names_quantitatives)
    qualitatives_chosen = intersect(as.list(pred$data_pred), names_qualitatives)
    
    n_model = length(input$models_pred)
    
    if ( "cart" %in% pred$models ) {
      n_model = n_model +1
    }
    
    if ( "svr" %in% pred$models ) {
      n_model = n_model +1
    }
    
    n_plots = n_model * 2     # Pour chaque modèle : nuage observés / prédits + graphe des résidus

    plots_test_list <- list()
    plots_train_list <- list()
    
    res_app = matrix(0,nrow = 4, ncol = n_model)
    res_test = matrix(0,nrow = 4, ncol = n_model)
    
    colnames_pred = list()
    
    idx_model = 1
    
    residus_train = as.data.frame(wage_train)
    residus_test = as.data.frame(wage_test)
    
    if ( "lm" %in% pred$models ) {
      
      model_lm = lm( wage_train ~. , data=data_fifa_train)
      wage_lm_train = as.numeric(predict(model_lm))
      wage_lm_test = as.numeric(predict(model_lm, newdata = data_fifa_test))
      
      colnames_pred <- append(colnames_pred, "Modele Linéaire")
      
      res_app <- store_results( res_app, idx_model, wage_train, wage_lm_train ) 
      res_test <- store_results( res_test, idx_model, wage_test, wage_lm_test )

      residus_train = cbind( residus_train, wage_lm_train)
      residus_test = cbind( residus_test, wage_lm_test)
      
      plots_train_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_train, wage_train, wage_lm_train )
      plots_train_list[[2*idx_model ]] <- nuage_res(residus_train, wage_train, wage_lm_train )
      
      plots_test_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_test, wage_test, wage_lm_test )
      plots_test_list[[2*idx_model ]] <- nuage_res(residus_test, wage_test, wage_lm_test )
      
      idx_model = idx_model + 1
      
    }
  
    if ( "cart" %in% pred$models ) {
      
      model_cart_max = rpart(wage_train ~ . , control=rpart.control(cp=0), method = "anova", data = data_fifa_train)
      wage_cart_max_train = as.numeric(predict(model_cart_max))
      wage_cart_max_test = as.numeric(predict(model_cart_max , newdata=data_fifa_test))
      
      Cp=model_cart_max$cptable[which.min(model_cart_max$cptable[,4]),1]
      model_cart_opt = prune(model_cart_max, cp = Cp)
      wage_cart_opt_train = predict(model_cart_opt)
      wage_cart_opt_test = as.numeric(predict(model_cart_opt , newdata=data_fifa_test))
      
      colnames_pred <- append(colnames_pred, "Arbre CART Max")
      colnames_pred <- append(colnames_pred, "Arbre CART Optimal")
      
      res_app <- store_results( res_app, idx_model, wage_train, wage_cart_max_train ) 
      res_test <- store_results( res_test, idx_model, wage_test, wage_cart_max_test )
      
      residus_train = cbind( residus_train, wage_cart_max_train)
      residus_test = cbind( residus_test, wage_cart_max_test)
      
      plots_train_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_train, wage_train, wage_cart_max_train )
      plots_train_list[[2*idx_model ]] <- nuage_res(residus_train, wage_train, wage_cart_max_train )
      
      plots_test_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_test, wage_test, wage_cart_max_test )
      plots_test_list[[2*idx_model ]] <- nuage_res(residus_test, wage_test, wage_cart_max_test )
      
      idx_model = idx_model + 1
      
      res_app <- store_results( res_app, idx_model, wage_train, wage_cart_opt_train ) 
      res_test <- store_results( res_test, idx_model, wage_test, wage_cart_opt_test )
      
      residus_train = cbind( residus_train, wage_cart_opt_train)
      residus_test = cbind( residus_test, wage_cart_opt_test)
      
      plots_train_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_train, wage_train, wage_cart_opt_train )
      plots_train_list[[2*idx_model ]] <- nuage_res(residus_train, wage_train, wage_cart_opt_train )
      
      plots_test_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_test, wage_test, wage_cart_opt_test )
      plots_test_list[[2*idx_model ]] <- nuage_res(residus_test, wage_test, wage_cart_opt_test )

      idx_model = idx_model + 1
    
    }
    
    if ( "gam" %in% pred$models ) { 
      
      
      colnames_pred <- append(colnames_pred, "GAM")

      gam_formula = "wage_train ~ "
      
      for ( quant in seq_along(quantitatives_chosen )) 
        {
          if ( quant == 1 ) {
            
            gam_formula = paste(gam_formula," s(",quantitatives_chosen[quant],", k = -1 ) ")
          }
          else {
            gam_formula = paste(gam_formula,"+ s(",quantitatives_chosen[quant],", k = -1 )") # unique(data_fifa_train[c(quantitatives_chosen[[quant]])])
          } 
        }
      
      for ( quali in seq_along(qualitatives_chosen) ) 
        {
          if ( quali == 1 && length(quantitatives_chosen) == 1) {
            gam_formula = paste(gam_formula," + ",quantitatives_chosen[quant],"", sep= " ") # unique(data_fifa_train[c(quantitatives_chosen[[quant]])])
          }
          else {
            gam_formula = paste(gam_formula," + ",qualitatives_chosen[quali],"", sep= " ")
          }
        }
      
      model_gam = gam( formula = eval(as.formula(gam_formula)), data=data_fifa_train )
      
      wage_gam_train = predict(model_gam)
      wage_gam_test = predict(model_gam, newdata=data_fifa_test)
      
      res_app <- store_results( res_app, idx_model, wage_train, wage_gam_train ) 
      res_test <- store_results( res_test, idx_model, wage_test, wage_gam_test )
      
      residus_train = cbind( residus_train, wage_gam_train)
      residus_test = cbind( residus_test, wage_gam_test)
      
      plots_train_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_train, wage_train, wage_gam_train )
      plots_train_list[[2*idx_model ]] <- nuage_res(residus_train, wage_train, wage_gam_train )
      
      plots_test_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_test, wage_test, wage_gam_test )
      plots_test_list[[2*idx_model ]] <- nuage_res(residus_test, wage_test, wage_gam_test )
      
      idx_model = idx_model + 1

      
    }
    
    if ( "svr" %in% pred$models ) { 
      
      fifa19_pred_svr <-cbind.data.frame( fifa19_pred_quantitatives[unlist(quantitatives_chosen)], dummy.data.frame(fifa19_pred_qualitatives[unlist(qualitatives_chosen)]))
    
      data_fifa_train_svr = fifa19_pred_svr[indices_fifa,]
      data_fifa_test_svr = fifa19_pred_svr[-indices_fifa,] 
      
      colnames_pred <- append(colnames_pred, "SVR Noyau Linéaire")
      colnames_pred <- append(colnames_pred, "SVR Noyau Radial")
      
      model_svr_lin = svm(formula = wage_train ~ ., data = data_fifa_train_svr, epsilon = 0.025, cost=10,type = "eps-regression", kernel="linear")
      model_svr_rad = svm(formula = wage_train ~ ., data = data_fifa_train_svr, epsilon = 0.025, cost=10,type = "eps-regression", kernel="radial")

      wage_svr_lin_train = as.numeric(predict(model_svr_lin, newdata=data_fifa_train_svr))
      wage_svr_lin_test = as.numeric(predict(model_svr_lin, newdata=data_fifa_test_svr))
        
      res_app <- store_results( res_app, idx_model, wage_train, wage_svr_lin_train ) 
      res_test <- store_results( res_test, idx_model, wage_test, wage_svr_lin_test )
      
      residus_train = cbind( residus_train, wage_svr_lin_train)
      residus_test = cbind( residus_test, wage_svr_lin_test)
      
      plots_train_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_train, wage_train, wage_svr_lin_train )
      plots_train_list[[2*idx_model ]] <- nuage_res(residus_train, wage_train, wage_svr_lin_train )
      
      plots_test_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_test, wage_test, wage_svr_lin_test )
      plots_test_list[[2*idx_model ]] <- nuage_res(residus_test, wage_test, wage_svr_lin_test )
       
      idx_model = idx_model + 1
      
      wage_svr_rad_train = as.numeric(predict(model_svr_rad, newdata=data_fifa_train_svr))
      wage_svr_rad_test = as.numeric(predict(model_svr_rad, newdata=data_fifa_test_svr))      
      
      res_app <- store_results( res_app, idx_model, wage_train, wage_svr_rad_train ) 
      res_test <- store_results( res_test, idx_model, wage_test, wage_svr_rad_test )
      
      residus_train = cbind( residus_train, wage_svr_rad_train)
      residus_test = cbind( residus_test, wage_svr_rad_test)
      
      plots_train_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_train, wage_train, wage_svr_rad_train )
      plots_train_list[[2*idx_model ]] <- nuage_res(residus_train, wage_train, wage_svr_rad_train )
      
      plots_test_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_test, wage_test, wage_svr_rad_test )
      plots_test_list[[2*idx_model ]] <- nuage_res(residus_test, wage_test, wage_svr_rad_test )
       
      idx_model = idx_model + 1
      
    }
    
    if ( "rf" %in% pred$models ) { 
      
      model_rf = randomForest(wage_train ~., importance=TRUE, data=data_fifa_train, ntree= 200)
      wage_rf_train = as.numeric(predict(model_rf, newdata=data_fifa_train))
      wage_rf_test = as.numeric(predict(model_rf, newdata=data_fifa_test))
      
      colnames_pred <- append(colnames_pred, "RandomForest")

      res_app <- store_results( res_app, idx_model, wage_train, wage_rf_train ) 
      res_test <- store_results( res_test, idx_model, wage_test, wage_rf_test )
      
      residus_train = cbind( residus_train, wage_rf_train)
      residus_test = cbind( residus_test, wage_rf_test)
      
      plots_train_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_train, wage_train, wage_rf_train )
      plots_train_list[[2*idx_model ]] <- nuage_res(residus_train, wage_train, wage_rf_train )
      
      plots_test_list[[(2*idx_model) - 1 ]] <- obs_prev(residus_test, wage_test, wage_rf_test )
      plots_test_list[[2*idx_model ]] <- nuage_res(residus_test, wage_test, wage_rf_test )
      
      idx_model = idx_model + 1
      
    }
    
    updateRadioButtons( 
      session, "model_res",
      label = "Modele",
      choices = colnames_pred
    )
    
    dimnames(res_app) = dimnames(res_test) = list(c("MAE", "RMSE", "Variance Expliquee", "R2"),colnames_pred)
  
    res_app = t(res_app)
    res_test = t(res_test)
    
    first_pred <<- FALSE
    
    res_app_global <<- res_app
    res_test_global <<- res_test
    plots_train_list_global <<- plots_train_list
    plots_test_list_global <<- plots_test_list
    nb_model <<- n_model
    dict_index <<- hash( colnames_pred, 1:length(colnames_pred))

    output$barplot_metrics <- renderPlot(barplot(res_app[,1:2], beside = T, col = brewer.pal(n = n_model, name = "Spectral")), width = 700)
    output$obs_prev_lm <- renderPlot( plot_grid(plotlist = plots_train_list[1:2], ncol = 2), width = 750) 

    show("obs_prev_lm")
    show("barplot_metrics")
  })
  
  
  observeEvent({input$metric
               input$set_left}, { 
                 if (input$metric == "ev")
                 { if (input$set_left == "train")
                   {
                   output$barplot_metrics <- renderPlot(barplot(res_app_global[,3:4], beside = T, col = brewer.pal(n = nb_model, name = "Spectral")), width = 700)
                   }
                   else
                   {
                   output$barplot_metrics <- renderPlot(barplot(res_test_global[,3:4], beside = T, col = brewer.pal(n = nb_model, name = "Spectral")), width = 700)    
                   }
                 }
                 else
                 { if (input$set_left == "train")
                   {
                   output$barplot_metrics <- renderPlot(barplot(res_app_global[,1:2], beside = T, col = brewer.pal(n = nb_model, name = "Spectral")), width = 700)
                   }
                   else
                   {
                   output$barplot_metrics <- renderPlot(barplot(res_test_global[,1:2], beside = T, col = brewer.pal(n = nb_model, name = "Spectral")), width = 700)
                   }
                 }
              } )
  
  observeEvent({input$model_res
                input$set_right},
               {
                 # if (!first_pred)
                 #   {
                 #     print(typeof(input$model_res))
                 #     print(typeof(toString(input$model_res)))
                 #     print(input$model_res)
                 #   }
                 
                 if (input$set_right == "train")
                 {
                   output$obs_prev_lm <- renderPlot( plot_grid(plotlist = plots_train_list_global[(2*dict_index[[input$model_res]]-1) :(2*dict_index[[input$model_res]])], ncol = 2), width = 750)
                 }
                 else
                 {
                   output$obs_prev_lm <- renderPlot( plot_grid(plotlist = plots_test_list_global[(2*dict_index[[input$model_res]]-1) :(2*dict_index[[input$model_res]])], ncol = 2), width = 750)
                 }

               }
               )
  
  output$distPlot2 <- renderPlot({
    updated_data <- over_slider[over_slider$Overall>=input$overall[1]&over_slider$Overall<=input$overall[2],]
    plot(updated_data$Overall,updated_data$Age,xlab = "Overall", ylab = "Age of player", main = "test")
  })
  
  output$map <- renderLeaflet({
    # visitedMap <- joinCountryData2Map(world_map, 
    #                                   joinCode = "NAME",
    #                                   nameJoinColumn = "country",
    #                                   verbose = TRUE)
    # mapParams <- mapCountryData(visitedMap, 
    #                             nameColumnToPlot="Freq",
    #                             oceanCol = "azure2",
    #                             catMethod = "categorical",
    #                             missingCountryCol = gray(.8),
    #                             colourPalette = c("coral",
    #                                               "coral2",
    #                                               "coral3", "orangered", 
    #                                               "orangered3", "orangered4"),
    #                             addLegend = FALSE,
    #                             mapTitle = "Repartition des joueurs dans le monde",
    #                             border = NA)
    # 
    # do.call(addMapLegendBoxes, c(mapParams,
    #                              x = 'bottom',
    #                              title = "No. of visits",
    #                              horiz = TRUE,
    #                              bg = "transparent",
    #                              bty = "n"))
    
    # mapParams <- mapCountryData(visitedMap,
    #                             nameColumnToPlot="Freq",
    #                             oceanCol = "lightblue",
    #                             missingCountryCol = "white",
    #                             addLegend=FALSE)
    #do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2))
    # Initialize the leaflet map with the leaflet() function
    # Then we Add default OpenStreetMap map tiles
    # Same stuff but using the %>% operator
    #   m <- leaflet::addTiles(m)
    #   m <- leaflet() %>%  addProviderTiles("Esri.OceanBasemap")
    #   plot(m)
    # m <- leaflet()
    # m <- addTiles(m)
    #leaflet() %>%  addProviderTiles("Esri.OceanBasemap")
    
    
    map <- read.csv(file = "country.csv",sep=";")
    names(map)[names(map) == "Freq"] <- "n"
    data(world.cities)
    df2 <- world.cities %>%
      filter(capital == 1) %>%
      dplyr::select(country = country.etc, lat, lng = long) %>%
      left_join(map, ., by = "country")
    # now map the result
    leaflet(df2) %>% addTiles()%>% addMarkers(label = ~n) 
    
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
