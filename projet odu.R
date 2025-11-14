####  --------- partie bibliothèque -----------

library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(cluster)
library(dendextend)
library(plotly)

####  --------- partie nettoyage des données -----------

data <- read_xlsx("les-francais-et-l-information-arcom-2024-base-anonymisee.xlsx")

data <- data |> 
  mutate(
    sexe = factor(RS1_R, levels = c(1, 2), labels = c("Homme", "Femme")),
    age = factor(RS2C_RECODE_AG_R, levels = c(1, 2, 3, 4, 5, 6), 
                 labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65 et plus"))
  )

####  --------- partie utilisateur -----------

ui <- dashboardPage(
  dashboardHeader(
    title = "Enquête sur l'audiovisuel",
    titleWidth = 400
  ),
  
####  --------- partie barre latérale -----------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Informations générales", tabName = "Info", icon = icon("home")),
      menuItem("Graphique", icon = icon("chart-bar"),
               menuSubItem("Télévision", tabName = "television"),
               menuSubItem("Radio", tabName = "radio"),
               menuSubItem("Journaux", tabName = "journaux"),
               menuSubItem("Réseaux sociaux", tabName = "reseaux_sociaux")),
      menuItem("Visualisation de donnée : ACM", tabName = "acm", icon = icon("chart-line")),
      menuItem("Catégorisation non supervisée", tabName = "cat_non_sup", icon = icon("sitemap"),
               menuSubItem("catégorisation hiérarchique", tabName = "cat_hierar"),
               menuSubItem("Cluster envisagé", tabName = "clustering")),
      menuItem("k-means", tabName = "kmeans", icon = icon("th")),
      menuItem("Annexe", tabName = "annexe", icon = icon("info-circle"))
    )
  ),
  
####  --------- partie body -----------
  dashboardBody(
    tabItems(
      tabItem(tabName = "Info",
              h2("Informations générales sur l'enquête"),
              fluidRow(
                box(title = "À quelle fréquence vous informez-vous ?", 
                    status = "primary", solidHeader = TRUE, width = 4),
                box(title = "À quelle fréquence vous informez-vous ?", 
                    status = "primary", solidHeader = TRUE, width = 4),
                box(title = "Au cours des 12 derniers mois, avez-vous payé pour vous informer ?", 
                    status = "primary", solidHeader = TRUE, width = 4)
      )),
      
      tabItem(tabName = "television",
              h2("Graphiques Télévision"),
              selectInput("choix_sexe", 
                          label = "Choisissez le sexe :", 
                          choices = levels(data$sexe),
                          selected = levels(data$sexe)[1]),
              checkboxGroupInput("choix_age",
                                 label = "Choisissez les tranches d'âge :",
                                 choices = levels(data$age),
                                 selected = levels(data$age)),
              fluidRow(
                box(title = "Vous informez-vous via la télévision ?", 
                    status = "primary", solidHeader = TRUE, width = 6),
                box(title = "Par quelles chaînes de télévision vous informez-vous ?", 
                    status = "primary", solidHeader = TRUE, width = 6)
              )
      ),
      
      tabItem(tabName = "radio",
              h2("Graphiques Radio"),
              selectInput("choix_sexe", 
                          label = "Choisissez le sexe :", 
                          choices = levels(data$sexe),
                          selected = levels(data$sexe)[1]),
              checkboxGroupInput("choix_age",
                                 label = "Choisissez les tranches d'âge :",
                                 choices = levels(data$age),
                                 selected = levels(data$age)),
              fluidRow(
                box(title = "Vous informez-vous via la radio / des podcasts ?", 
                    status = "primary", solidHeader = TRUE, width = 6),
                box(title = "Quelles radios / podcasts consultez-vous pour vous informer ?", 
                    status = "primary", solidHeader = TRUE, width = 6)
              )
      ),
      
      tabItem(tabName = "journaux",
              h2("Graphiques Journaux"),
              selectInput("choix_sexe", 
                          label = "Choisissez le sexe :", 
                          choices = levels(data$sexe),
                          selected = levels(data$sexe)[1]),
              checkboxGroupInput("choix_age",
                                 label = "Choisissez les tranches d'âge :",
                                 choices = levels(data$age),
                                 selected = levels(data$age)),
              fluidRow(
                box(title = "Vous informez-vous via des journaux / magazines ?", 
                    status = "primary", solidHeader = TRUE, width = 6),
                box(title = "Quels titres de presse consultez-vous pour vous informer ?", 
                    status = "primary", solidHeader = TRUE, width = 6)
              )
      ),
      
      tabItem(tabName = "reseaux_sociaux",
              h2("Graphiques Réseaux Sociaux"),
              selectInput("choix_sexe", 
                          label = "Choisissez le sexe :", 
                          choices = levels(data$sexe),
                          selected = levels(data$sexe)[1]),
              checkboxGroupInput("choix_age",
                                 label = "Choisissez les tranches d'âge :",
                                 choices = levels(data$age),
                                 selected = levels(data$age)),
              fluidRow(
                box(title = "Vous informez-vous via au moins un réseau social / une plateforme ?", 
                    status = "primary", solidHeader = TRUE, width = 6),
                box(title = "Si oui : par quels réseaux / plateformes ?", 
                    status = "primary", solidHeader = TRUE, width = 6)
              )
      ),
      
      tabItem(tabName = "acm", h2("Analyse des Correspondances Multiples"),
              fluidRow(
                box(title = "Nuage des individus", 
                    status = "primary", solidHeader = TRUE, width = 6),
                box(title = "représentation des variables", 
                    status = "primary", solidHeader = TRUE, width = 6)),
              fluidRow(
                box(title = "graphique des contributions", 
                    status = "primary", solidHeader = TRUE, width = 6),
                box(title = "graphique des qualités de représentation",
                    status = "primary", solidHeader = TRUE, width = 6))),
      tabItem(tabName = "cat_non_sup"),
      tabItem(tabName = "cat_hierar", h2("Catégorisation hiérarchique")),
      tabItem(tabName = "clustering", h2("Clustering")),
      tabItem(tabName = "kmeans", h2("k-means")),
      tabItem(tabName = "annexe",
              h3("Base de données"),
              p("Cette base de donnée de l'ARCOM"),
              
              h3("Méthodologie"),
              p("Les données ont été collectées via un questionnaire par le biais de la méthode d'enquête 
                CAWI (Computer-Assisted Web Interview) et la méthode CATI (Computer-Assisted Telephone Interview)"),
              
              h3("Variables principales"),
              tags$ul(
                tags$li("FREQUENCE_INFO : fréquence à laquelle les individus s'informent"),
                tags$li("SEXE : genre des répondants"),
                tags$li("AGE : tranche d'âge"),
                tags$li("CONFIANCE_INFO : niveau de confiance dans les sources d'information")
              ),
              
              h3("Remarques"),
              p("Les résultats doivent être interprétés avec prudence, notamment en raison du biais de sélection lié à la diffusion numérique du questionnaire."),
              
              br(),
              em("Dernière mise à jour : novembre 2025")
      )
    )
  ))

####  --------- partie server -----------

server <- function(input, output) {
  
  data_filtrée <- reactive({
    data |> 
      filter(
        sexe %in% input$choix_sexe,
        age %in% input$choix_age
      )
  })
  
  output$graph_chaine <- renderPlotly({
    data_plot <- data_filtrée() |> 
      group_by(chaine_tv) |> 
      summarise(count = n())
    
    plot_ly(data_plot, x = ~chaine_tv, y = ~count, type = 'bar')
  })
}

####  --------- partie application -----------

shinyApp(ui = ui, server = server)
