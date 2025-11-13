# Bibliothèques
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

# --------- Chargement des données -----------#

data <- read_xlsx("les-francais-et-l-information-arcom-2024-base-anonymisee.xlsx")

data <- data |> 
  mutate(
    sexe = factor(RS1_R, levels = c(1, 2), labels = c("Homme", "Femme")),
    age = factor(RS2C_RECODE_AG_R, levels = c(1, 2, 3, 4, 5, 6), labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65 et plus"))
  )

# --------- partie UI -----------#


ui <- dashboardPage(
  dashboardHeader(
    title = "Enquête sur l'audiovisuel",
    titleWidth = 400
  ),

# --------- partie sidebar -----------#

  dashboardSidebar(
    sidebarMenu(
      menuItem("Informations générales", tabName = "Informations générales", icon = icon("home")),
      menuItem("Graphique", icon = icon("chart-bar"),
               menuSubItem("Télévision", tabName = "television"),
               menuSubItem("Radio", tabName = "radio"),
               menuSubItem("Journaux", tabName = "journaux"),
               menuSubItem("Réseaux sociaux", tabName = "reseaux_sociaux")),
      menuItem("Visualisation de donnée : ACP", tabName = "acp", icon = icon("chart-line")),
      menuItem("Catégorisation hiérarchique", tabName = "categorisation_hierarchique", icon = icon("sitemap")),
      menuItem("Clustering", tabName = "clustering", icon = icon("project-diagram")),
      menuItem("Annexe", tabName = "annexe", icon = icon("info-circle")))),
  
  
#---------- partie body -----------#
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Informations générales"),
      
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
                box(title = "Vous informez-vous via la télévision ?", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("gauge_tele")),
              fluidRow(
                box(title = "Par quelles chaînes de télévision vous informez-vous ?", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("graph_chaine")))
                  
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
                                 selected = levels(data$age))
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
                                 selected = levels(data$age))
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
                                 selected = levels(data$age))
      ),
      
      tabItem(tabName = "acp", h2("Analyse en Composantes Principales (ACP)")),
      tabItem(tabName = "categorisation_hierarchique", h2("Catégorisation hiérarchique")),
      tabItem(tabName = "clustering", h2("Clustering")),
      tabItem(tabName = "annexe", h2("Annexe"),
              tabItem(tabName = "annexe",
                      h3("Base de données"),
                      p("Cette base de donnée de l'ARCOM"),
                      
                      h3("Méthodologie"),
                      p("Les données ont été collectées via un questionnaire par le bais de la méthode d'enquête 
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
                      em("Dernière mise à jour : novembre 2025"))
              
              )
    )
  )
)




# --------- partie server -----------#

server <- function(input, output) {
  # Le serveur reste inchangé pour l'instant


data_filtrée <- reactive({
  data |> 
    filter(
      sexe %in% input$choix_sexe,
      age %in% input$choix_age
    )
})

renderPlotly({
  data_plot <- data_filtrée() |> 
    group_by(chaine_tv) |> 
    summarise(count = n())
  
  plot_ly(data_plot, x = ~chaine_tv, y = ~count, type = 'bar')})
}

# --------- Lancement de l'application -----------#

shinyApp(ui = ui, server = server)