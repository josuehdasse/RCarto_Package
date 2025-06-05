#Ui de gestion des cartes thématiques avec-ggplot
#charger les dépendances (fonctions, données, packages)
source("R/dependances.R")

theme_app <- bs_theme(
  version = 5,
  bg="white",
  fg="white",
  primary = "#606060"
)

#Association des fichiers modules
source("R/modules/gestion_couches.R")
source("R/modules/controle_impression.R")

source("R/modules/impression_carte.R")
source("R/modules/rendu_code.R")


shinyUI(
  navbarPage(
              #theme=theme_app,
             title="Générateur de cartes thématiques",
             tabPanel("A propos"),
             tabPanel("Application",
                      withTags(
                        fluidRow( class="cadre_general_app container-fluid",

                            column(width=4,class="zones",
                                #Apel du module de la gestion des couches
                                mod_gestion_couches_ui("map_ggplot")
                            ),
                            column(width=8,class="zones",
                                fluidRow(class="zone_travail",#Gestion des controles
                                  mod_controle_impression_ui("map_ggplot")
                                ),
                                fluidRow(class="zone_travail",#GEstion de l'afficage graphique
                                  mod_impression_carte_ui("map_ggplot")
                                ),
                                fluidRow(class="zone_travail",#Gestion de l'affichage du code
                                  mod_rendu_code_ui("map_ggplot")
                                )

                            )

                        )

                      )#fin withTags

             ), #fin tabPanel

             selected = "Application",
             position = "fixed-top",
             includeCSS("www/css/style.css"),
             tags$script(src="js/fonctions.js"),
             tags$script(src="js/script.js"),





  )
)



