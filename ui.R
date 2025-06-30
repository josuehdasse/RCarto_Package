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
source("R/modules/mod_mise_en_page.R")
source("R/modules/mod_gestionnaire_carte.R")

shinyUI(
  navbarPage(
             useShinyjs(),#On appelle le packahe ShinyJs
              #theme=theme_app,
             title="Générateur de cartes thématiques",
             tabPanel("A propos"),
             tabPanel("Application", width="100%",
                      withTags(
                        fluidRow( class="cadre_general_app container-fluid",

                            column(width=3,class="zones_gauche",


                                   fluidRow(
                                     tabBox(id="box_gestion_data",
                                            tabPanel("Couches", value = "couches_map",  #Apel du module de la gestion des couches
                                                     mod_gestion_couches_ui("map_ggplot")  ),

                                            tabPanel("Objets", value = "objets_graph", gestionnaire_objet_carte_ui("map_ggplot") ),#le gestionnaire d'objets

                                            tabPanel("Mise en page", value = "mise_en_page", mise_en_page_ui("map_ggplot") ),#

                                            tabPanel("Carte", value = "carte", uiOutput("ca_ui") ),#Gestionnaire de la carte

                                            width = 12,
                                            height = "100%"
                                     )
                                   )






                            ),
                            column(width=9,class="zones",
                                fluidRow(id="commandes_gestion_graph",
                                  mod_controle_impression_ui("map_ggplot")
                                ),
                                fluidRow(class="zone_travail",#GEstion de l'afficage graphique
                                  mod_impression_carte_ui("map_ggplot")
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



