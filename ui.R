#charger les dépendances (fonctions, données, packages)
source("R/dependances.R")

theme_app <- bs_theme(
  version = 5,
  bg="white",
  fg="white",
  primary = "#606060"
)




shinyUI(
  navbarPage(
              #theme=theme_app,
             title="Générateur de cartes thématiques",
             tabPanel("A propos"),
             tabPanel("Application",

                      withTags(
                        div(class="row col-md-12 cadre_general_app container-fluid",

                            tagList(
                              div(
                                class="col-md-4 zones",
                                tagList(
                                  div(class="col-md-12 controles",
                                      actionButton("ajouter_couche", "", icon = icon("add"), class="btn-primary btn-sm"),
                                      actionButton("supp_couche", "", icon = icon("trash"), class="btn-primary btn-sm")
                                  ),

                                  div(class="col-md-12 zone_travail", #liste ds couches
                                      tags$table(class="table table-responsive-md",
                                                 tags$caption(style="caption-side:top;text-align:left",
                                                              "Liste des couches"
                                                              ),
                                                 tags$thead(
                                                   tags$tr(
                                                     tags$th("#"),
                                                     tags$th("Symboles", scope="col"),
                                                     tags$th("Couches", scope="col"),
                                                     tags$th("Légende", scope="col"),
                                                     tags$th("Options", scope="col")
                                                   )
                                                 ),
                                                 tags$tbody(id="liste_couches_carte")

                                      )
                                  )
                                )
                              ),
                              div(
                                class="col-md-8 zones",
                                tagList(

                                  div(class="col-md-12 controles",
                                      p("contoles impression")
                                  ),

                                  div(class="col-md-12 zone_travail",
                                      div(class="col-md-12",
                                          imageOutput("sortie_carte_ui")
                                          ),
                                      div(class="col-md-12",
                                          uiOutput("sortie_code_ui")
                                      )
                                  )

                                )
                              )
                            )



                            )

                      )#fin withTags

             ), #fin tabPanel

             selected = "Application",
             position = "fixed-top",
             includeCSS("www/css/style.css"),
             tags$script(src="js/fonctions.js"),
             tags$script(src="js/script.js")





  )
)



