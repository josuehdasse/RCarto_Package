#module de la gestion des option du controle de la zone d'ipression des cartes
library(shiny)

mod_controle_impression_ui <- function(id){
  ns<-NS(id)
    tagList(
      actionButton(ns("export_png"), "", icon = icon("glyphicon glyphicon-download-alt", lib = "glyphicon"), class="btn-primary btn-sm"),
      actionButton(ns("add_image"), "", icon = icon("glyphicon glyphicon-picture", lib = "glyphicon"), class="btn-primary btn-sm"),

      actionButton(ns("add_map"), "", icon = icon("file-pdf-o"), class="btn-primary btn-sm"),

      actionButton(ns("add_cadre"), "", icon = icon("object-group"), class="btn-primary btn-sm"),


    )

}

mod_controle_impression_server <- function(input, output, session) {
  ns<- session$ns


}
