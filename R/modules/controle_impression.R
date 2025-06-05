#module de la gestion des option du controle de la zone d'ipression des cartes
library(shiny)

mod_controle_impression_ui <- function(id){
  ns<-NS(id)
    tagList(
      p("options du controle de la zone d'imoression")
    )

}

mod_controle_impression_server <- function(input, output, session) {
  ns<- session$ns


}
