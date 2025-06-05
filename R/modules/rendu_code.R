#Generer les codes
library(shiny)

mod_rendu_code_ui <- function(id){
  ns<- NS(id)
  tagList(
    verbatimTextOutput(ns("sortie_code"))
  )
}



mod_rendu_code_server <- function(input, output, session, code){
  ns<- session$ns

  output$sortie_code <- renderPrint({
    code
  })

}
