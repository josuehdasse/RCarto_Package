#Generer les codes
library(shiny)
library(clipr)

mod_rendu_code_ui <- function(id){
  ns<- NS(id)
  tagList(
    uiOutput(ns("sortie_code"))
  )
}



mod_rendu_code_server <- function(input, output, session, code){
  ns<- session$ns



  output$sortie_code <- renderUI({

      tags$pre(
        style="background-color:#f8f9fa; padding:10px; border-radius:5px; position:relative; ",
        code,
        actionButton(
          ns("bouton_copier"),
          "",
          icon = icon("copy"),
          style = "position:abosolute; top:5px; right:5px;",
          title="copier le code"
        )
      )

  })


  observeEvent(input$bouton_copier, {
    clipr::write_clip(code)
    showNotification("Code copié !", type = "message")
  }

  )

}
