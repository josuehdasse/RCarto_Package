library(shiny)

mise_en_page_ui <- function(id){
  ns<- NS(id)
  tagList(
    uiOutput(ns("mise_en_page_ui"))
  )

}


mise_en_page_server <- function(input, output, session, largeur_page_actif, hauteur_page_actif, resolution_page_actif, fond_page_actif, orientation_page_actif ){
  ns<- session$ns


  format_page_actif<-reactiveVal("a4")




  #Gestion de la mis en page#####


  output$mise_en_page_ui <- renderUI({
    tagList(
      #format de la page
      fluidRow(class="form-group",
               column(width = 4,   tags$label("Format de la page")    ),
               column(width = 8,    selectInput(ns("select_format_page"), label = NULL, choices = liste_choix_options_pages, selected = format_page_actif() )      )
      ),

      #format de la page
      fluidRow(class="form-group",
               column(width = 4,   tags$label("Orientation")    ),
               column(width = 8,    selectInput(ns("select_orientation_page"), label = NULL, choices = c("Paysage", "Portrait"), selected = orientation_page_actif()  )      )
      ),

      uiOutput(ns("options_taille_page_ui")),

      fluidRow(class="form-group",
               column(width = 4,
                      tags$label("Résolution")
               ),
               column(width = 8,
                      numericInput(ns("select_resolution_page"), label = NULL, min = 0, max=NA, width = "100px", value =  resolution_page_actif() )
               )
      ),

      fluidRow(class="form-group",
               column(width = 4,    tags$label("Couleur de fond ")     ),
               column(width = 8, colourInput(ns("select_fond_page"), label = NULL, value=fond_page_actif() , allowTransparent = TRUE, palette = "square", closeOnClick = TRUE  )
               )
      )

    )
  })


  observeEvent(input$select_format_page, {
    req(input$select_format_page)

    format_page_actif(input$select_format_page)

    switch (input$select_orientation_page,
            "Paysage" = {
              min_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "min_taille" , sep = "$")  ))
              max_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "max_taille" , sep = "$")  ))

              hauteur_page_actif(min_taille)
              largeur_page_actif(max_taille)
            },

            "Portrait"={
              min_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "min_taille" , sep = "$")  ))
              max_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "max_taille" , sep = "$")  ))

              hauteur_page_actif(max_taille)
              largeur_page_actif(min_taille)
            }

    )



  })


  observeEvent(input$select_orientation_page, {
    orientation_page_actif(input$select_orientation_page)

    switch (orientation_page_actif(),
            "Paysage" = {
              min_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "min_taille" , sep = "$")  ))
              max_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "max_taille" , sep = "$")  ))

              hauteur_page_actif(min_taille)
              largeur_page_actif(max_taille)
            },

            "Portrait"={
              min_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "min_taille" , sep = "$")  ))
              max_taille <- eval(parse(text =  paste("options_pages", input$select_format_page, "max_taille" , sep = "$")  ))

              hauteur_page_actif(max_taille)
              largeur_page_actif(min_taille)
            }

    )


  })



  #Suic de la résolution
  observeEvent(input$select_resolution_page, {
    req(input$select_resolution_page)

    resolution_page_actif(input$select_resolution_page)

  })





  #affigae des taille des pages
  output$options_taille_page_ui <- renderUI({
    req(input$select_format_page)
    req(input$select_orientation_page)

    tagList(
      fluidRow(class="form-group",
               column(width = 4,
                      tags$label("Largeur")
               ),
               column(width = 8,
                      numericInput(ns("select_min_taille"), label = NULL, min = 0, max=NA, width = "100px", value =  largeur_page_actif() )
               )
      ),
      fluidRow(class="form-group",
               column(width = 4,
                      tags$label("Hauteur")
               ),
               column(width = 8,
                      numericInput(ns("select_hauteur_taille"), label = NULL, min = 0, max=NA, width = "100px", value =  hauteur_page_actif() )
               )
      ),
    )

  })


  observeEvent(input$select_min_taille,{
    largeur_page_actif(input$select_min_taille)
  })


  observeEvent(input$select_hauteur_taille,{
    if(input$select_hauteur_taille != hauteur_page_actif() ){
      hauteur_page_actif(input$select_hauteur_taille)
    }
  })

   observeEvent(input$select_min_taille,{
    largeur_page_actif(input$select_min_taille)
  })

   #suivi du fond actif d ela page
   observe({
     req(input$select_fond_page != fond_page_actif() )
     fond_page_actif(input$select_fond_page)
   })




  return(
    list(
      largeur= reactive(input$select_min_taille),
      hauteur=reactive(input$select_hauteur_taille),
      couleur_fond=reactive(input$select_fond_page),
      orientation=reactive(input$select_orientation_page),
      resolution = reactive(input$select_resolution_page)
    )
  )

}
