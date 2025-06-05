#Control de l'impression de la carte
library(shiny)

mod_impression_carte_ui <- function(id){
    ns<- NS(id)

    tagList(
      imageOutput(ns("sortie_carte_ui"))
    )

}



mod_impression_carte_server <- function(input, output, session, graph, ratio){
    ns<- session$ns

    output$sortie_carte_ui <- renderImage({

      outfile<- tempfile(fileext = "png")
      pixelratio <- session$clientData$pixelratio
      #dimensions automatique de l'image png

      width <- session$clientData[[paste0("output_", ns("sortie_carte_ui"), "_width")]]#+75  #dans le module
      #width <- session$clientData$output_sortie_carte_ui_width*pixelratio#+75
      height <- width/ratio

      png(outfile, width =width, height =height, res = 95 )
      print(graph)
      dev.off()
      list(src=outfile)
    }, deleteFile=TRUE) #fin impression première carte du rendu

}
