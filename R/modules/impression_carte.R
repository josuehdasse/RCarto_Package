#Control de l'impression de la carte
library(shiny)

mod_impression_carte_ui <- function(id){
    ns<- NS(id)

    tagList(
      fluidRow(style="height:auto !important; display: relative; width:100%; margin:0;",
          imageOutput(ns("sortie_carte_ui"), fill = TRUE)
      )
    )

}



mod_impression_carte_server <- function(input, output, session, liste_objets_mise_en_page, page, hauteur=210, largeur=297, resolution_page_actif=300, liste_couches, box_zone_carte ){
    ns<- session$ns

    liste_objets <- liste_objets_mise_en_page

    graph=page


    #observe({#debut observe


      if (length(liste_objets)>=1) {

        for (i in 1:length(liste_objets)) {

          #on prend les informations sur les themes
          statut_cadre= liste_objets[[i]]$statut_cadre
          statut_grille= liste_objets[[i]]$statut_grille


          #on édite  le code pour prendre en compte les parametres de la grille
          intervalleX <- liste_objets[[i]]$grille$intervalleX
          intervalleY <- liste_objets[[i]]$grille$intervalleY

          #on retient les couches visibles de la carte
          couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches )#Filter est une fonction de base de R et non celui de tidyverse

          graph_obj <- generer_map(couches_visibles) #on va après voir comment rendre le thème dynamique

          texte_scale=paste0('
                         scale_x_continuous(breaks=seq(', floor(box_zone_carte$xmin),', ',floor(box_zone_carte$xmax),',by= ',intervalleX,') ) +
                         scale_y_continuous(breaks=seq(',floor(box_zone_carte$ymin),', ',floor(box_zone_carte$ymax),',by= ',intervalleY,') )
                         ')

          texte_sf <- paste0('coord_sf(
                            crs=st_crs(4326),
                            datum=st_crs(4326),
                            label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
                            xlim=c(',floor(box_zone_carte$xmin),', ',floor(box_zone_carte$xmax),' ),
                            ylim=c(',floor(box_zone_carte$ymin),', ',floor(box_zone_carte$ymax),' )
                           )')

          code_graphique_complet <- paste(graph_obj$code_graphique, texte_sf ,  texte_scale, sep = "+")


          #le thème grahique d'affiche de l'objet (dynamique)
          theme_graph=generer_theme_objets_cartes(statut_cadre,  statut_grille,   liste_objets[[i]]$cadre$PanelBackground, liste_objets[[i]]$cadre$PanelborderColor,
                                                  liste_objets[[i]]$cadre$PanelborderSize,  liste_objets[[i]]$cadre$PanelLinetype, liste_objets[[i]]$grille$gridColour, liste_objets[[i]]$grille$gridLinetype,
                                                  liste_objets[[i]]$grille$gridSizeLine, liste_objets[[i]]$grille$EspacementCadre  )



          #l'objet graphique à représenter directement
          liste_objets[[i]]$objet <- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph ))

          #le code texte de l'objet
          liste_objets[[i]]$texte_objet <- paste(graph_obj$code_graphique, texte_sf ,  texte_scale, theme_graph,  sep = "+")



          carte<- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph ))

          graph <- combiner_cartes(graph, carte , xmin = liste_objets[[i]]$xmin, xmax = liste_objets[[i]]$xmax, ymin = liste_objets[[i]]$ymin, ymax = liste_objets[[i]]$ymax )
        }




      }


      #on représente l'objet graphique
      proportion=largeur/hauteur


      output$sortie_carte_ui <- renderImage({

        outfile<- tempfile(fileext = "png")
        pixelratio <- session$clientData$pixelratio
        #dimensions automatique de l'image png

        width <- session$clientData[[paste0("output_", ns("sortie_carte_ui"), "_width")]]*pixelratio#+75  #dans le module
        #width <- session$clientData$output_sortie_carte_ui_width*pixelratio#+75
        #height <- session$clientData[[paste0("output_", ns("sortie_carte_ui"), "_height")]]
        height<- (width / proportion)*pixelratio

        #print(resolution_page_actif)


        png(outfile, width =width, height =height, res = resolution_page_actif )
        print(graph)
        dev.off()
        list(src=outfile)
      }, deleteFile=TRUE) #fin impression première carte du rendu







    #})#Fin observe



}
