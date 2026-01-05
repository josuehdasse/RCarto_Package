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

          graph_obj <- generer_map(couches_visibles, liste_objets[[i]]$emprise_carte) #on va après voir comment rendre le thème dynamique

          texte_scale=paste0('
                         scale_x_continuous(breaks=seq(', floor(liste_objets[[i]]$emprise_carte$xmin),', ',floor(liste_objets[[i]]$emprise_carte$xmax),',by= ',intervalleX,') ) +
                         scale_y_continuous(breaks=seq(',floor(liste_objets[[i]]$emprise_carte$ymin),', ',floor(liste_objets[[i]]$emprise_carte$ymax),',by= ',intervalleY,') )')



          #exception sur l'affichage ou non de la grille de la carte
          if(statut_grille){
            texte_sf <- paste0('coord_sf(
                                  crs=st_crs(4326),
                                  datum=st_crs(4326),
                                  label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
                                  xlim=c(',floor(liste_objets[[i]]$emprise_carte$xmin),', ',floor(liste_objets[[i]]$emprise_carte$xmax),' ),
                                  ylim=c(',floor(liste_objets[[i]]$emprise_carte$ymin),', ',floor(liste_objets[[i]]$emprise_carte$ymax),' )
                                 )')

            code_graphique_complet <- paste(graph_obj$code_graphique, texte_sf ,  texte_scale, sep = "+")

            #le code texte de l'objet
            liste_objets[[i]]$texte_objet <- paste(graph_obj$code_graphique, texte_sf ,  texte_scale,  sep = "+")

          }else{
            code_graphique_complet <- paste(graph_obj$code_graphique,  texte_scale, sep = "+")

            #le code texte de l'objet
            liste_objets[[i]]$texte_objet <- paste(graph_obj$code_graphique ,  texte_scale,  sep = "+")
          }



          #Gestion de la fleche nord
          statut_fleche_nord = liste_objets[[i]]$statut_fleche_nord

          if(statut_fleche_nord){

            width_fleche_nord <- liste_objets[[i]]$fleche_nord$width
            height_fleche_nord <- liste_objets[[i]]$fleche_nord$height
            location_fleche_nord <- liste_objets[[i]]$fleche_nord$location
            padx_fleche_nord <-liste_objets[[i]]$fleche_nord$pad_x
            pady_fleche_nord <- liste_objets[[i]]$fleche_nord$pad_y
            style_fleche_nord <- liste_objets[[i]]$fleche_nord$style

            couche_fleche_nord <- paste0('annotation_north_arrow(
             location = "',location_fleche_nord,'",
              width=unit(',width_fleche_nord,',"cm"),
              height=unit(',height_fleche_nord,',"cm"),
              pad_x = unit(',padx_fleche_nord,', "in"),
              pad_y = unit(',pady_fleche_nord,', "in"),
              style = ',style_fleche_nord,'
            )')


            code_graphique_complet <- paste(code_graphique_complet, couche_fleche_nord, sep="+\n")

          }


          #Gestion de l'échelle de la carte
          statut_echelle_carte = liste_objets[[i]]$statut_echelle
          if(statut_echelle_carte){
            width_hint <- liste_objets[[i]]$echelle$width_hint#ok
            height<- liste_objets[[i]]$echelle$height#ok
            location<- liste_objets[[i]]$echelle$location#ok
            pad_x <- liste_objets[[i]]$echelle$pad_x#ok
            pad_y <- liste_objets[[i]]$echelle$pad_y#ok
            style<- liste_objets[[i]]$echelle$style#ok
            unit<- liste_objets[[i]]$echelle$unit#ok
            text_cex<- liste_objets[[i]]$echelle$text_cex#ok
            text_col<- liste_objets[[i]]$echelle$text_col#ok

           if(is.na(width_hint)){
             width_hint=0.3
           }

           couche_echelle <-paste0('annotation_scale(#echelle
              location = "',location,'",
              width_hint = ',width_hint,',
              plot_unit = "',unit,'",
              height=unit(',height,',"cm"),
              pad_x = unit(',pad_x,', "cm"),
              pad_y = unit(',pad_y,', "cm"),
              text_cex = ',text_cex,',
              text_col = "',text_col,'",
              style = "',style,'")')


              code_graphique_complet <- paste(code_graphique_complet, couche_echelle, sep="+\n")
          }


          #le thème grahique d'affiche de l'objet (dynamique)
          theme_graph=generer_theme_objets_cartes(statut_cadre,  statut_grille,   liste_objets[[i]]$cadre$PanelBackground, liste_objets[[i]]$cadre$PanelborderColor,
                                                  liste_objets[[i]]$cadre$PanelborderSize,  liste_objets[[i]]$cadre$PanelLinetype, liste_objets[[i]]$grille$gridColour, liste_objets[[i]]$grille$gridLinetype,
                                                  liste_objets[[i]]$grille$gridSizeLine, liste_objets[[i]]$grille$EspacementCadre  )


          #l'objet graphique à représenter directement
          #liste_objets[[i]]$objet <- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph )


          carte<- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph ))

          #On intègre les valeurs des box et leurs contenus#########
          carte <- generer_code_data_box_couches(carte, couches_visibles)

          graph <- combiner_cartes(graph, carte , xmin = liste_objets[[i]]$position$xmin, xmax = liste_objets[[i]]$position$xmax, ymin = liste_objets[[i]]$position$ymin, ymax = liste_objets[[i]]$position$ymax )



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


        png(outfile, width =0.80*width, height =0.8*height, res = resolution_page_actif-85 )
        print(graph)
        dev.off()
        list(src=outfile)
      }, deleteFile=TRUE) #fin impression première carte du rendu







    #})#Fin observe



}
