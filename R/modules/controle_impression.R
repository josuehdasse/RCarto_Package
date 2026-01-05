#module de la gestion des option du controle de la zone d'ipression des cartes
library(shiny)


#Les fonctions utiles au module
creer_nouvel_objet <- function(
    type_objet, #le type de l'objet
    liste_objets,#la liste des objets existants (actif)
    xmin=0,xmax,ymin=0,ymax,

    objet_graphique=NULL,#objet ggplot a représenter
    code_texte_objet=NULL#le code texte de l'objet
    ){

    #on contruit l'objet à ajouter à la liste
    nouvel_objet_liste <- option_defaut_objets_composeurs
    #on personnalise l'objet
    nouvel_objet_liste$objet <-objet_graphique#
    nouvel_objet_liste$texte_objet <- code_texte_objet#ok
    nouvel_objet_liste$visible <- TRUE#l'objet vient par defaut avec le mode visible#ok
    nouvel_objet_liste$ordre <- length(liste_objets)+1 #ok
    nouvel_objet_liste$type<- type_objet#ok
    nouvel_objet_liste$label <- paste0("carte", length(liste_objets)+1 )#ok
    nouvel_objet_liste$name <-  paste0("obg_", length(liste_objets) +1 )#ok

    #La position de l'objet
    nouvel_objet_liste$positions$xmin <- xmin
    nouvel_objet_liste$positions$xmax = xmax
    nouvel_objet_liste$positions$ymin <- ymin
    nouvel_objet_liste$positions$ymax <- ymax

    return(nouvel_objet_liste)
}



mod_controle_impression_ui <- function(id){
  ns<-NS(id)
    tagList(
      uiOutput(ns("afficher_code_graph_ui")),

      downloadButton(outputId = ns("export_png"), label = "PNG"),#exporter l'image au format png
      downloadButton(outputId = ns("export_pdf"), label = "PDF"),#Exporter l'image au format pdf
      #actionButton(ns("export_png"), "", icon = icon("glyphicon glyphicon-download-alt", lib = "glyphicon"), class="btn-primary btn-sm"),
      actionButton(ns("add_image"), "", icon = icon("glyphicon glyphicon-picture", lib = "glyphicon"), class="btn-primary btn-sm"),
      actionButton(ns("add_text"), "", icon = icon("glyphicon glyphicon-font", lib = "glyphicon"), class="btn-primary btn-sm"),

      uiOutput(ns("add_map_ui")),

      actionButton(ns("add_cadre"), "", icon = icon("object-group"), class="btn-primary btn-sm")

    )

}

mod_controle_impression_server <- function(input, output, session, liste_objets_mise_en_page, liste_couches, largeur_page_actif, hauteur_page_actif,resolution_page_actif, box_zone_carte  ) {
  ns<- session$ns


  #les objets reactifs
  code_graphique <- reactive({

    ## On renvoie le code de sortie#######################
    ### Elaboration du code
    liste_objets=liste_objets_mise_en_page()#clonage d l'objet

    #le graphique de base de la représentation
    texte_base_graph <- paste0( "base_graph <- ggplot() +
                    coord_equal(xlim = c(0, ",largeur_page_actif(), "), ylim = c(0, ",hauteur_page_actif(),"), expand = TRUE)" )#Travailler avec une image HD sur 16:9


    code_assemblage_graph <- ""


    if (length(liste_objets)>=1) {

      for (i in 1:length(liste_objets)) {

        #on prend les informations sur les themes
        statut_cadre= liste_objets[[i]]$statut_cadre
        statut_grille= liste_objets[[i]]$statut_grille


        #on édite  le code pour prendre en compte les parametres de la grille
        intervalleX <- liste_objets[[i]]$grille$intervalleX
        intervalleY <- liste_objets[[i]]$grille$intervalleY

        #on retient les couches visibles de la carte
        couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches() )#Filter est une fonction de base de R et non celui de tidyverse

        graph_obj <- generer_map(couches_visibles, liste_objets[[i]]$emprise_carte ) #on va après voir comment rendre le thème dynamique

        #La gestion de l'échelle
        texte_scale=paste0('scale_x_continuous(breaks=seq(', floor(box_zone_carte()$xmin),', ',floor(box_zone_carte()$xmax),',by= ',intervalleX,') ) +\n scale_y_continuous(breaks=seq(',floor(box_zone_carte()$ymin),', ',floor(box_zone_carte()$ymax),',by= ',intervalleY,') ) ')

        #le thème grahique d'affiche de l'objet (dynamique)
        theme_graph=generer_theme_objets_cartes(statut_cadre,  statut_grille,   liste_objets[[i]]$cadre$PanelBackground, liste_objets[[i]]$cadre$PanelborderColor,
                                                liste_objets[[i]]$cadre$PanelborderSize,  liste_objets[[i]]$cadre$PanelLinetype, liste_objets[[i]]$grille$gridColour, liste_objets[[i]]$grille$gridLinetype,
                                                liste_objets[[i]]$grille$gridSizeLine, liste_objets[[i]]$grille$EspacementCadre  )


        #contrôle de la partie code qui dépend du code
        if(statut_grille){
              texte_sf <- paste0('coord_sf(crs=st_crs(4326),
                                          datum=st_crs(4326),
                                          label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
                                          xlim=c(',floor(box_zone_carte()$xmin),', ',floor(box_zone_carte()$xmax),' ),
                                          ylim=c(',floor(box_zone_carte()$ymin),', ',floor(box_zone_carte()$ymax),' )
                                          )')

              #le code texte de l'objet
              code_graphique <- paste0("graph_",i,"<- ",
                                       paste(graph_obj$code_couches_symbologie, texte_sf ,  texte_scale ,  sep = "+\n")
              )

        }else{

            #le code texte de l'objet
            code_graphique <- paste0("graph_",i,"<- ",
                                     paste(graph_obj$code_couches_symbologie ,  texte_scale ,  sep = "+\n")
            )

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


            code_graphique <- paste(code_graphique,
                                    paste0("#Partie du Code pour l'annotion de la fleche Nord", "\n", couche_fleche_nord),
                                    sep="+\n")

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

          code_graphique <- paste(code_graphique,
                                  paste("#Partie du code pour l'annotation d'échelle", "\n", couche_echelle),
                                  sep="+\n")
        }


        #l'objet graphique à représenter directement
        #liste_objets[[i]]$objet <- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph ))

        #le code final
        code_graphique <- paste0(graph_obj$code_data, "\n\n",
                                 "#Code de la carte", "\n",
                                 paste(code_graphique,theme_graph, sep = "+\n" ))

        #assemblage du graphique
        texte_graph <- paste0( "graph <- combiner_cartes(base_graph, graph_",i," , xmin = ",liste_objets[[i]]$positions$xmin,", xmax = ",liste_objets[[i]]$positions$xmax,", ymin = ",liste_objets[[i]]$positions$ymin,", ymax = ",liste_objets[[i]]$positions$ymax," )" )


        #gestion de l'assemblage
        if(i==1){
          code_assemblage_graph <-  paste0(

            paste0("#La carte de base des repères"), "\n",
            texte_base_graph, "\n\n\n",

            paste0("#Le code pour l'objet graphique ", i), "\n",
            code_graphique

            #paste0("#assemblage avec la carte de base de l'objet graphique ", i), "\n",
            #texte_graph
          )
        }else if(i>1){
          code_assemblage_graph <-  paste0(
            code_assemblage_graph, "\n",

            paste0("#Le code pour l'objet graphique ", i), "\n",
            code_graphique

            #paste0("#assemblage avec la carte de base de l'objet graphique ", i), "\n",
            #texte_graph
          )

        }



      }



    }#fin de la gestion de la boucle des objets du composeur graphique


    #boucle pour l'assemblage

    if(length(liste_objets)==1){
      boucle_assemblage <-  paste0("
         graph <- combiner_cartes(base_graph, graph_",i," , xmin = ",liste_objets[[i]]$positions$xmin,", xmax = ",liste_objets[[i]]$positions$xmax,", ymin = ",liste_objets[[i]]$positions$ymin,", ymax = ",liste_objets[[i]]$positions$ymax," )
         ")

      code_assemblage_graph <- paste0(
        code_assemblage_graph, "\n\n",

        paste0("#Assemblage des objets graphqiques créees"),"\n",
        boucle_assemblage
      )

    }else if(length(liste_objets)>1){

      boucle_assemblage_1 <-  paste0("graph <- combiner_cartes(base_graph, graph_1 , xmin = ",liste_objets[[1]]$positions$xmin,
                                     ", xmax = ",liste_objets[[1]]$positions$xmax,", ymin = ",liste_objets[[1]]$positions$ymin,", ymax = ",liste_objets[[1]]$positions$ymax," )")


      for (i in 2:length(liste_objets)) {
        boucle_assemblage <-  paste0("graph <- combiner_cartes(graph, graph_",i," , xmin = ",liste_objets[[i]]$positions$xmin,
                                       ", xmax = ",liste_objets[[i]]$positions$xmax,", ymin = ",liste_objets[[i]]$positions$ymin,", ymax = ",liste_objets[[i]]$positions$ymax," )")


        code_assemblage_graph <- paste0(
          code_assemblage_graph, "\n\n",

          paste0("#Assemblage des objets graphqiques créees"),"\n",
          boucle_assemblage_1, "\n",
          boucle_assemblage
        )

      }



    }



    code_assemblage_graph




  })




  #l'objet graphique reactif pour l'export
  graphique <- reactive({

    #on construit le graphique à nouveau
    liste_objets <- liste_objets_mise_en_page()

    graph <- ggplot() +
                    coord_equal(xlim = c(0 ,largeur_page_actif() ), ylim = c(0,hauteur_page_actif()), expand = TRUE) #Travailler avec une image HD sur 16:9


    #observe({#debut observe


    if (length(liste_objets)>=1) {

      for (i in 1:length(liste_objets)) {

        #On gère au cas par cas selon les types d'objets

        #on prend les informations sur les themes
        statut_cadre= liste_objets[[i]]$statut_cadre
        statut_grille= liste_objets[[i]]$statut_grille


        #on édite  le code pour prendre en compte les parametres de la grille
        intervalleX <- liste_objets[[i]]$grille$intervalleX
        intervalleY <- liste_objets[[i]]$grille$intervalleY

        #on retient les couches visibles de la carte
        couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches() )#Filter est une fonction de base de R et non celui de tidyverse


        #on generer le box de l'emprise de la carte
        box_emprise = st_polygon(
          list(
            rbind(
                c(liste_objets[[i]]$emprise_carte$xmin, liste_objets[[i]]$emprise_carte$ymin),
                c(liste_objets[[i]]$emprise_carte$xmin, liste_objets[[i]]$emprise_carte$xmax),
                c(liste_objets[[i]]$emprise_carte$xmax, liste_objets[[i]]$emprise_carte$ymax),
                c(liste_objets[[i]]$emprise_carte$xmax, liste_objets[[i]]$emprise_carte$ymin),
                c(liste_objets[[i]]$emprise_carte$xmin, liste_objets[[i]]$emprise_carte$ymin)
              )
          )
        ) %>% st_sfc() %>% st_as_sf() %>% st_bbox()

        graph_obj <- generer_map(couches_visibles, liste_objets[[i]]$emprise_carte ) #on va après voir comment rendre le thème dynamique

        texte_scale=paste0('
                           scale_x_continuous(breaks=seq(', floor(box_emprise$xmin),', ',floor(box_emprise$xmax),',by= ',intervalleX,') ) +
                           scale_y_continuous(breaks=seq(',floor(box_emprise$ymin),', ',floor(box_emprise$ymax),',by= ',intervalleY,') )')


          #le thème grahique d'affiche de l'objet (dynamique) = >> cadre
          theme_graph=generer_theme_objets_cartes(statut_cadre,  statut_grille,   liste_objets[[i]]$cadre$PanelBackground, liste_objets[[i]]$cadre$PanelborderColor,
                                                  liste_objets[[i]]$cadre$PanelborderSize,  liste_objets[[i]]$cadre$PanelLinetype, liste_objets[[i]]$grille$gridColour, liste_objets[[i]]$grille$gridLinetype,
                                                  liste_objets[[i]]$grille$gridSizeLine, liste_objets[[i]]$grille$EspacementCadre  )

          #exception sur l'affichage ou non de la grille de la carte
          if(statut_grille){
              texte_sf <- paste0('coord_sf(
                                  crs=st_crs(4326),
                                  datum=st_crs(4326),
                                  label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
                                  xlim=c(',floor(box_emprise$xmin),', ',floor(box_emprise$xmax),' ),
                                  ylim=c(',floor(box_emprise$ymin),', ',floor(box_emprise$ymax),' )
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


        #l'objet graphique à représenter directement
        liste_objets[[i]]$objet <- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph ))

        carte<- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph ))

        #On intègre les box ici
        carte <- generer_code_data_box_couches(carte, couches_visibles)

        graph <- combiner_cartes(graph, carte , xmin = liste_objets[[i]]$xmin, xmax = liste_objets[[i]]$xmax, ymin = liste_objets[[i]]$ymin, ymax = liste_objets[[i]]$ymax )
      }




    }


    graph


  })



  #Gestion de l'ajout d'une carte au graphique ######
  output$add_map_ui <- renderUI({
    req(length(liste_couches()) >=1)
    actionButton(ns("add_map"), "", icon = icon("map"), class="btn-primary btn-sm")
  })

  #Ajout d'un objet de type texte au composeur################
  observeEvent(input$add_text, {
    req(input$add_text)

    #On coie la liste des objets du composeur
    copie_liste_objets_map <- liste_objets_mise_en_page()

    #on construit l'objet à ajouter à la liste
    nouvel_objet_liste <- creer_nouvel_objet(
      type_objet = "texte", #le type de l'objet
      liste_objets=copie_liste_objets_map,#Copie de la liste des objets
      xmin=0,xmax=largeur_page_actif(),ymin=0,ymax=hauteur_page_actif(),

    )



  })


  #ajout d'un objet de type carte au composeur########################
  observeEvent(input$add_map, {
    req(input$add_map)
    req(length(liste_couches()) >=1)

    copie_liste_objets_map <- liste_objets_mise_en_page()


    #print(liste_couches)


    #on retient les couches visibles de la carte
    couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches() )#Filter est une fonction de b

    #couches_visibles_app <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de base de R
    #le graphique ici (on produit une version finalisée du graphique pour la présentation)


    graph_obj <- generer_map(couches_visibles, box_zone_carte() ) #on va après voir comment rendre le thème dynamique

    #on a mintenant l'objet grapgique
    graph<- eval(parse(text = graph_obj$code_graphique ))



    #on construit l'objet à ajouter à la liste
    nouvel_objet_liste <- creer_nouvel_objet(
                          type_objet = "carte", #le type de l'objet
                          liste_objets=copie_liste_objets_map,#Copie de la liste des objets
                          xmin=0,xmax=largeur_page_actif(),ymin=0,ymax=hauteur_page_actif(),
                          objet_graphique=graph,#objet ggplot a représenter
                          code_texte_objet=graph_obj#le code texte de l'objet
                          )

        #On gère les informations sur l'emprise de l'objet graphique
        nouvel_objet_liste$emprise_carte$xmin <- box_zone_carte()$xmin
        nouvel_objet_liste$emprise_carte$ymin <- box_zone_carte()$ymin
        nouvel_objet_liste$emprise_carte$xmax <- box_zone_carte()$xmax
        nouvel_objet_liste$emprise_carte$ymax <- box_zone_carte()$ymax

    copie_liste_objets_map <- append(copie_liste_objets_map, list(nouvel_objet_liste ))
    names(copie_liste_objets_map)[length(copie_liste_objets_map)] <-  paste0("obg_", length(copie_liste_objets_map) )

    ##On ajoute l'ojet crée à la liste des objets
    liste_objets_mise_en_page(copie_liste_objets_map)

  })





  #Exportation de l'image au format png ########
  output$export_png <- downloadHandler(

    #Spécication du nom du fichier exporté
    filename=function(){
      paste0("carte_png.png")
    },

    content=function(file){


      ggsave(
        file,
        plot = graphique(),
        device = "png",
        width = largeur_page_actif()/2.54,
        height = hauteur_page_actif()/2.54,
        dpi = resolution_page_actif()+50,
        bg = "#FFFFFF00",
        limitsize = FALSE
      )
     # png(file, width =largeur_page_actif()/resolution_page_actif(), height =hauteur_page_actif()/resolution_page_actif(), res = resolution_page_actif(), units = "in",  bg="white" )
      #plot(graphique())
      #dev.off()

    }#fin content
  )





  #Exportation de l'image au format PDF ########
  output$export_pdf <- downloadHandler(

    #Spécication du nom du fichier exporté
    filename=function(){
      paste0("carte_pdf.pdf")
    },

    content=function(file){

      ggsave(
        file,
        plot = graphique(),
        device = "pdf",
        width = largeur_page_actif()/2.54,
        height = hauteur_page_actif()/2.54,
        dpi = resolution_page_actif()+50,
        bg = "#FFFFFF00",
        limitsize = FALSE
      )
      # png(file, width =largeur_page_actif()/resolution_page_actif(), height =hauteur_page_actif()/resolution_page_actif(), res = resolution_page_actif(), units = "in",  bg="white" )
      #plot(graphique())
      #dev.off()

    }#fin content
  )






  #Gestion du code du graphique #####
  output$afficher_code_graph_ui <- renderUI({
    req(length(liste_objets_mise_en_page())>=1)
    actionButton(ns("afficher_code_graph"), "", icon = icon("terminal"), class="btn-primary btn-sm")
  })


  observeEvent(input$afficher_code_graph,{
    req(input$afficher_code_graph)


    showModal(modalDialog(
      title = "Code du rendu de la carte",
      footer=tagList(
        div(class="div_footer_modal",
            modalButton("Fermer")
        )

      ),
      fluidRow(style="display:inline-block;",
        withSpinner(
          #uiOutput(ns("sortie_code"))
          uiOutput(ns("sortie_code"))

          )
      ),#on paramètre le contenu de la fenêtre cible ici
      #trigger = "ajouter_couche",
      size="l",
      style="width:90%; margin:auto; padding:0;",
      easyClose = TRUE
    ))


  })




  output$sortie_code <- renderUI({


    tags$pre(
      style="background-color:#f8f9fa; padding:10px; border-radius:5px; position:relative; ",
      code_graphique(),
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


    clipr::write_clip(code_graphique())
    showNotification("Code copié !", type = "message")
  }

  )







}
