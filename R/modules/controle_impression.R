#module de la gestion des option du controle de la zone d'ipression des cartes
library(shiny)

mod_controle_impression_ui <- function(id){
  ns<-NS(id)
    tagList(
      uiOutput(ns("afficher_code_graph_ui")),

      downloadButton(outputId = ns("export_png"), label = "PNG"),#exporter l'image au format png
      downloadButton(outputId = ns("export_pdf"), label = "PDF"),#Exporter l'image au format pdf
      #actionButton(ns("export_png"), "", icon = icon("glyphicon glyphicon-download-alt", lib = "glyphicon"), class="btn-primary btn-sm"),
      actionButton(ns("add_image"), "", icon = icon("glyphicon glyphicon-picture", lib = "glyphicon"), class="btn-primary btn-sm"),

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

        graph_obj <- generer_map(couches_visibles) #on va après voir comment rendre le thème dynamique

        texte_scale=paste0('
                            scale_x_continuous(breaks=seq(', floor(box_zone_carte()$xmin),', ',floor(box_zone_carte()$xmax),',by= ',intervalleX,') ) +
                            scale_y_continuous(breaks=seq(',floor(box_zone_carte()$ymin),', ',floor(box_zone_carte()$ymax),',by= ',intervalleY,') ) ')

        texte_sf <- paste0('coord_sf(crs=st_crs(4326),
                                      datum=st_crs(4326),
                                      label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
                                      xlim=c(',floor(box_zone_carte()$xmin),', ',floor(box_zone_carte()$xmax),' ),
                                      ylim=c(',floor(box_zone_carte()$ymin),', ',floor(box_zone_carte()$ymax),' )
                                      )')




        #le thème grahique d'affiche de l'objet (dynamique)
        theme_graph=generer_theme_objets_cartes(statut_cadre,  statut_grille,   liste_objets[[i]]$cadre$PanelBackground, liste_objets[[i]]$cadre$PanelborderColor,
                                                liste_objets[[i]]$cadre$PanelborderSize,  liste_objets[[i]]$cadre$PanelLinetype, liste_objets[[i]]$grille$gridColour, liste_objets[[i]]$grille$gridLinetype,
                                                liste_objets[[i]]$grille$gridSizeLine, liste_objets[[i]]$grille$EspacementCadre  )



        #l'objet graphique à représenter directement
        #liste_objets[[i]]$objet <- eval(parse(text = code_graphique_complet  ))  + eval(parse(text = theme_graph ))

        #le code texte de l'objet
        code_graphique <- paste0("graph_",i,"<- ",
                                         paste(graph_obj$code_graphique, texte_sf ,  texte_scale, "\n" ,  sep = "+\n")
        )

        code_graphique <- paste0(code_graphique,theme_graph )


        #assemblage du graphique
        texte_graph <- paste0( "graph <- combiner_cartes(base_graph, graph_",i," , xmin = ",liste_objets[[i]]$xmin,", xmax = ",liste_objets[[i]]$xmax,", ymin = ",liste_objets[[i]]$ymin,", ymax = ",liste_objets[[i]]$ymax," )" )


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
         graph <- combiner_cartes(base_graph, graph_",i," , xmin = ",liste_objets[[i]]$xmin,", xmax = ",liste_objets[[i]]$xmax,", ymin = ",liste_objets[[i]]$ymin,", ymax = ",liste_objets[[i]]$ymax," )
         ")


      code_assemblage_graph <- paste0(
        code_assemblage_graph, "\n\n\n",

        paste0("#Assemblage des objets graphqiques créees"),"\n",
        boucle_assemblage
      )

    }else if(length(liste_objets)>1){

      boucle_assemblage_1 <-  paste0("graph <- combiner_cartes(base_graph, graph_1 , xmin = ",liste_objets[[1]]$xmin,
                                     ", xmax = ",liste_objets[[1]]$xmax,", ymin = ",liste_objets[[1]]$ymin,", ymax = ",liste_objets[[1]]$ymax," )")


      for (i in 2:length(liste_objets)) {
        boucle_assemblage <-  paste0("graph <- combiner_cartes(graph, graph_",i," , xmin = ",liste_objets[[i]]$xmin,
                                       ", xmax = ",liste_objets[[i]]$xmax,", ymin = ",liste_objets[[i]]$ymin,", ymax = ",liste_objets[[i]]$ymax," )")


        code_assemblage_graph <- paste0(
          code_assemblage_graph, "\n\n\n",

          paste0("#Assemblage des objets graphqiques créees"),"\n",
          boucle_assemblage_1, "\n",
          boucle_assemblage
        )

      }



    }



    code_assemblage_graph




  })




  #l'objet graphique reactif
  graphique <- reactive({

    #on construit le graphique à nouveau
    liste_objets <- liste_objets_mise_en_page()

    graph <- ggplot() +
                    coord_equal(xlim = c(0 ,largeur_page_actif() ), ylim = c(0,hauteur_page_actif()), expand = TRUE) #Travailler avec une image HD sur 16:9


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
        couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches() )#Filter est une fonction de base de R et non celui de tidyverse

        graph_obj <- generer_map(couches_visibles) #on va après voir comment rendre le thème dynamique

        texte_scale=paste0('
                           scale_x_continuous(breaks=seq(', floor(box_zone_carte()$xmin),', ',floor(box_zone_carte()$xmax),',by= ',intervalleX,') ) +
                           scale_y_continuous(breaks=seq(',floor(box_zone_carte()$ymin),', ',floor(box_zone_carte()$ymax),',by= ',intervalleY,') )
                           ')

        texte_sf <- paste0('coord_sf(
                              crs=st_crs(4326),
                              datum=st_crs(4326),
                              label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
                              xlim=c(',floor(box_zone_carte()$xmin),', ',floor(box_zone_carte()$xmax),' ),
                              ylim=c(',floor(box_zone_carte()$ymin),', ',floor(box_zone_carte()$ymax),' )
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


    graph


  })



  #Gestion de l'ajout d'une carte au graphique ######
  output$add_map_ui <- renderUI({
    req(length(liste_couches()) >=1)
    actionButton(ns("add_map"), "", icon = icon("map"), class="btn-primary btn-sm")
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


    graph_obj <- generer_map(couches_visibles) #on va après voir comment rendre le thème dynamique

    #on a mintenant l'objet grapgique
    graph<- eval(parse(text = graph_obj$code_graphique ))

    #on contruit l'objet à ajouter à la liste
    nouvel_objet_liste <- list(
      objet=graph,
      texte_objet=graph_obj,
      visible=TRUE,
      position=length(copie_liste_objets_map)+1,
      type="carte",  #image, rectangle, ellipse , zoom_pays, zoom_zone, texte, légende, fleche nord, echelle
      label=paste0("carte", length(copie_liste_objets_map)+1 ),
      name= paste0("obg_", length(copie_liste_objets_map) +1 ),
      xmin =0,
      xmax = largeur_page_actif() ,
      ymin =0,
      ymax =hauteur_page_actif(),
      statut_cadre=FALSE,
      cadre= list(
        PanelBackground="#ffffff",
        PanelborderColor="#000000",
        PanelborderSize=1,
        PanelLinetype="solid"
      ),
      statut_grille=FALSE,
      grille=list(
        intervalleX=1,
        intervalleY=1,
        gridColour="#DCDCDC",
        gridLinetype="solid",
        gridSizeLine=0.2,
        EspacementCadre=0
      )
    )


    #print(paste0("obg_", length(copie_liste_objets_map)+1 ) )

    copie_liste_objets_map <- append(copie_liste_objets_map, list(nouvel_objet_liste ))
    names(copie_liste_objets_map)[length(copie_liste_objets_map)] <-  paste0("obg_", length(copie_liste_objets_map) )

    #print(copie_liste_objets_map)
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
        dpi = resolution_page_actif()+10,
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
