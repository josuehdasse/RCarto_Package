#module de la gestion des option du controle de la zone d'ipression des cartes
library(shiny)

mod_controle_impression_ui <- function(id){
  ns<-NS(id)
    tagList(
      uiOutput(ns("afficher_code_graph_ui")),

      actionButton(ns("export_png"), "", icon = icon("glyphicon glyphicon-download-alt", lib = "glyphicon"), class="btn-primary btn-sm"),
      actionButton(ns("add_image"), "", icon = icon("glyphicon glyphicon-picture", lib = "glyphicon"), class="btn-primary btn-sm"),

      uiOutput(ns("add_map_ui")),

      actionButton(ns("add_cadre"), "", icon = icon("object-group"), class="btn-primary btn-sm")

    )

}

mod_controle_impression_server <- function(input, output, session, liste_objets_mise_en_page, liste_couches, largeur_page_actif, hauteur_page_actif  ) {
  ns<- session$ns


  #Gestion de l'ajout d'une carte au graphique ######
  output$add_map_ui <- renderUI({
    req(length(liste_couches()) >=1)
    actionButton(ns("add_map"), "", icon = icon("map"), class="btn-primary btn-sm")
  })


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
        gridSizeLine=0.2
      )
    )


    #print(paste0("obg_", length(copie_liste_objets_map)+1 ) )

    copie_liste_objets_map <- append(copie_liste_objets_map, list(nouvel_objet_liste ))
    names(copie_liste_objets_map)[length(copie_liste_objets_map)] <-  paste0("obg_", length(copie_liste_objets_map) )

    #print(copie_liste_objets_map)
    liste_objets_mise_en_page(copie_liste_objets_map)



  })




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
          uiOutput(ns("sortie_code"))    )
      ),#on paramètre le contenu de la fenêtre cible ici
      #trigger = "ajouter_couche",
      size="l",
      style="width:90%; margin:auto; padding:0;",
      easyClose = TRUE
    ))

  })

    ## On renvoie le code de sortie
    output$sortie_code <- renderUI({

      tags$pre(
        style="background-color:#f8f9fa; padding:10px;  ",
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

      })




}
