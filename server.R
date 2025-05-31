#charger les dépendances (fonctions, données, packages)
source("R/dependances.R")


liste_crs <- list(
  "WGS84 (long/lat)"="4326",
  "NAD83 (Amérique du Nord)"="4269",
  "Lambert 93 (France)"="2154",
  "Web Mercator"="3857",
  "OSGB36 (Royaume Uni)"="27700",
  "Lammbert II étendu"="27572",
  "UTM zone 31 N (Nord France)"="32631",
  "ESRI Robinson (projection du monde)"="54030"
)

print(names(liste_crs))

shinyServer(
  function(input, output, session){

    liste_couches <- reactiveVal(
      list()
    )


    #on met un observateur sur la liste des couches afin de déclencehr des actions relatives
    observeEvent(liste_couches(),{
      #on envoie la liste des couches à javascript
      session$sendCustomMessage("liste_couches", liste_couches() )

    })

    #gestion de l'affichage du modal de l'ajout d'une couche
    observeEvent(input$ajouter_couche,{

      #afficher le modal
      showModal(modalDialog(
        title = "Importer une couche",
        footer=tagList(
          div(class="div_footer_modal",
            uiOutput("bouton_confirmer_ajout_layer_ui"),
            modalButton("Annuler")
          )


        ),
        fluidRow(
          withSpinner(
            uiOutput("options_import_layer_ui") )
        ),#on paramètre le contenu de la fenêtre cible ici
        #trigger = "ajouter_couche",
        size="l",
        easyClose = TRUE
      ))

    })



    #Timer réactif pour l'envoi automatique des données
    envoiAutomatique <- reactiveTimer(1000)



    #gestin de l'envoi automatique des données


    #Contenu de la fenêtre modale de la gestion des couches à importer
    output$options_import_layer_ui <- renderUI({

      print("liste des couches ici au clic 2")

      #supprimer ceux qui sont déja dans la liste
      liste_couches_final <- setdiff(
        ls(envir = globalenv() ) , names(liste_couches())
        )

      print(liste_couches_final)

      #print("Je sui spassé")

        tagList(
          div(class="col-md-12",
              #sélection des couches sf à importer
              div(
                class="form-group",
                div(
                  div(class="col-md-3",
                      tags$label("Sélection de la couche "),
                  ),
                  div(class="col-md-9",
                      selectInput("select_couche", label = NULL, choices = liste_couches_final  )
                  )
                )

              ),

              #Choix du CRS

              uiOutput("select_projection_ui")

          )
        )




    })


    observeEvent(input$select_couche, {

      #print(paste0("select couche :", input$select_couche ))
      couche <- get(input$select_couche)

      #validation du type
      if(inherits(couche, "sf")){

        #test du crs
        if (is.na(st_crs(couche))) {
          output$select_projection_ui <- renderUI({
            withTags(
              div(
                class="form-group",
                div(
                  div(class="col-md-3",
                      tags$label("Projection de la couche "),
                  ),
                  div(class="col-md-9",
                      selectInput("select_projection", label = NULL, choices = liste_crs, selected = 4326  )
                  )
                )

              )
            )
          })
        }else{
          output$select_projection_ui <- renderUI({
            withTags(
              div(
                class="form-group",
                div(
                  div(class="col-md-3",
                      tags$label("Projection de la couche "),
                  ),
                  div(class="col-md-9",
                      selectInput("select_projection", label = NULL, choices = liste_crs, selected = st_crs(couche)$epsg   )
                  )
                )

              )
            )
          })

        }

      }else{

        #on désactive le bouton d'importation
        output$bouton_confirmer_ajout_layer_ui <- renderUI({

        })

        output$select_projection_ui <- renderUI({
          p("L'objet sélectionné n'est pas de type sf", style="color:red;")
        })
      }

    })



    #affichage du bouton d'importation de la couche si la projection est choisie
    observeEvent(input$select_projection,{

      output$bouton_confirmer_ajout_layer_ui <- renderUI({
        actionButton("confirmer_ajout_layer", "Importer", class="btn-success")
      })
    })


    #Gestion de l'importation maintenant
    observeEvent(input$confirmer_ajout_layer,{
      #déroulement de l'importation de la couche
      ##La couche est directement importée dans le format avec une symbologie unique qui prend les couleurs du système par défaut
      couche_courant <- get(input$select_couche)

      #nmbre_elements dans la liste des couches
      nbre_couches_ajoutes <- length(liste_couches())

      print(input$select_projection)

      #application du crs
      if(is.na(st_crs(couche_courant))){
        couche_courant <- couche_courant %>% st_set_crs(as.numeric(input$select_projection) )
      }else{
        couche_courant <- couche_courant %>%   st_transform( st_crs(as.numeric(input$select_projection) ) )
      }

      #création de la liste
      couche_courant =list(
        couche= couche_courant,
        crs=as.numeric(input$select_projection),
        type_symbologie="Symbole unique",
        geometrie= unique(as.character(st_geometry_type(couche_courant))), #on controle la geometrie pour gerer la crte plus tard (point, ligne, polygone, etc)
        options_symbologie_couhe=list(
          options_symbologie_unique=list(#Gestion des symboles uniques de la couche
            couleur_symbole= liste_couleurs[nbre_couches_ajoutes+1],
            legende=input$select_couche,
            couleur_trait="#000000",
            style_trait="solid",
            epaisseur_trait=1

          )
          #la suite des symbologies
        ),
        position_couche = nbre_couches_ajoutes+1
      )

      #on crémente la couche
      mes_couches <- append(liste_couches(), list(couche_courant))
      names(mes_couches)[nbre_couches_ajoutes+1] <- input$select_couche

      #actualiser la valeur réactive
      liste_couches(mes_couches)

      zones_carte <- reunion_couches(liste_couches())
      box_zone_carte <- zones_carte %>% st_bbox()#le box

      #On imprime la sortie de la carte (pour toutes les couches présentes dans l'application)
      output$sortie_carte_ui <- renderImage({


        #tire la carte
        graph_obj <- generer_map(liste_couches() ) #on va après voir comment rendre le thème dynamique
        graph<- eval(parse(text = graph_obj$code_graphique ))
        ratio_hauteur_graph <- graph_obj$ratio_hauteur

        #essi d'impression du code
        print(graph)

            #les dimensions de la carte à imprimer
            largeur_box <- sqrt( (box_zone_carte$xmax- box_zone_carte$xmin)^2   )
            longeur_box <- sqrt( (box_zone_carte$ymax- box_zone_carte$ymin)^2   )

            #estimation de la taille des polygones
            ratio=largeur_box/longeur_box

            #on essaie de produire une carte qui se rapproche le plus possible de la dimension 16:9
            largeur=12
            hauteur=largeur/ratio

            #la différence de hauteur du graphique
            diff_hauteur_obj <- ratio_hauteur_graph*longeur_box
            diff_largeur_obj <- ratio*diff_hauteur_obj


        #la zone d'impression de la carte
        zone_impression <- impression_carte(orientation_carte = "paysage", largeur_dimension = largeur, hauteur_dimension = hauteur, theme_carte = theme_graphique)

        #on procède à l'ajout des éléments à la zone d'impression
        ## la carte principale
        mon_graphique <- combiner_cartes(zone_impression, graph, xmin = -0.8, xmax = largeur+0.8, ymin = 0, ymax = hauteur+0.2 )


        outfile<- tempfile(fileext = "png")


        pixelratio <- session$clientData$pixelratio
        #dimensions automatique de l'image png
        width <- session$clientData$output_sortie_carte_ui_width*pixelratio#+75
        #width <- session$clientData[[paste0("output_", ns("carte_severite_besoins"), "_width")]]#+75  #dans le module

        #width<- 815*ratio

        height <- width/ratio

        print(paste("width : ", width, "height : ", height, "pixelratio:", pixelratio,  sep = " " ))

        #La hauteur
        #if(session$clientData$output_carte_severite_besoins_height*pixelratio==0){
        #height <- 815
        #}else{
        #height <- session$clientData$output_carte_severite_besoins_height*pixelratio
        #}

        #print(paste0("Hauteur :", height, " largeur :", width))

        png(outfile, width =width, height =height, res = 95 )
        print(mon_graphique)
        dev.off()
        list(src=outfile)

      }, deleteFile=TRUE) #fin impression première carte du rendu


      #fermeture du modal après traitement
      removeModal()

    })


    #on ferme la connexion du serveur à la fin de la session (websocket)
    #session$onSessionEnded(function(){
      #ws$close()

    #})

  }
)
