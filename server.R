#charger les dépendances (fonctions, données, packages)
source("R/dependances.R")

shinyServer( function(input, output, session){

    # Element reactif pour la gestion des projets
    liste_projets <- reactiveVal(
      list()
    )

    #Element réactif pour la gestion des couches ##########
    liste_couches <- reactiveVal(
      list()
    )

    #liste des objets ajoutés à la mise en forme dela crte
    liste_objets_mise_en_page <- reactiveVal(
      list()
    )

    #reactifs pour la gestion de la mise en page
    hauteur_page_actif<-reactiveVal(210)
    largeur_page_actif<-reactiveVal(297)
    resolution_page_actif<- reactiveVal(100)
    fond_page_actif <- reactiveVal("#ffffff")
    orientation_page_actif<-reactiveVal("Paysage")

    #Informations sur le projet courant¨
    id_projet_actif <- reactiveVal(NULL)#on initialise
    name_projet_actif <- reactiveVal(NULL)#on initialise


    #on rend aussi dynamique la zone globale de la carte #####
    box_zone_carte <- reactive({
      if(length(liste_couches())>=1){
        base<- reunion_couches(liste_couches()) %>% st_bbox()#le box
      }
    })


    #on gère l'affichage des projets
    #TEs du nombr de porjets
    observeEvent(liste_projets(), {

      if(length(liste_projets())>0){#on a des projets enregistrés

        if(is.null(id_projet_actif)){#pas de projet selcêctionnés
          #on affiche la liste des projets

          output$ui_gestionnaire_projet <- renderUI({
            ListeProjetsUI()
          })


        }else{#cas ou un projet est sélectionnée déjà

          #on autorise la gestion des couches du projet
          output$ui_gestionnaire_projet <- renderUI({
            NULL
          })

          output$titre_projet <- renderText({
            name_projet_actif()
          })

          output$ui_gestionnaire_couches  <- renderUI({
            withTags(
              fluidRow( class="cadre_general_app container-fluid",

                        fluidRow(
                          column(width=3,class="zones_gauche",

                                 fluidRow(
                                   mod_gestion_couches_ui("map_ggplot")
                                 )

                          ),
                          column(width=9,class="zones",
                                 tagList(
                                   #uiOutput("visuel_couches_map_ui"),
                                   imageOutput("visuel_couches_map")
                                 )



                          )
                        )
              )

            )#fin withTags
          })

        }

      }else{#pas de prrojets
        #on charge le projet sans titres
        id_projet_actif(1)
        name_projet_actif("Projet sans titre")

        #on autorise la gestion des couches du projet
        output$ui_gestionnaire_projet <- renderUI({
          NULL
        })

        output$titre_projet <- renderText({
          name_projet_actif()
        })

        output$ui_gestionnaire_couches  <- renderUI({
          withTags(
            fluidRow( class="cadre_general_app container-fluid",

                        column(width=3,class="zones_gauche",

                               fluidRow(
                                 mod_gestion_couches_ui("map_ggplot")
                               )

                        ),
                        column(width=9,class="zones",

                               fluidRow(style="height:auto !important; display: relative; width:100%; margin:0;",
                                        imageOutput("visuel_couches_map",, fill = TRUE)
                               )


                        )

            )

          )#fin withTags
        })


      }


    })


    #la liste réactive des projets
    ListeProjetsUI <- reactive({
      ProjetsUI <- lapply(liste_projets(), function(i){
        tags$li(
          class="list_item",
          span(
            i$name,
            onclick="alert('ok')"
          )
        )

      })
    })






    #Appel du module de la gestion des couches
    callModule(mod_gestion_couches_server, "map_ggplot",id_projet_actif, liste_couches, resolution_page_actif )

    #on recupère les options de la mise en page
    options_mise_en_page <- callModule(mise_en_page_server, "map_ggplot", largeur_page_actif, hauteur_page_actif, resolution_page_actif, fond_page_actif, orientation_page_actif )


    #Module de gestion des options de contrôle
    #On lance la mise en page de la carte
    callModule(mod_controle_impression_server, "map_ggplot", liste_objets_mise_en_page, liste_couches, largeur_page_actif, hauteur_page_actif,resolution_page_actif,   box_zone_carte   )

    #Gestionnaire des objets de la carte
    callModule(gestionnaire_objet_carte_server, "map_ggplot",liste_objets_mise_en_page )


    #la liaison directe avec la carte produite
    observe({
      page_carte <- zone_impression_carte(orientation_carte = orientation_page_actif(), largeur_dimension =  largeur_page_actif(), hauteur_dimension =hauteur_page_actif(), fond = fond_page_actif()       )
      callModule(mod_impression_carte_server, "map_ggplot", liste_objets_mise_en_page(), page_carte,hauteur_page_actif(), largeur_page_actif(), resolution_page_actif(), liste_couches(), box_zone_carte() )

    })


    #suivi de l'action quan on modifie une id de projet
    observeEvent(id_projet_actif(), {
      req(!is.null(id_projet_actif()))

      req(length(liste_couches())>=1 )

      #on filtre les couches du projet
      couche_projet <- Filter( function(x) x$id_projet==id_projet_actif(), liste_couches())#Filter est une fonction de base de R

      liste_couches(couche_projet)
    })



    #on met un observateur sur la liste des couches afin de déclencehr des actions relatives ######
    observeEvent(liste_couches(),{

     # print("La liste est vide")

      if(length(liste_couches())>=1){

       #print("La liste n'est plus vide")
        #on envoie la liste des couches à javascript

        session$sendCustomMessage("liste_couches", liste_couches() )
        #on doit sélectionner spécialement les couches visibles
        #couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de b



        output$visuel_couches_map <- renderImage({


          #les dimensions de la carte à imprimer
          largeur_box <- sqrt( (box_zone_carte()$xmax- box_zone_carte()$xmin)^2   )
          longeur_box <- sqrt( (box_zone_carte()$ymax- box_zone_carte()$ymin)^2   )
          #estimation de la taille des polygones
          ratio=largeur_box/longeur_box

          couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de base de R
          #le graphique ici (on produit une version finalisée du graphique pour la présentation)


          graph_obj <- generer_map(couches_visibles) #on va après voir comment rendre le thème dynamique


          graph <-  eval(parse(text = graph_obj$code_graphique )) + eval(parse(text = theme_graphique  ))


          outfile<- tempfile(fileext = "png")

          #,hauteur_page_actif(), largeur_page_actif()
          proportion=largeur_page_actif()/hauteur_page_actif()

          pixelratio <- session$clientData$pixelratio

          width <- session$clientData[[paste0("output_", "visuel_couches_map", "_width")]]*pixelratio#+75  #dans le module
          #width <- session$clientData$output_sortie_carte_ui_width*pixelratio#+75
          #height <- session$clientData[[paste0("output_", ns("sortie_carte_ui"), "_height")]]
          height<- (width / proportion)*pixelratio

          png(outfile, width =0.8*width, height =0.8*height, res = resolution_page_actif()-85 )
          print(graph)
          dev.off()
          list(src=outfile)


        }, deleteFile=TRUE)





        #print(them)
        #data_graph<- finaliser_carte( couches_visibles_app, box_zone_carte(), theme_graphique )

        #les données graphiques à transmettre
        #graph<-  data_graph$mon_graphique #+eval(parse(text = theme_graphique  )) #on ajoute le thème ici

        #code_graph<- data_graph$code_graphique  #paste( data_graph$code_graphique, paste0(theme_graphique), sep = "+\n")
        #ratio<- data_graph$ratio

        #on n'actualise que si le nbre des couches est >0
        #on actualise aussi la représentation des couches séclectionnés
        #callModule(mod_impression_carte_server, "map_ggplot", graph, ratio)



        #Gestion de la carte source
        #callModule(gestionnaire_objet_carte_server,"map_ggplot", box_zone_carte() )


        #Gestion des options du contrôle grapgique


      }else{#fin contrôle sur la condition selon que le nombre de couches doit excéder 1

        output$visuel_couches_map_ui <- renderUI({
          fluidRow(
            p("Pas de couhes à afficher", style="color:red")
          )
        })

      }

    })





  })#fin de l'application


