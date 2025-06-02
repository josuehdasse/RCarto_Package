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
    #Element réactif pour la gestion des couches #####
    liste_couches <- reactiveVal(
      list()
    )

    #on rend aussi dynamique la zone globale de la carte #####
    box_zone_carte <- reactive({
      base<- reunion_couches(liste_couches()) %>% st_bbox()#le box
    })


    #Gestion des données recues chez JS pour les couleurs des couches uniques ######
    donnees_couleurs_couche_unique <- reactive({
      if(!is.null(input$couleur_unique)){
        input$couleur_unique
      }
    })

    observeEvent(donnees_couleurs_couche_unique(),{
      #traitement de la modification des couleurs avec la nouvelle couleur reçue depuis JavaScript
      data_couleur<- fromJSON(input$couleur_unique)
      name_couche <-data_couleur$name
      nouvelle_couleur <- data_couleur$couleur

      copie_couche <- liste_couches()
      type_symbologie <- eval(parse(text = paste("copie_couche", name_couche ,"type_symbologie", sep="$") ))

      if(type_symbologie=="unique"){

        partie_gauche <- paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","couleur_symbole",  sep="$")

        requete <- paste0( partie_gauche , " <- '",nouvelle_couleur, "'" )

        eval(parse(text = requete ))
      }

      #actualisation de la couche
      liste_couches(copie_couche)
    })


    #Gestion de l'activation et de désactivation des couches#####
    donnees_activation_couche <- reactive({
      if(!is.null(input$select_activation_couche)){
        input$select_activation_couche
      }
    })

    observeEvent(donnees_activation_couche(),{
      #traitement de la modification des couleurs avec la nouvelle couleur reçue depuis JavaScript
      data_activation<- fromJSON(input$select_activation_couche)
      name_couche <-data_activation$name
      valeur_activation <- data_activation$activation
      #duplication de la couche
      copie_couche <- liste_couches()

      partie_gauche <- paste("copie_couche", name_couche ,"visible", sep="$")

      print(paste0(partie_gauche, "<-", paste(valeur_activation) ))

      #if(valeur_activation=="on")

      eval(parse(text=paste0(partie_gauche, "<-", paste0(valeur_activation) )))

      liste_couches(copie_couche)

    })


    #Gestion de la symbologie d'une couche#############
    donnees_symbologie_couche <- reactive({
      if(!is.null(input$select_option_symbologie_couche)){
        input$select_option_symbologie_couche
      }
    })

    ##Tracking des éléments caractéristques de la symbologie d'une couche####
      mame_couche_actif <- reactiveVal(#le nom de la couche active
        NULL
      )


      type_symbologie_actif <- reactiveVal(
        NULL
      )

      couleur_remplissage_symbole_actif <- reactiveVal(
        NULL
      )

      opacity_fill_actif <- reactiveVal(
        NULL
      )

      opacity_border_actif <- reactiveVal(
        NULL
      )

      style_trait_actif <- reactiveVal(
        NULL
      )

      style_fill_symbologie_actif <- reactiveVal(
        NULL
      )

      couleur_trait_actif <- reactiveVal(
        NULL
      )

      epaisseur_trait_actif <- reactiveVal(
        NULL
      )


    ##Ecoute des changements qui peuvent provenir de JS##########
    observeEvent(donnees_symbologie_couche(), {
      data_symbologie <- fromJSON(input$select_option_symbologie_couche)
      name_couche <- data_symbologie$name
      mame_couche_actif(name_couche)

      #duplication de la couche
      copie_couche <- liste_couches()

        #On actualise les valeurs de la symbologie suivant la  couche sélectionnée
        type_symbologie <- eval(parse(text = paste("copie_couche", name_couche ,"type_symbologie", sep="$") ))
          type_symbologie_actif(type_symbologie)

          #cas des valeurs
          #if(type_symbologie_actif()=="unique"){#cas du symbole unique
              couleur_fill <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","couleur_symbole",  sep="$") ))
              couleur_remplissage_symbole_actif(couleur_fill)


              opacity_fill <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","opacity_fill",  sep="$") ))
              opacity_fill_actif(opacity_fill)


              opacity_border <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","opacity_border",  sep="$") ))
              opacity_border_actif(opacity_border)

              style_trait <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","style_trait",  sep="$") ))
              style_trait_actif(style_trait)

              style_fill_symbologie <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","style_fill_symbologie",  sep="$") ))
              style_fill_symbologie_actif(style_fill_symbologie)

              couleur_trait <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","couleur_trait",  sep="$") ))
              couleur_trait_actif(couleur_trait)

              epaisseur_trait <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","epaisseur_trait",  sep="$") ))
              epaisseur_trait_actif(epaisseur_trait)




          #}


      #on donne accès à la fenêtre modale permettant de gérer les options de la symbologie de la couche courante
      showModal(modalDialog(
        title = paste0("Options de la symbologie de la couche ", name_couche),
        footer=tagList(
          div(class="div_footer_modal",
              uiOutput("bouton_appliquer_symbologie_couche_ui"),
              modalButton("Annuler")
          )
        ),
        fluidRow(
          withSpinner(
            uiOutput("options_symbologie_layer_ui") )
        ),#on paramètre le contenu de la fenêtre cible ici
        #trigger = "ajouter_couche",
        size="l",
        easyClose = TRUE
      ))
    })

        ###contenu des options de gestion de la symbologie####
        output$options_symbologie_layer_ui <- renderUI({
          withTags(
            div(class="col-md-12",
                uiOutput("type_symbole"),#le choix de la symbologie (pour actualiser dans la liste plus tard)
                uiOutput("options_symbologie_ui"),#gestionnaire du contrôle  de l'apparence des symboles
                uiOutput("palette_couleurs_ui"),#la palette des couleurs à utiliser pour le dégradé des catégories
                uiOutput("controle_classes_categories_ui"),
                uiOutput("classes_indicateurs"),
                uiOutput("labels_classes_indicateurs")
            )
          )
        })

            ####le choix du type symbologie de l'utilisateur (avec comme valeur de base la valeur existante) #####
            output$type_symbole <- renderUI({
              withTags(
                div(
                  class="form-group",
                  div(
                    div(class="col-md-3",
                        tags$label("Type de symbologie"),
                    ),

                    div(class="col-md-9",
                        selectInput("selec_type_symbole", NULL, choices = list("symbole unique"="unique", "catégorisé"="categorie", "gradué"="graduate"), selected = type_symbologie_actif())
                    )
                  )
                )
              )

            })


            ####on remplit les caractéristiques de la gestion des options de controle des symboles####
            output$options_symbologie_ui <- renderUI({

              if(!is.null(input$selec_type_symbole)){
                if( input$selec_type_symbole=="unique"){##
                  withTags(
                    div(
                      class="row options_symbologie_ui",
                      tagList(


                        #sélection des couleurs de la symbologie
                        div(
                          class="form-group",
                          div(
                            div(class="col-md-3",
                                tags$label("Couleur de remplissage "),
                            ),

                            div(class="col-md-7",
                                tags$input(type="color", onchange="fonction_color_symboble_unique(this.value)", id="select_couleur_symbole_color", style="width:100%; height:30px;", value=couleur_remplissage_symbole_actif() )
                            ),
                            div(class="col-md-2",
                               textInput("select_couleur_symbole", label = NULL, width='100px', value= couleur_remplissage_symbole_actif())
                            )
                          )
                        ),

                        #Opacité de la couleur de remplissage de la symbologie de la couche
                        div(
                          class="form-group",
                          div(
                            div(class="col-md-3",
                                tags$label("Opacité de remplissage "),
                            ),
                            div(class="col-md-9",
                                sliderInput("select_opacity_fill", label = NULL, min = 0, max = 1, sep = 0.1, value=opacity_fill_actif() )
                            )
                          )
                        ),


                        #Style de remplissage
                        div(
                          class="form-group",
                          div(
                            div(class="col-md-3",
                                tags$label("Style de remplissage "),
                            ),
                            div(class="col-md-9",
                                selectInput("select_style_fill_symbologie", label = NULL, choices = list("Continue"="continu",
                                                                                                         "Pas de remplissage"="blank",
                                                                                                         "Motif"="motif"),  selected = style_fill_symbologie_actif() )
                            )
                          )
                        ),


                        #Couleur de trait
                        div(
                          class="form-group",
                          div(
                            div(class="col-md-3",
                                tags$label("Couleur de trait"),
                            ),
                            div(class="col-md-7",
                                tags$input(type="color", id="select_couleur_trait_color",onchange="fonction_color_trait_unique(this.value)", style="width:100%; height:30px;" , class="shiny-bound-input", value=couleur_trait_actif() )
                            ),
                            div(class="col-md-2",
                                textInput("select_couleur_trait", label = NULL, width='100px', value= couleur_trait_actif() )
                            )
                          )
                        ),


                        #Opacité des traits de la couche
                        div(
                          class="form-group",
                          div(
                            div(class="col-md-3",
                                tags$label("Opacité des traits "),
                            ),
                            div(class="col-md-9",
                                sliderInput("select_opacity_border", label = NULL, min = 0, max = 1, value=opacity_border_actif(), sep = 0.1)
                            )
                          )
                        ),


                        #largeur de trait
                        div(
                          class="form-group",
                          div(
                            div(class="col-md-3",
                                tags$label("Largeur de trait"),
                            ),
                            div(class="col-md-9",
                                numericInput("select_epaisseur_trait", label = NULL, min = 0, max=NA, width = "100px", value =  epaisseur_trait_actif() )
                            )
                          )
                        ),

                        #Style de trait
                        div(
                          class="form-group",
                          div(
                            div(class="col-md-3",
                                tags$label("Style de trait "),
                            ),
                            div(class="col-md-9",
                                selectInput("select_style_trait", label = NULL,
                                            choices = list("Ligne continue"="solid",
                                                           "Pas de ligne"="blank",
                                                           "Ligne en tiret"="longdash",
                                                           "Ligne en pointillet"="dotted",
                                                           "Ligne en tiret-point"="dotdash",
                                                           "Ligne en tiret-point-point"="twodash",
                                                           "Tirets"="dashed"), selected = style_trait_actif())
                            )
                          )
                        ),


                        #trait
                        hr(),

                        div(
                          class="form-group",
                          div(class="col-md-12",
                              checkboxInput("select_effet_symbologie", "Effects", value = FALSE)
                          )
                        ),

                        div(
                          class="col-md-12",
                          uiOutput("gestion_effets_symbologie_ui")
                        )


                      )
                    )
                  )


                }#fin traitement symbolo unique


              }



            })



            #### Gesttion de la validation par le bouton d'authemtification######
            output$bouton_appliquer_symbologie_couche_ui <-renderUI({
              actionButton("bouton_appliquer_symbologie_couche", "Appliquer", class="btn-success")
            })

            observeEvent(input$bouton_appliquer_symbologie_couche, {
              copie_couche <- liste_couches()
              #le nom de la couche en cours
              name_couche<- mame_couche_actif()


              #On fait les imputations nécessaires

              #on actualise les valeurs d'abord des types de symbologie choisie
              #print("esasi code imputation type symbole")
              #print( paste0( paste("copie_couche", name_couche ,"type_symbologie", sep="$"), "<-", input$selec_type_symbole ))
              eval(parse(text = paste0( paste("copie_couche", name_couche ,"type_symbologie", sep="$"), "<-'", input$selec_type_symbole, "'" ) ))

              if(!is.null(input$selec_type_symbole)){
                print("DEDANS")

                if(input$selec_type_symbole=="unique"){
                  ##on récupère d'abord les noms des clés
                  cles<- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique",  sep="$") ))
                  nom_cles <-setdiff(names(cles), c("legende"))#on n'a pas de champ pour la gestion des légendes au niveau de la symbologie

                  #On actualise la couche copiée
                  for (i in nom_cles) {#debut for
                    gauche <- paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique", paste0(i),  sep="$")
                    droite <- eval(parse(text = paste0("input$select_", paste0(i)   ) ))

                    if(!is.null(droite)){
                      if(!is.na( as.numeric(droite)) ){
                        print( paste( gauche, droite, sep = "<-")  )

                        eval(parse(text =   paste( gauche, droite, sep = "<-")    ))
                      }else{
                        print( paste( gauche, paste0('"',droite, '"'), sep = "<-")   )
                        eval(parse(text =   paste( gauche, paste0('"',droite, '"'), sep = "<-")    ))
                      }
                    }else{
                      print( paste0("input$select_", paste0(i)   ) )
                    }

                  }#Fin for


                }
              }

              liste_couches(copie_couche)

              #ferméture de la fenêtre modale
              removeModal()

              #cas des valeurs
              #if(type_symbologie_actif()=="unique"){#cas du symbole unique
              #couleur_fill <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","couleur_symbole",  sep="$") ))
              #couleur_remplissage_symbole_actif(couleur_fill)





            })




    #on met un observateur sur la liste des couches afin de déclencehr des actions relatives ######
    observeEvent(liste_couches(),{

      print(liste_couches())

      #on envoie la liste des couches à javascript
      session$sendCustomMessage("liste_couches", liste_couches() )

      #resultJs<- fromJSON(input$couleur_unique)


      if(length(liste_couches())>=1){ #on n'actualise que si le nbre des couches est >0
        #on actualise aussi la représentation des couches séclectionnés
        output$sortie_carte_ui <- renderImage({

          #on doit sélectionner spécialement les couches visibles
          couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de base de R

          #le graphique ici (on produit une version finalisée du graphique pour la présentation)
          graph<- finaliser_carte (couches_visibles, box_zone_carte() )

          outfile<- tempfile(fileext = "png")
          pixelratio <- session$clientData$pixelratio
          #dimensions automatique de l'image png
          width <- session$clientData$output_sortie_carte_ui_width*pixelratio#+75
          height <- width/graph$ratio

          png(outfile, width =width, height =height, res = 95 )
          print(graph$mon_graphique)
          dev.off()
          list(src=outfile)
        }, deleteFile=TRUE) #fin impression première carte du rendu

      }#fin contrôle sur la condition selon que le nombre de couches doit excéder 1

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
        type_symbologie="unique",
        visible=TRUE,
        geometrie= unique(as.character(st_geometry_type(couche_courant))), #on controle la geometrie pour gerer la crte plus tard (point, ligne, polygone, etc)
        options_symbologie_couche=list(
          options_symbologie_unique=list(#Gestion des symboles uniques de la couche
            couleur_symbole= liste_couleurs[nbre_couches_ajoutes+1],
            style_fill_symbologie="continu",
            legende=input$select_couche,
            couleur_trait="#DF1616",
            style_trait="solid",
            epaisseur_trait=1,
            opacity_fill=1,
            opacity_border=1
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

      showNotification(paste0("couche ", input$select_couche, " ajouté à la crte avec succès."), type = "message" )
      #fermeture du modal après traitement
      removeModal()

    })


    #on ferme la connexion du serveur à la fin de la session (websocket)
    #session$onSessionEnded(function(){
      #ws$close()

    #})

  }
)
