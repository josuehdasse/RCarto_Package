#Module de  egstion des couches
library(shiny)

mod_gestion_couches_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(class="controles",
             actionButton(ns("ajouter_couche"), "", icon = icon("add"), class="btn-primary btn-sm"),
             actionButton(ns("supp_couche"), "", icon = icon("trash"), class="btn-primary btn-sm")
    ),
    fluidRow(class="zone_travail",
             tags$table(class="table table-responsive-md",
                        tags$caption(style="caption-side:top;text-align:left",
                                     "Liste des couches"
                        ),
                        tags$thead(
                          tags$tr(
                            tags$th("#"),
                            tags$th("Symboles", scope="col"),
                            tags$th("Couches", scope="col"),
                            tags$th("Légende", scope="col"),
                            tags$th("Options", scope="col")
                          )
                        ),
                        tags$tbody(id=ns("liste_couches_carte"))

             )
    ),

    #on injecte les names spaces du modole dans Javascript ici
    tagList(
      tags$script(HTML(sprintf(
        "
        window.gestion_couchesNS='%s';//Stocke le namespace dans une variabel globale",
        ns("")#ns("") renvoie l'id du module courant
      )))
    )
  )

}


mod_gestion_couches_server<- function(input, output, session, liste_couches){
  ns<- session$ns


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





      #Ajout d'une nuvelle couche###################
      observeEvent(input$ajouter_couche,{
        #afficher le modal
        showModal(modalDialog(
          title = "Importer une couche",
          footer=tagList(
            div(class="div_footer_modal",
                uiOutput(ns("bouton_confirmer_ajout_layer_ui")),
                modalButton("Annuler")
            )

          ),
          fluidRow(
            withSpinner(
              uiOutput(ns("options_import_layer_ui")) )
          ),#on paramètre le contenu de la fenêtre cible ici
          #trigger = "ajouter_couche",
          size="m",
          easyClose = TRUE
        ))
      })


      ##Chargement du contenu de la fenetre modale d'importation des couches ########
        output$options_import_layer_ui <- renderUI({

          #supprimer ceux qui sont déja dans la liste
          liste_couches_final <- setdiff(
            ls(envir = globalenv() ) , names(liste_couches)
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
                        selectInput(ns("select_couche"), label = NULL, choices = liste_couches_final  )
                    )
                  )

                ),

                #Choix du CRS

                uiOutput(ns("select_projection_ui"))

            )
          )

        })



        ##Gestion du controle des couches et affichage des projections###########
        observeEvent(input$select_couche, {

          #on s'assure quúne couche au moins est sélectionnée
          if(!is.null(input$select_couche)){


            #print(paste0("select couche :", input$select_couche ))
            couche <- get(input$select_couche)

            print(paste0("couche sélectionnée !!! :", couche))

            #validation du type
            if(inherits(couche, "sf")){

              #test du crs
              if (is.na(st_crs(couche))) {#si la couche n'a pas de projetcion CRS
                output$select_projection_ui <- renderUI({
                  withTags(
                    div(
                      class="form-group",
                      div(
                        div(class="col-md-3",
                            tags$label("Projection de la couche "),
                        ),
                        div(class="col-md-9",
                            selectInput(ns("select_projection"), label = NULL, choices = liste_crs, selected = 4326  )
                        )
                      )

                    )
                  )
                })
              }else{#si la couche a une projection crs
                output$select_projection_ui <- renderUI({
                  withTags(
                    div(
                      class="form-group",
                      div(
                        div(class="col-md-3",
                            tags$label("Projection de la couche "),
                        ),
                        div(class="col-md-9",
                            selectInput(ns("select_projection"), label = NULL, choices = liste_crs, selected = st_crs(couche)$epsg   )
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




          }#FIn de la verification de la non-nullité de la couche



        })



        ##affichage du bouton de confirmation de la couche si la projection est choisie ########
        observeEvent(input$select_projection,{
          output$bouton_confirmer_ajout_layer_ui <- renderUI({
            actionButton(ns("confirmer_ajout_layer"), "Importer", class="btn-success")
          })
        })


        ##Confirmer l'ajout d'une couche et egstion de son importation#########
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






        #Actualisation des couleurs de remplissage de la symbologie via interface avec Javascript  ######
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


        #Gestion de l'activation et de désactivation des couches (interaction avec le JS et Shiny)#####
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

          eval( parse(text =  paste0(partie_gauche, "<-", paste(valeur_activation) ) ))

          #if(valeur_activation=="on")

          liste_couches(copie_couche)

        })


        #Gestion de la symbologie d'une couche#############
        donnees_symbologie_couche <- reactive({
          if(!is.null(input$select_option_symbologie_couche)){
            input$select_option_symbologie_couche
          }
        })


        ##Ecoute des changements qui peuvent provenir de JS##########
        observeEvent(donnees_symbologie_couche(), {
          data_symbologie <- fromJSON(input$select_option_symbologie_couche)
          name_couche <- data_symbologie$name
          mame_couche_actif(name_couche)

          #duplication de la couche
          copie_couche <- liste_couches()

          #On actualise les valeurs de la symbologie suivant la  couche sélectionnée
          type_symbologie <- eval(parse(text = paste("copie_couche", name_couche ,"type_symbologie", sep="$") ))
          print(paste("copie_couche", name_couche ,"type_symbologie", sep="$"))
          type_symbologie_actif(type_symbologie)
          print(type_symbologie_actif)

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
                  uiOutput(ns("bouton_appliquer_symbologie_couche_ui")),
                  modalButton("Annuler")
              )
            ),
            fluidRow(
              withSpinner(
                uiOutput(ns("options_symbologie_layer_ui")) )
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
                uiOutput(ns("type_symbole")),#le choix de la symbologie (pour actualiser dans la liste plus tard)
                uiOutput(ns("options_symbologie_ui")),#gestionnaire du contrôle  de l'apparence des symboles
                uiOutput(ns("palette_couleurs_ui")),#la palette des couleurs à utiliser pour le dégradé des catégories
                uiOutput(ns("controle_classes_categories_ui")),
                uiOutput(ns("classes_indicateurs")),
                uiOutput(ns("labels_classes_indicateurs"))
            )
          )
        })

        ####le choix du type symbologie de l'utilisateur (avec comme valeur de base la valeur existante) #####
        output$type_symbole <- renderUI({

          withTags(
            div(class="col-md-12",
                class="form-group",
                div(
                  div(class="col-md-3",
                      tags$label("Type de symbologie"),
                  ),

                  div(class="col-md-9",
                      selectInput(ns("selec_type_symbole"), NULL, choices = list("symbole unique"="unique", "catégorisé"="categorie", "gradué"="graduate"), selected = type_symbologie_actif() )
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

                        div(class="col-md-9",
                            colourInput(ns("select_couleur_symbole"), label = NULL, value= couleur_remplissage_symbole_actif())
                            # tags$input(type="color", onchange="fonction_color_symboble_unique(this.value)", id="select_couleur_symbole_color", style="width:100%; height:30px;", value=couleur_remplissage_symbole_actif() )
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
                            sliderInput(ns("select_opacity_fill"), label = NULL, min = 0, max = 1, sep = 0.1, value=opacity_fill_actif() )
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
                            selectInput(ns("select_style_fill_symbologie"), label = NULL, choices = list("Continue"="continu",
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

                        div(class="col-md-9",
                            colourInput(ns("select_couleur_trait"), label = NULL, value=couleur_trait_actif())
                            # tags$input(type="color", onchange="fonction_color_symboble_unique(this.value)", id="select_couleur_symbole_color", style="width:100%; height:30px;", value=couleur_remplissage_symbole_actif() )
                        )

                      )
                    ),


                    #Opacité des traits de la couche
                    fluidRow(
                      class="form-group",
                      div(
                        div(class="col-md-3",
                            tags$label("Opacité des traits "),
                        ),
                        div(class="col-md-9",
                            sliderInput(ns("select_opacity_border"), label = NULL, min = 0, max = 1, value=opacity_border_actif(), sep = 0.1)
                        )
                      )
                    ),


                    #largeur de trait
                    fluidRow(
                      class="form-group",
                      div(
                        div(class="col-md-3",
                            tags$label("Largeur de trait"),
                        ),
                        div(class="col-md-9",
                            numericInput(ns("select_epaisseur_trait"), label = NULL, min = 0, max=NA, width = "100px", value =  epaisseur_trait_actif() )
                        )
                      )
                    ),

                    #Style de trait
                    fluidRow(
                      class="form-group",

                      column(width = 3,
                             tags$label("Style de trait "),
                      ),
                      column(width = 9,
                             selectInput(ns("select_style_trait"), label = NULL,
                                         choices = list("Ligne continue"="solid",
                                                        "Pas de ligne"="blank",
                                                        "Ligne en tiret"="longdash",
                                                        "Ligne en pointillet"="dotted",
                                                        "Ligne en tiret-point"="dotdash",
                                                        "Ligne en tiret-point-point"="twodash",
                                                        "Tirets"="dashed"), selected = style_trait_actif())
                      )

                    ),


                    #trait
                    #hr(),

                    fluidRow(
                      checkboxInput(ns("select_effet_symbologie"), "Effects", value = FALSE)
                    ),

                    fluidRow(
                      uiOutput(ns("gestion_effets_symbologie_ui"))
                    )


                  )
                )
              )


            }#fin traitement symbolo unique


          }



        })




        ##Gestion des effets de la symbologie d'une couche##############
        options_complets =list(
          drop_shadow_portee=list(#options de la gestion de l'ombre de portée
            decalage_x=135,#angle de décalage en X (horizontal)
            delalage_y=2, #distance du décallage en y
            sigma=2.6450,#Rayon de floutage ou intensité de flou
            alpha=1, #Opacité
            couleur="#000000",#la couleur de l'ombre
            mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply)
          ),
          drop_shadow_interieure=list(
            decalage_x=135,#angle de décalage en X (horizontal)
            delalage_y=2, #distance du décallage en y
            sigma=2.6450,#Rayon de floutage ou intensité de flou
            alpha=1, #Opacité
            couleur="#000000",#la couleur de l'ombre
            mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))
          ),
          innner_glow=list(#luminaissance interne
            rayon=2,#Rayon
            sigma=2.6450,#Rayon de floutage ou intensité de flou
            alpha=0.5, #Opacité
            couleur="#000000",#la couleur de l'ombre,
            palette="",#la palette de couleurs
            mode_fusion="normal"#le mode de fusion de l'ombre (défaut sur normal)
          ),
          outer_glow=list(
            rayon=2,#Rayon
            sigma=2.6450,#Rayon de floutage ou intensité de flou
            alpha=0.5, #Opacité
            couleur="#000000",#la couleur de l'ombre,
            palette="",#la palette de couleurs
            mode_fusion="normal"#le mode de fusion de l'ombre (défaut sur normal)
          ),
          source=list(
            alpha=1, #Opacité
            couleur="#000000",#la couleur de l'ombre
            mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))
          ),
          transformer=list(
            miroir_horizontal=FALSE,
            miroir_vertical=FALSE,
            cisaille_x=0,
            cisaille_y=0,
            echelle_x=1,
            echelle_y=1,
            rotation=0,
            translation_x=0,
            translation_y=0
          ),
          flou=list(
            type_flou="empilé",
            sigma=2.6450,#Rayon de floutage ou intensité de flou
            alpha=1, #Opacité
            mode_fusion="normal"#le mode de fusion de l'ombre (défaut sur normal)
          ),
          coloriser=list(
            luminosite=0,
            contraste=0,
            saturation=0,
            coloriser=1,
            couleur_coloriser="#ff8080",
            niveau_gris="pas_clarte",
            alpha=0.5, #Opacité
            mode_fusion="normal"
          )
        )

        ###option des effets reactivf (traqué)#####
        options_effets_symbologie_actif_simult <- reactiveVal( #le plus comlet pour la simulation
          list(
            source=list(
              alpha=1, #Opacité
              couleur="#000000",#la couleur de l'ombre
              mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))
            )
          )
        )

        observeEvent(input$select_effet_symbologie,{
          #gestion de l'affichaage des effets des symbologies
          output$gestion_effets_symbologie_ui <- renderUI({
            if(input$select_effet_symbologie){#si l'on coché la case "effets"
              withTags(
                fluidRow(
                  column(width =6 ,
                         selectInput(ns("liste_effects_symbologie"), "Choisir un effet", choices = list(
                           "Ombre de portée"="drop_shadow_portee",
                           "Ombre intérieure"="drop_shadow_interieure",
                           "Luminescence interne"="innner_glow",
                           "Luminescence externe"="outer_glow",
                           "Source"="source",
                           "Transformer"= "transformer",
                           "Flou"="flou",
                           "Coloriser"="coloriser"
                         ), selected = "source"),#l'effect source est sélectionné automatiwurment si le case effets est sélectionné
                         uiOutput("options_controle_effects_couche_ui")
                  ),
                  column(width = 6,
                  )
                ),
                fluidRow(
                  column(width = 12,
                         uiOutput(ns("details_effet_symbologie_ui"))
                  )
                )

              )
            }
          })



        })



        ## Affichage du bouton d'aplication des options de la symbollogie et valisation ######
        output$bouton_appliquer_symbologie_couche_ui <-renderUI({
          actionButton(ns("bouton_appliquer_symbologie_couche"), "Appliquer", class="btn-success")
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

        #observeEvent(input$infos_color_symboble_unique, {
          #couleur_remplissage_symbole_actif(input$infos_color_symboble_unique$color_symboble_unique_js)
          #updateTextInput(session, "select_couleur_symbole", value = input$infos_color_symboble_unique$color_symboble_unique_js )
        #})



}#fin serveur
