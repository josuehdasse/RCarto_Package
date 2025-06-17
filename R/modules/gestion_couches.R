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

             tagList(

               tags$div(class="list-header",
                        tags$p("liste des couches")
                        ),
               tags$div(class="container-hierarchy",
                 tags$ul(id=ns("liste_couches_carte"),
                         p("Pas de couche à afficher", style="color:red;")
                 )
               )

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


            #le statut d'ajout de
            statut_effet_actif <- reactiveVal(FALSE)



            #options de la symbologie ds couches
            options_symbologies_couche_actif <- reactiveVal(NULL)


            options_effets_symbologie_actif <- reactiveVal( #le plus comlet pour la simulation
              list(
                source=list(
                  checked=TRUE,
                  label="Source",
                  name="source",
                  options=list(
                    alpha=1, #Opacité
                    couleur="#000000",#la couleur de l'ombre
                    mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))
                  )

                )
              )
            )






      #Ajout d'une nuvelle couche###################
            ###afficher le modal#####
      observeEvent(input$ajouter_couche,{

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


      ###Chargement du contenu de la fenetre modale d'importation des couches ########
        output$options_import_layer_ui <- renderUI({

          #supprimer ceux qui sont déja dans la liste
          liste_couches_final <- setdiff(
            ls(envir = globalenv() ) , names(liste_couches)
          )

          #print(liste_couches_final)

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



        autoriser_importation_couche <- reactiveVal(FALSE)

        ###Gestion du controle des couches et affichage des projections###########
        observeEvent(input$select_couche, {

          req(input$select_couche)

          #on s'assure quúne couche au moins est sélectionnée
          if(!is.null(input$select_couche)){

            #print(paste0("select couche :", input$select_couche ))
            couche <- get(input$select_couche)

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

                #autoriser_importation_couche(TRUE)

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

                autoriser_importation_couche(TRUE)

              }

            }else{

              autoriser_importation_couche(FALSE)

              #on désactive le bouton d'importation
              output$bouton_confirmer_ajout_layer_ui <- renderUI({

              })

              output$select_projection_ui <- renderUI({
                p("L'objet sélectionné n'est pas de type sf", style="color:red;")
              })
            }

          }#FIn de la verification de la non-nullité de la couche



        })



        ###affichage du bouton de confirmation de la couche si la projection est choisie ########
        observeEvent(input$select_projection,{
          autoriser_importation_couche(TRUE)
        })

        observeEvent(autoriser_importation_couche(),{
          if(autoriser_importation_couche()){
            output$bouton_confirmer_ajout_layer_ui <- renderUI({
              actionButton(ns("confirmer_ajout_layer"), "Importer", class="btn-success")
            })
          }
        })


        ##Confirmer l'ajout d'une couche et egstion de son importation#########
        observeEvent(input$confirmer_ajout_layer,{
          #déroulement de l'importation de la couche
          ##La couche est directement importée dans le format avec une symbologie unique qui prend les couleurs du système par défaut
          couche_import <- get(input$select_couche)

          #nmbre_elements dans la liste des couches
          nbre_couches_ajoutes <- length(liste_couches())

          #application du crs
          if(is.na(st_crs(couche_import))){
            couche_import <- couche_import %>% st_set_crs(as.numeric(input$select_projection) )
          }else{
            couche_import <- couche_import %>%   st_transform( st_crs(as.numeric(input$select_projection) ) )
          }

          #création de la liste
          couche_courant =list(
            couche= couche_import,
            crs=as.numeric(input$select_projection),
            type_symbologie="unique",
            visible=TRUE,
            geometrie= unique(as.character(st_geometry_type(couche_import))), #on controle la geometrie pour gerer la crte plus tard (point, ligne, polygone, etc)
            options_symbologie_couche=list(
              options_symbologie_unique=list(
                symbologie_1=list(
                  name="symbologie_1",
                  label="Remplissage simple 1",
                  #Gestion des symboles uniques de la couche
                  couleur_symbole= liste_couleurs[nbre_couches_ajoutes+1],
                  style_fill_symbologie="continu",
                  legende=input$select_couche,
                  couleur_trait="#DF1616",
                  style_trait="solid",
                  epaisseur_trait=1,
                  opacity_fill=1,
                  opacity_border=1,
                  statut_effet=FALSE,
                  effects=options_effets_symbologie_actif()
                )#fin de la première symbologie


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

          showNotification(paste0("couche ", input$select_couche, " ajouté à la carte avec succès."), type = "message" )
          #fermeture du modal après traitement
          autoriser_importation_couche(FALSE)
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

          #print(paste0(partie_gauche, "<-", paste(valeur_activation) ))

          eval( parse(text =  paste0(partie_gauche, "<-", paste(valeur_activation) ) ))

          #if(valeur_activation=="on")

          liste_couches(copie_couche)

        })

        #Gestion de la symbologie des couches ######################
        ###Gestion de la symbologie d'une couche : données recees de JS #############
        donnees_symbologie_couche <- reactive({
          if(!is.null(input$select_option_symbologie_couche)){
            input$select_option_symbologie_couche
          }
        })


        ###Ecoute des changements qui peuvent provenir de JS sur la couche choisie ##########
        couche_symbologie_actif <- reactiveVal(NULL)

        observeEvent(donnees_symbologie_couche(), {
          data_symbologie <- fromJSON(input$select_option_symbologie_couche)
          name_couche <- data_symbologie$name
          mame_couche_actif(name_couche)


          #on reinitialise les informations sur le choix de la couche de symbologie
          couche_symbologie_actif(NULL)

          #duplication de la couche
          copie_couche <- liste_couches()

          #### resultat requete: On actualise les valeurs de la symbologie suivant la  couche sélectionnée #############################
          type_symbologie <- eval(parse(text = paste("copie_couche", name_couche ,"type_symbologie", sep="$") ))
          type_symbologie_actif(type_symbologie)


          #Elaboration de la liste des couches de symbologie
          switch (type_symbologie,
                  "unique" = {
                    options_symbologies_couche <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique",  sep="$") ))
                    options_symbologies_couche_actif(options_symbologies_couche)
                  }
          )









          ####on donne accès à la fenêtre modale permettant de gérer les options de la symbologie de la couche courante#####################
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

        ##### contenu des options de gestion de la symbologie##############################
        output$options_symbologie_layer_ui <- renderUI({
          withTags(
            fluidRow(
                fluidRow(
                  uiOutput(ns("type_symbole"))#le choix de la symbologie (pour actualiser dans la liste plus tard)
                ),
                fluidRow(
                  uiOutput(ns("couches_symbologie"))#liste des couches de la symbologie (en reactif)
                ),
                tags$hr(width="80%"),
                fluidRow(
                  #UIParametresSymbologie()
                  uiOutput(ns("options_symbologie_ui"))#gestionnaire du contrôle  de l'apparence des symboles
                ),
                fluidRow(
                  uiOutput(ns("palette_couleurs_ui"))#la palette des couleurs à utiliser pour le dégradé des catégories
                ),
                fluidRow(
                  uiOutput(ns("controle_classes_categories_ui"))
                ),
                fluidRow(
                  uiOutput(ns("classes_indicateurs"))
                ),
                fluidRow(
                  uiOutput(ns("labels_classes_indicateurs"))
                )
            )
          )
        })

        ####le choix du type symbologie de l'utilisateur (avec comme valeur de base la valeur existante) #####
        output$type_symbole <- renderUI({

          withTags(
            fluidRow(class="ligne_contenu_modal",
                  column(width = 3,
                      tags$label("Type de symbologie"),
                  ),

                  column(width = 9,
                      selectInput(ns("selec_type_symbole"), NULL, choices = list("symbole unique"="unique", "catégorisé"="categorie", "gradué"="graduate"), selected = type_symbologie_actif() )
                  )

            )
          )

        })





        #### On affiche d'abord les couches de la symbologie ##########


        ###### La liste des reactives des coucehs de symbologie ############
        couches_symbologieUI <- reactive({
          symbologieUI <- lapply(options_symbologies_couche_actif() , function(i){ #les effets qui sont ajoutés à la couche courante
              print(paste0("border: ",i$epaisseur_trait, "px ",  paste0(i$style_trait), " ", i$couleur_trait, ";"))

              tags$li(
                class="list_item",
                tagList(
                  tags$div(class="symbol_container",
                    tagList(
                      tags$input(
                        type="color",
                        width=20,
                        height=20,
                        disabled=TRUE,
                        value=i$couleur_symbole,
                        style=paste0("border: ",i$epaisseur_trait/0.75+1, "px ",  paste0(i$style_trait), " ", i$couleur_trait, ";")
                      )

                    )
                  ),#fin label
                  tags$span(
                    i$label,
                    id=paste0(i$name),
                    onclick="afficher_options_couche_symbologie(this.id)"
                    )
                )
                #i$label
              )#fin li

          })

        })



        #on recupère la valeur choisie par l'utlisateur pour lui donner la main afin de faire les paramétrages
        observeEvent(input$choix_couche_symbologie, {

          couche_symbologie_actif(NULL)

          req(input$choix_couche_symbologie)

          #on actualise la valeur de la synbologie en cours tel que choisi par l'utlisateu
            data_couche_select<- fromJSON(input$choix_couche_symbologie)
            name_couche_symbologie <-data_couche_select$name
            couche_symbologie_actif(name_couche_symbologie)

            print(couche_symbologie_actif())


            #peut déjà ici propager les réactives à utiliser dans l'application
            #copie d ela couche des valeurs
            copie_couche <- liste_couches()
            copie_couleur_fill <-

            #on fait une copie de la couche des otpions de la symbologie
            #options_symbologies_couche =options_symbologies_couche_actif()

            #on echerche les valeurs de la couche de symbologie et on les affiche
            #if(type_symbologie_actif()=="unique"){#cas du symbole unique
            couleur_fill <- eval(parse(text = paste("copie_couche", mame_couche_actif() ,"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,   "couleur_symbole",  sep="$") ))
            couleur_remplissage_symbole_actif(couleur_fill)


            opacity_fill <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"opacity_fill",  sep="$") ))
            opacity_fill_actif(opacity_fill)


            opacity_border <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"opacity_border",  sep="$") ))
            opacity_border_actif(opacity_border)

            style_trait <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"style_trait",  sep="$") ))
            style_trait_actif(style_trait)

            style_fill_symbologie <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"style_fill_symbologie",  sep="$") ))
            style_fill_symbologie_actif(style_fill_symbologie)

            couleur_trait <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"couleur_trait",  sep="$") ))
            couleur_trait_actif(couleur_trait)

            epaisseur_trait <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"epaisseur_trait",  sep="$") ))
            epaisseur_trait_actif(epaisseur_trait)

            #les informations sur les effets
            statut_effet <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"statut_effet",  sep="$") ))
            statut_effet_actif(statut_effet)


            #les options sur les effets appliqués à cette couche
            #options_effets_symbologie<- eval(parse(text = paste("copie_couche",mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",paste0(couche_symbologie_actif()),"effects",  sep="$") ))


        })


        ##### La prévisualisation des choix de la symbologie #########
        previsulationSymbologieUI <- reactive({

          #on copie les options de symbologie de la couche
          options_symbologie = options_symbologies_couche_actif()

          #initialisation du graphique
          graphique <- ggplot()

          switch (type_symbologie_actif(),
            "unique" = {

              names_symbologies <- names(options_symbologie)

              for (i in 1:length(options_symbologie)) {
                data_graph <- rbind( c(0.0), c(1,0), c(1,1), c(0,1), c(0,0) )#les données du graphique
                polygone <- st_polygon(list(data_graph))
                sfc_obj <- st_sfc(polygone)
                data_points <- data.frame(names=c("A", "B", "C", "D"))
                data_points$geometry <- sfc_obj
                data_points <- st_as_sf(data_points)

                print(data_points)

                fill_couche <- eval(parse(text =  paste("options_symbologie", names_symbologies[i], "couleur_symbole", sep = "$")  ))
                opacity_fill <- eval(parse(text =  paste("options_symbologie", names_symbologies[i], "opacity_fill", sep = "$")  ))
                couleur_trait <- eval(parse(text =  paste("options_symbologie", names_symbologies[i], "couleur_trait", sep = "$")  ))
                opacity_border <- eval(parse(text =  paste("options_symbologie", names_symbologies[i], "opacity_border", sep = "$")  ))
                style_trait <- eval(parse(text =  paste("options_symbologie", names_symbologies[i], "style_trait", sep = "$")  ))


                couche_graph <- geom_sf(data = data_points,
                                        fill= alpha(paste0(fill_couche), opacity_fill),
                                        colour= alpha(paste0(couleur_trait), opacity_border),
                                        linetype=paste0(style_trait)
                                      )

                graphique <- graphique + couche_graph

              }

            }



          )#fin des swithchs


          graphique <- graphique + eval(parse(text = theme_graphique))


          graphique
        })


        output$couches_symbologie <- renderUI({
          req(input$selec_type_symbole)#Eviter les valeurs nuls

              tagList(
                fluidRow(class="ligne_contenu_modal",
                  column(width = 2, style="height:170px;",  imageOutput(ns("previsualisation_symbologie_ui"))),#place pour la visualisation du rendu final de la symbologie sur toutes les couches
                  column(width = 8, class="list-items-couches",
                         tagList(
                           tags$p("Remplissage"),
                           tags$ul(
                             couches_symbologieUI()#liste des couches de sumbologie pour le type sélectionné
                           )
                         )

                         ),#liste des couches de symbologies
                  column(width = 2, style="height:170px;",
                         tagList(
                           actionButton(ns("add_symbologie"), "", icon = icon("add"), class="btn-primary btn-sm")
                         )

                         )#options de controle de la liste des symbologies
                )
              )#fin taglist



        })

        ### On renvoie l'image de la prévisualisation
        output$previsualisation_symbologie_ui <- renderImage({
          graphique <- previsulationSymbologieUI()

          outfile<- tempfile(fileext = "png")
          png(outfile, width =100, height =100, res = 20 )
          print(graphique)
          dev.off()
          list(src=outfile)

        }, deleteFile=TRUE)



        ####on remplit les caractéristiques de la gestion des options de controle des symboles####
        symbologie_selectionne_actif<- reactiveVal(NULL)


        #Suivi des modification sur les paramètres des erreus
        observe({


          req(input$select_couleur_symbole)
          updateColourInput(session, "select_couleur_symbole", value=input$select_couleur_symbole)
          couleur_remplissage_symbole_actif(input$select_couleur_symbole)


          req(input$select_opacity_fill)
          opacity_fill_actif(input$select_opacity_fill)
          print(input$select_opacity_fill)
          updateSliderInput(session, "select_opacity_fill", label = NULL, min = 0, max = 1, value=input$select_opacity_fill   )



        })


        output$options_symbologie_ui <- renderUI({
          req(input$choix_couche_symbologie)#Eviter les valeurs nulles du clic de l'utilisateur
          req(input$selec_type_symbole)
          req(couche_symbologie_actif())


          if( input$selec_type_symbole=="unique"){##

            isolate({
              tagList(
                # tagList(
                #sélection des couleurs de la symbologie
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,tags$label("Couleur de remplissage ") ),

                         column(width = 8,colourInput(ns("select_couleur_symbole"), label = NULL, value= couleur_remplissage_symbole_actif(), allowTransparent = TRUE, palette = "square", closeOnClick = TRUE  ) )
                ),

                #Opacité de la couleur de remplissage de la symbologie de la couche
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,tags$label("Opacité de remplissage ") ),
                         column(width = 8,sliderInput(ns("select_opacity_fill"), label = NULL, min = 0, max = 1, sep = 0.1, value=opacity_fill_actif() ) )
                ),

                #Style de remplissage
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Style de remplissage ")
                         ),
                         column(width = 8,
                                selectInput(ns("select_style_fill_symbologie"), label = NULL, choices = list("Continue"="continu",
                                                                                                             "Pas de remplissage"="blank",
                                                                                                             "Motif"="motif"),  selected = style_fill_symbologie_actif() )
                         )
                ),


                #Couleur de trait
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Couleur de trait")
                         ),
                         column(width = 8,
                                colourInput(ns("select_couleur_trait"), label = NULL, value=couleur_trait_actif() )
                         )
                ),

                #Opacité des traits de la couche
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Opacité des traits "),
                         ),
                         column(width = 8,
                                sliderInput(ns("select_opacity_border"), label = NULL, min = 0, max = 1, value=opacity_border_actif(), sep = 0.1)
                         )
                ),


                #largeur de trait

                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Largeur de trait")
                         ),
                         column(width = 8,
                                numericInput(ns("select_epaisseur_trait"), label = NULL, min = 0, max=NA, width = "100px", value =  epaisseur_trait_actif() )
                         )
                ),


                #Style de trait
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Style de trait ")
                         ),
                         column(width = 8,
                                selectInput(ns("select_style_trait"), label = NULL,
                                            choices = list("Ligne continue"="solid",
                                                           "Pas de ligne"="blank",
                                                           "Ligne en tiret"="longdash",
                                                           "Ligne en pointillet"="dotted",
                                                           "Ligne en tiret-point"="dotdash",
                                                           "Ligne en tiret-point-point"="twodash",
                                                           "Tirets"="dashed"), selected = style_trait_actif(), width = "80%" )
                         )


                )


                #)#fin taglist
                ,

                hr(width="85%"), #trait séparateur

                fluidRow(
                  fluidRow(class="ligne_contenu_modal",
                           div(class="form-group",
                               checkboxInput(ns("select_effet_symbologie"), "Effects", value = statut_effet_actif() )
                           )
                  ),

                  fluidRow(class="ligne_contenu_modal",

                           uiOutput(ns("gestion_effets_symbologie_ui"))
                  )
                )


                #fin fluid row
              )#Fin withTag


            })



          }




        })


        UIParametresSymbologie <- reactive({

              if(!is.null(couche_symbologie_actif())){


                if(!is.null(input$selec_type_symbole)){
                  if( input$selec_type_symbole=="unique"){##
                    UIReactif <- tagList(
                      # tagList(
                      #sélection des couleurs de la symbologie
                      fluidRow(class="form-group ligne_contenu_modal",
                               column(width = 4,tags$label("Couleur de remplissage ") ),
                               column(width = 8,colourInput(ns("select_couleur_symbole"), label = NULL, value= couleur_fill  ) )
                      ),

                      #Opacité de la couleur de remplissage de la symbologie de la couche
                      fluidRow(class="form-group ligne_contenu_modal",
                               column(width = 4,tags$label("Opacité de remplissage ") ),
                               column(width = 8,sliderInput(ns("select_opacity_fill"), label = NULL, min = 0, max = 1, sep = 0.1, value=opacity_fill ) )
                      ),

                      #Style de remplissage
                      fluidRow(class="form-group ligne_contenu_modal",
                               column(width = 4,
                                      tags$label("Style de remplissage ")
                               ),
                               column(width = 8,
                                      selectInput(ns("select_style_fill_symbologie"), label = NULL, choices = list("Continue"="continu",
                                                                                                                   "Pas de remplissage"="blank",
                                                                                                                   "Motif"="motif"),  selected = style_fill_symbologie )
                               )
                      ),


                      #Couleur de trait
                      fluidRow(class="form-group ligne_contenu_modal",
                               column(width = 4,
                                      tags$label("Couleur de trait")
                               ),
                               column(width = 8,
                                      colourInput(ns("select_couleur_trait"), label = NULL, value=couleur_trait )
                               )
                      ),

                      #Opacité des traits de la couche
                      fluidRow(class="form-group ligne_contenu_modal",
                               column(width = 4,
                                      tags$label("Opacité des traits "),
                               ),
                               column(width = 8,
                                      sliderInput(ns("select_opacity_border"), label = NULL, min = 0, max = 1, value=opacity_border, sep = 0.1)
                               )
                      ),


                      #largeur de trait

                      fluidRow(class="form-group ligne_contenu_modal",
                               column(width = 4,
                                      tags$label("Largeur de trait")
                               ),
                               column(width = 8,
                                      numericInput(ns("select_epaisseur_trait"), label = NULL, min = 0, max=NA, width = "100px", value =  epaisseur_trait )
                               )
                      ),


                      #Style de trait
                      fluidRow(class="form-group ligne_contenu_modal",
                               column(width = 4,
                                      tags$label("Style de trait ")
                               ),
                               column(width = 8,
                                      selectInput(ns("select_style_trait"), label = NULL,
                                                  choices = list("Ligne continue"="solid",
                                                                 "Pas de ligne"="blank",
                                                                 "Ligne en tiret"="longdash",
                                                                 "Ligne en pointillet"="dotted",
                                                                 "Ligne en tiret-point"="dotdash",
                                                                 "Ligne en tiret-point-point"="twodash",
                                                                 "Tirets"="dashed"), selected = style_trait, width = "80%" )
                               )


                      )


                      #)#fin taglist
                      ,

                      hr(width="85%"), #trait séparateur

                      fluidRow(
                        fluidRow(class="ligne_contenu_modal",
                                 div(class="form-group",
                                     checkboxInput(ns("select_effet_symbologie"), "Effects", value = statut_effet )
                                 )
                        ),

                        fluidRow(class="ligne_contenu_modal",

                                 uiOutput(ns("gestion_effets_symbologie_ui"))
                        )
                      )


                      #fin fluid row
                    )#Fin withTag


                  }#fin traitement symbolo unique


                }










              }else{
                UIReactif <- p("Pas de détails de couche de symologie à afficher")
              }


              UIReactif


        })






        ##Gestion des effets #####
        observeEvent(input$select_effet_symbologie,{
          #gestion de l'affichaage des effets des symbologies
          if(input$select_effet_symbologie){
            nouveau_statut_effet<-TRUE
            statut_effet_actif(nouveau_statut_effet)
          }


        })


        observeEvent(statut_effet_actif(),{

          if(statut_effet_actif()){

            output$gestion_effets_symbologie_ui <- renderUI({
              if(input$select_effet_symbologie){#si l'on coché la case "effets"
                tagList(
                  fluidRow(class="ligne_contenu_modal",
                           column(width =5 ,
                                  fluidRow(

                                    selectInput(ns("liste_effects_symbologie"), NULL,
                                                choices = list(
                                                  "Ombre de portée"="drop_shadow_portee",
                                                  "Ombre intérieure"="drop_shadow_interieure",
                                                  "Luminescence interne"="innner_glow",
                                                  "Luminescence externe"="outer_glow",
                                                  #"Source"="source",
                                                  "Transformer"= "transformer",
                                                  "Flou"="flou",
                                                  "Coloriser"="coloriser"
                                                ), selected = "source", width = "60%")

                                  ),
                                  fluidRow(#options de controle des effets (boutons ajouter et supprimer)
                                    actionButton(ns("ajouter_effet"), "", icon = icon("add"), class="btn-primary btn-sm"),
                                    actionButton(ns("supp_effet"), "", icon = icon("trash"), class="btn-primary btn-sm")
                                  )
                           ),
                           column(width = 7,#option des listes
                                  fluidRow(
                                    p( sprintf("Liste des couches d'effets de %s",  mame_couche_actif()  )),
                                    tags$ul(id=ns("liste_effets_associes_symbologie"),
                                            UIEffets()
                                            )
                                  )
                           )
                  ),#fin fluidrow
                  fluidRow(
                    uiOutput(ns("parametres_effets_ui"))
                  )
                )#fluidpage

              }



              #tagQuery(paste0("#", ns("liste_effets_associes_symbologie")))$empty()$append(uiEffets)$run

            })#Fin actualisation options des effets de la symbologie


          }
        } )



        ### Ce qui se passe lorsque l'on choisit d'ajouter une couche d'effets ##########
        observeEvent(input$ajouter_effet,{
          if(!is.null(input$ajouter_effet) & (input$ajouter_effet !="source" ) ){
            effet_choisi <- input$liste_effects_symbologie

            #on charge les options par defaut de l'effet
            options_effet <- sprintf("%s$%s", "options_defaut_effets", input$liste_effects_symbologie)
              options_effet<- eval(parse(text = options_effet ))

              nouvelle_liste_effets<- append(options_effets_symbologie_actif(), list(options_effet))
              names(nouvelle_liste_effets)[length(nouvelle_liste_effets)]<- input$liste_effects_symbologie

              #On recharge la liste des effets de la couche
              options_effets_symbologie_actif(nouvelle_liste_effets)

          }

        })



        UIEffets <- reactive({
          uiEffets <- lapply(options_effets_symbologie_actif() , function(i){ #les effets qui sont ajoutés à la couche courante

            if(i$checked){
              tags$li(
                class="list_item",
                tagList(
                  tags$label(
                    tagList(
                      tags$input(
                        type="checkbox",
                        checked="checked",
                        id=paste0("checked_", i$name),
                        width=20,
                        height=20,
                        onclick="alert(this.id)"
                      ),#fin input
                      tags$span(class="slider")

                    )
                  ),#fin label
                  tags$span(
                    id=paste0("name_", i$name),
                    onclick="afficher_options_effet(this.id)",
                    i$label)
                )
                #i$label
              )#fin li
            }else{
              tags$li(
                class="ligne_effets",
                tagList(
                  tags$label(
                    tagList(
                      tags$input(
                        type="checkbox",
                        id=paste0("checked_", i$name),
                        width=20,
                        height=20,
                        onclick="alert(this.id)"
                      ),#fin input
                      tags$span(class="slider")

                    )
                  ),#fin label
                  tags$span(
                    id=paste0("name_", i$name),
                    onclick="afficher_options_effet(this.id)",
                    i$label)
                )
                #i$label
              )#fin li
            }




          })

        })


        #suivi des réactifs de l'effet sé
        effet_actif <- reactiveVal(NULL)
        angle_effet_actif <- reactiveVal(NULL)
        distance_effet_actif<- reactiveVal(NULL)
        sigma_effet_actif<- reactiveVal(NULL)
        alpha_effet_actif <- reactiveVal(NULL)
        mode_fusion_effet_actif <- reactiveVal(NULL)
        rayon_effet_actif <- reactiveVal(NULL)
        couleur_effet_actif <- reactiveVal(NULL)


        #les paramètres liés à l'effet sélectionné

        ParametresEffets <- reactive({


          if( !is.null(effet_actif() ) ){

            #copie des effets en cours
            effets_couches <- options_effets_symbologie_actif()


            #on reprend les valeurs actives de ces paramètres
            angle_effet <-eval(parse(text =  paste("effets_couches", effet_actif(), "options", "angle", sep = "$")     ))
            angle_effet_actif(angle_effet)

            distance_effet <-eval(parse(text =  paste("effets_couches", effet_actif(), "options", "distance", sep = "$")     ))
            distance_effet_actif(distance_effet)

            sigma_effet <-eval(parse(text =  paste("effets_couches", effet_actif(), "options", "sigma", sep = "$")     ))
            sigma_effet_actif(sigma_effet)

            alpha_effet <-eval(parse(text =  paste("effets_couches", effet_actif(), "options", "alpha", sep = "$")     ))
            alpha_effet_actif(alpha_effet)

            mode_fusion_effet <-eval(parse(text =  paste("effets_couches", effet_actif(), "options", "mode_fusion", sep = "$")     ))
            mode_fusion_effet_actif(mode_fusion_effet)

            couleur_effet <-eval(parse(text =  paste("effets_couches", effet_actif(), "options", "couleur", sep = "$")     ))
            couleur_effet_actif(couleur_effet)

            rayon_effet <-eval(parse(text =  paste("effets_couches", effet_actif(), "options", "rayon", sep = "$")     ))
            rayon_effet_actif(rayon_effet)





            #sortie=p("Sortie des paramètres")

            #Dépend u type d'effet choisi
            switch (effet_actif(),
                    "drop_shadow_portee" = {
                      tagList(
                        fluidRow(class="ligne_contenu_modal",
                                 #Angle
                                 fluidRow(class="form-group",
                                     column(width = 3, tags$label("Angle") ),
                                     column(width = 9, numericInput(ns("param_effet_angle"), label = NULL, min = 0, max=360, width = "100px", value = angle_effet_actif() )
                                     )
                                 ),

                                 #distance
                                 fluidRow(class="form-group",
                                     column(width = 3, tags$label("Distance") ),
                                     column(width = 9, numericInput(ns("param_effet_distance"), label = NULL, min = 0, max=NA, width = "100px", value = distance_effet_actif() )
                                     )
                                 ),

                                 #sigma
                                 fluidRow(class="form-group",
                                     column(width = 3,  tags$label("Rayon de floutage")),
                                     column(width = 9, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                                 ),

                                 #Alpha
                                 fluidRow(
                                   class="form-group",
                                   div(
                                     div(class="col-md-3",  tags$label("Opacité  ")),
                                     div(class="col-md-9", sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )
                                   )
                                 ),#,


                                 #Mode de fusion
                                 fluidRow(class="form-group",
                                     column(width = 3, tags$label("Mode de fusion") ),
                                     column(width = 9, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()   )
                                     )
                                 )
                        )#Fin de la ligne
                      )

                    },
                    "drop_shadow_interieure"={

                      tagList(
                        fluidRow(class="ligne_contenu_modal",
                                 #Angle
                                 div(class="form-group",
                                     column(width = 3, tags$label("Angle") ),
                                     column(width = 9, numericInput(ns("param_effet_angle"), label = NULL, min = 0, max=360, width = "100px", value = angle_effet_actif() )
                                     )
                                 ),


                                 #distance
                                 div(class="form-group",
                                     column(width = 3, tags$label("Distance") ),
                                     column(width = 9, numericInput(ns("param_effet_distance"), label = NULL, min = 0, max=NA, width = "100px", value = distance_effet_actif() )
                                     )
                                 ),

                                 #sigma
                                 div(class="form-group",
                                     column(width = 3,  tags$label("Rayon de floutage")),
                                     column(width = 9, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                                 ),

                                 #Alpha
                                 div(
                                   class="form-group",
                                   div(
                                     div(class="col-md-3",  tags$label("Opacité  ")),
                                     div(class="col-md-9", sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )
                                   )
                                 ),#,


                                 #Mode de fusion
                                 div(class="form-group",
                                     column(width = 3, tags$label("Mode de fusion") ),
                                     column(width = 9, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()  )
                                     )
                                 )
                        )#Fin de la ligne
                      )

                    },
                    "innner_glow"={

                      tagList(
                        fluidRow(class="ligne_contenu_modal",

                                 #Rayon
                                 div(class="form-group",
                                     column(width = 3, tags$label("Rayon")),
                                     column(width = 9, numericInput(ns("param_effet_rayon"), label = NULL, min = 0, max=NA, width = "100px", value = rayon_effet_actif() )
                                     )
                                 ),

                                 #sigma
                                 div(class="form-group",
                                     column(width = 3,  tags$label("Rayon de floutage")),
                                     column(width = 9, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                                 ),

                                 #Alpha
                                 div(
                                   class="form-group",
                                   div(
                                     div(class="col-md-3",  tags$label("Opacité  ")),
                                     div(class="col-md-9", sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )
                                   )
                                 ),#,


                                 #Couleur d
                                 div(class="form-group",
                                     column(width = 3,
                                            tags$label("Couleur")),
                                     column(width = 9, colourInput(ns("param_effet_couleur"), label = NULL, value=couleur_effet_actif())
                                     )
                                 ),

                                 #Mode de fusion
                                 div(class="form-group",
                                     column(width = 3, tags$label("Mode de fusion") ),
                                     column(width = 9, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()   )
                                     )
                                 )
                        )#Fin de la ligne
                      )

                    },
                    "outer_glow"={

                      tagList(
                        fluidRow(class="ligne_contenu_modal",

                                 #Rayon
                                 div(class="form-group",
                                     column(width = 3, tags$label("Rayon")),
                                     column(width = 9, numericInput(ns("param_effet_rayon"), label = NULL, min = 0, max=NA, width = "100px", value = rayon_effet_actif() )
                                     )
                                 ),

                                 #sigma
                                 div(class="form-group",
                                     column(width = 3,  tags$label("Rayon de floutage")),
                                     column(width = 9, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                                 ),

                                 #Alpha
                                 div(
                                   class="form-group",
                                   div(
                                     div(class="col-md-3",  tags$label("Opacité  ")),
                                     div(class="col-md-9", sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )
                                   )
                                 ),#,


                                 #Couleur d
                                 div(class="form-group",
                                     column(width = 3,
                                            tags$label("Couleur")),
                                     column(width = 9, colourInput(ns("param_effet_couleur"), label = NULL, value=couleur_effet_actif())
                                     )
                                 ),

                                 #Mode de fusion
                                 div(class="form-group",
                                     column(width = 3, tags$label("Mode de fusion") ),
                                     column(width = 9, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()   )
                                     )
                                 )
                        )#Fin de la ligne
                      )

                    },
                    "source"={

                    },
                    "transformer"={

                    },
                    "flou"={

                    },
                    "coloriser"={

                    }



            )#fin swith




          }else{
            p("ps de données selectionnées")
          }


          #sortie

        })


        ### Gestion d'un effet spécifique choisi par l'utilisateur #####
        observe({
          ####On recoit les valeurs recues dès le clic sur un effet spécifique####
          if(!is.null(input$select_effet_click)){
            data_name_effet<- fromJSON(input$select_effet_click)
            name_effet <-data_name_effet$name_effect
            #effet_actif(name_effet)

            #REderesseent
            switch (name_effet,
                    "dropshadowportee" = {
                      name_effet_vrai <- "drop_shadow_portee"
                      effet_actif(name_effet_vrai)
                    },
                    "dropshadowinterieure"={
                      name_effet_vrai <- "drop_shadow_interieure"
                      effet_actif(name_effet_vrai)

                    },
                    "innnerglow"={
                      name_effet_vrai <- "innner_glow"
                      effet_actif(name_effet_vrai)
                    },
                    "outerglow"={
                      name_effet_vrai <- "outer_glow"
                      effet_actif(name_effet_vrai)
                    },
                    "source"={
                      name_effet_vrai <- "source"
                      effet_actif(name_effet_vrai)
                    },
                    "transformer"={
                      name_effet_vrai <- "transformer"
                      effet_actif(name_effet_vrai)
                    },
                    "flou"={
                      name_effet_vrai <- "flou"
                      effet_actif(name_effet_vrai)
                    },
                    "coloriser"={
                      name_effet_vrai <- "coloriser"
                      effet_actif(name_effet_vrai)
                    }
            )


          }else{
            output$parametres_effets_ui<- renderUI({
              p("Veuiller choisir un effet.")
            })
          }#fin condition sur la valeur recue depuis le serveur


          ####Contrôles sur l'effet actif ###########
          if(!is.null(effet_actif())){

            effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
            effet_en_cours <- effet_actif()

            #print(angle_effet_actif())

            ##Gestioi des la modification du paramètre angle
            if(!is.null(input$param_effet_angle)){
              angle_effet_actif(input$param_effet_angle)
              gauche_angle_effet <- paste("effets_symbologies", effet_actif(),"options",  "angle", sep = "$")
              eval(parse(text = paste( gauche_angle_effet,angle_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_angle", label = NULL, value = angle_effet_actif(), min = 0, max = 360  )
            }


            ##Gestioi des la modification du paramètre distance
            if(!is.null(input$param_effet_distance)){
              distance_effet_actif(input$param_effet_distance)
              gauche_distance_effet <- paste("effets_symbologies", effet_actif(),"options",  "distance", sep = "$")
              eval(parse(text = paste( gauche_distance_effet,distance_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_distance", label = NULL, value = distance_effet_actif(), min = 0, max = NA  )
            }


            ##Gestioi des la modification du paramètre Rayon de floutage
            if(!is.null(input$param_effet_sigma)){
              sigma_effet_actif(input$param_effet_sigma)
              gauche_sigma_effet <- paste("effets_symbologies", effet_actif(),"options",  "sigma", sep = "$")
              eval(parse(text = paste( gauche_sigma_effet ,sigma_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_sigma", label = NULL, value = sigma_effet_actif(), min = 0, max = NA  )
            }

            ##Gestioi des la modification du paramètre alpha
            if(!is.null(input$param_effet_alpha)){
              alpha_effet_actif(input$param_effet_alpha)
              gauche_alpha_effet <- paste("effets_symbologies", effet_actif(),"options",  "alpha", sep = "$")
              eval(parse(text = paste( gauche_alpha_effet ,alpha_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateSliderInput(session, "param_effet_alpha", label = NULL, value = alpha_effet_actif(), min = 0, max = 1, step = 0.1  )
            }


            ##Gestioi des la modification du paramètre mode de fusion
            if(!is.null(input$param_effet_mode_fusion)){

              mode_fusion_effet_actif(input$param_effet_mode_fusion)
              gauche_mode_fusion_effet <- paste("effets_symbologies", effet_actif(),"options",  "mode_fusion", sep = "$")
              eval(parse(text = paste( gauche_mode_fusion_effet ,paste0("'",mode_fusion_effet_actif(),"'"), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateSelectInput(session,"param_effet_mode_fusion", label = NULL,choices = liste_mode_fusion, selected = mode_fusion_effet_actif() )

            }


            ##Gestion des la modification du paramètre Rayon
            if(!is.null(input$param_effet_rayon)){
              rayon_effet_actif(input$param_effet_rayon)
              gauche_rayon_effet <- paste("effets_symbologies", effet_actif(),"options",  "rayon", sep = "$")
              eval(parse(text = paste( gauche_rayon_effet ,rayon_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_rayon", label = NULL, value = rayon_effet_actif(), min = 0, max = NA  )
            }



            ##Gestion des la modification du paramètre couleur
            if(!is.null(input$param_effet_couleur)){
              couleur_effet_actif(input$param_effet_couleur)
              gauche_couleur_effet <- paste("effets_symbologies", effet_actif(),"options",  "couleur", sep = "$")
              eval(parse(text = paste( gauche_couleur_effet ,paste0("'",couleur_effet_actif(),"'"), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateColourInput(session,"param_effet_couleur", label = NULL, value = input$param_effet_couleur )

            }


            #options_effets_symbologie_actif(effets_symbologies)
            print("Options de symbologie")
            print( options_effets_symbologie_actif() )

          }else{
              output$parametres_effets_ui<- renderUI({
                p("Veuiller choisir un effet.")
              })
          }#fin condition sur les effets




        })



        observeEvent(effet_actif(), {

          print("effet actif à ce point")
          print( effet_actif() )

          if( !is.null(effet_actif()) ){

            sortie=ParametresEffets()
            #print(sortie)

            output$parametres_effets_ui <- renderUI({
              tagList(sortie)
            })
          }else{
            output$parametres_effets_ui <- renderUI({
            })
          }

        })


        #actionButton(ns("ajouter_couche"), "", icon = icon("add"), class="btn-primary btn-sm"),
        #actionButton(ns("supp_couche"), "", icon = icon("trash"), class="btn-primary btn-sm")



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

                  }#Fin


                  #actualisation des valeurs concernant les effets (statut) ===> ok
                  gauche_statut_effet <- paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique", "statut_effet",  sep="$")
                  droite_statut_effet <- statut_effet_actif()
                      eval(parse(text =   paste( gauche_statut_effet, droite_statut_effet, sep = "<-")    ))

                  ##on prend en compte les effets choisies (actualisation)
                  gauche_options_effet <- paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique", "effects",  sep="$")
                  droite_options_effet <- list(options_effets_symbologie_actif())
                      eval(parse(text =   paste( gauche_options_effet, droite_options_effet, sep = "<-")    ))



                #on applique les modifications à la liste principale des couches
                liste_couches(copie_couche)

            }
          }



          #on réinitialise certains paramètres



          #on reinitialise les informations sur le choix de la couche de symbologie
          couche_symbologie_actif(NULL)
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
