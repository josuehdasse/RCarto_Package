#Module de  egstion des couches
library(shiny)

#importer les fonctions
#source("./R/fonctions_cartes.R")


mod_gestion_couches_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(style="padding: 0 0 0 30px !important;",
             tagList(
               tags$div(class="list-header",
                        tags$p("liste des couches")
               ),
               actionButton(ns("ajouter_couche"), "", icon = icon("add"), class="btn-primary btn-sm"),
               actionButton(ns("supp_couche"), "", icon = icon("trash"), class="btn-primary btn-sm")
             )

    ),
    fluidRow(

             tagList(

               uiOutput(ns("test_couches")),

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


mod_gestion_couches_server<- function(input, output, session, liste_couches, resolution_page_actif){
  ns<- session$ns


            ##Tracking des éléments caractéristques de la symbologie d'une couche####
            mame_couche_actif <- reactiveVal(#le nom de la couche active
              NULL
            )

            position_couche_actif <- reactiveVal(#le nom de la couche active
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
          req(input$confirmer_ajout_layer)
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
            name= input$select_couche,
            type_symbologie="unique",
            visible=TRUE,
            geometrie= unique(as.character(st_geometry_type(couche_import))), #on controle la geometrie pour gerer la crte plus tard (point, ligne, polygone, etc)
            options_symbologie_couche=list(
              options_symbologie_unique=list(
                symbologie_1=list(
                  name="symbologie_1",
                  label="Remplissage simple 1",
                  position=nbre_couches_ajoutes+1,
                  #Gestion des symboles uniques de la couche
                  couleur_symbole= liste_couleurs[nbre_couches_ajoutes+1],
                  style_fill_symbologie="continu",
                  legende=input$select_couche,
                  couleur_trait="#DF1616",
                  style_trait="solid",
                  epaisseur_trait=1,
                  #opacity_fill=1,
                  #opacity_border=1,
                  statut_effet=FALSE,
                  effects=list(),
                  patterns=list(
                    pattern="stripe",

                    pattern_spacing=0.009,#Esapce entre deux motifs
                    pattern_density=0.1,#Densité
                    pattern_angle=45 , #Angle du motif,
                    pattern_size=0.5,#taille du motif
                    pattern_colour="#4682B445",#la couleur de bordure du patterne
                    pattern_linetype="solid",##le type de ligne du pattern

                    #pour le pattern gradient
                    pattern_frequency=5,
                    pattern_fill2="green",
                    pattern_orientation="horizontal",

                    #pour le pattern
                    pattern_type="squish",#Tile (repété), fit (ajusté), squish (déformé), expand (avec dégradé)
                    pattern_filename="./www/image.jpg",
                    pattern_scale=1
                  )
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



        ## Liste reactive  des couches avec leurs options de controle  ############


        observe({

          req(length(liste_couches())>0)



          for (i in 1:length(liste_couches()) ) {

            couche=liste_couches()[i]

            nom_couche=names(liste_couches())[i]
            type_symbologie= couche[[ nom_couche]]$type_symbologie


            #laboration du graphique de la couche courante
            #on copie les options de symbologie de la couche
            if(type_symbologie=="unique"){
              options_symbologie = couche[[nom_couche]]$options_symbologie_couche$options_symbologie_unique
            }


            positions<- sapply(options_symbologie, function(x) x$position )

            options_symbologie<- options_symbologie[order(positions, decreasing = TRUE)]#on fait pour que la premìère couche de symbologie soit en haut de la liste

            #Le jeu de données de visualisation pour les polygones
            data_graph <- rbind( c(0.0), c(1,0), c(1,1), c(0,1), c(0,0) )#les données du graphique  ==>> la couche
            polygone <- st_polygon(list(data_graph))
            sfc_obj <- st_sfc(polygone)
            data_points <- data.frame(names=c("A", "B", "C", "D"))
            data_points$geometry <- sfc_obj
            data_points <- st_as_sf(data_points)

            #initialisation du graphique
            graphique <- ggplot()


            code_symbologies_couches_graph <- generer_code_symbologies("data_points", couche[[nom_couche]]$type_symbologie, "POLYGON", options_symbologie, couche[[nom_couche]]$position_couche )

            #print(code_symbologies_couches_graph)
            graphique <- eval(parse(text =paste("ggplot()", code_symbologies_couches_graph, sep = "+") ))  + eval(parse(text = theme_graphique))


            output[[paste0("graph_", couche$name )]] <- renderImage({

              outfile<- tempfile(fileext = "png")
              png(outfile, width =50, height =50, res = resolution_page_actif() )
              print(graphique)
              dev.off()
              list(src=outfile)

            }, deleteFile=TRUE)




          }



        })

        output$test_couches <- renderUI({
          ListeCouchesUI()
        })


        ListeCouchesUI <- reactive({
          CouchesUI<- lapply(liste_couches() , function(i){ #liste des couche


            #On commence l'élément à afficher ici
            tags$li(
              class="list_item",
              tagList(
                #la case à cocher
                if(i$visible){
                  tags$input(
                    type="checkbox",
                    checked="checked",
                    id=paste0("checked0_", i$name),
                    width=20,
                    height=20,
                    onclick="visible_couches(this.id)"
                  )

                }else{

                  tags$input(
                    type="checkbox",
                    id=paste0("checked0_", i$name),
                    width=20,
                    height=20,
                    onclick="visible_couches(this.id)"
                  )#fin input

                }

              ),
              tagList(
                tags$div(class="symbole-container",
                         #la représentation graphique des symbologies de la couche ici
                        imageOutput(ns(paste0("graph_", i$name )))

                         )
              )



              #i$label
            )#fin li

          })

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
          req(donnees_symbologie_couche())

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

          #on prend la position
          position_symbologie <- eval(parse(text = paste("copie_couche", name_couche ,"position", sep="$") ))
          position_couche_actif(position_symbologie)


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
                  actionButton(ns("bouton_ok_symbologie_couche"), "Ok", class="btn-success"),
                  actionButton(ns("bouton_appliquer_symbologie_couche"), "Appliquer", class="btn-success"),
                  modalButton("Annuler")
              )
            ),

            fluidRow(
              withSpinner(
                uiOutput(ns("options_symbologie_layer_ui")) )
            ),#on paramètre le contenu de la fenêtre cible ici
            #trigger = "ajouter_couche",
            #scrollable=TRUE,
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





        #### On affiche d'abord les couches de la symbologie  avec leurs oprions de controles et visualisations##########

        ###### Elaborer La liste des reactives des couches de symbologie ############
        couches_symbologieUI <- reactive({
          symbologieUI <- lapply(options_symbologies_couche_actif() , function(i){ #les effets qui sont ajoutés à la couche courante

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
                  tags$a(href="#",
                    tags$span(
                      i$label,
                      id=paste0(i$name),
                      onclick="afficher_options_couche_symbologie(this.id)"
                    )
                  )

                )
                #i$label
              )#fin li

          })

        })



        ######on recupère la valeur choisie par l'utlisateur pour lui donner la main afin de faire les paramétrages #####
        observeEvent(input$choix_couche_symbologie, {

          couche_symbologie_actif(NULL)

          req(input$choix_couche_symbologie)
          req(options_symbologies_couche_actif())

          #on actualise la valeur de la synbologie en cours tel que choisi par l'utlisateu
            data_couche_select<- fromJSON(input$choix_couche_symbologie)
            name_couche_symbologie <-data_couche_select$name
            couche_symbologie_actif(name_couche_symbologie)

            print(couche_symbologie_actif())


            #peut déjà ici propager les réactives à utiliser dans l'application
            #copie d ela couche des valeurs
            copie_symbologie <- options_symbologies_couche_actif()

            #on fait une copie de la couche des otpions de la symbologie
            #options_symbologies_couche =options_symbologies_couche_actif()

            #on echerche les valeurs de la couche de symbologie et on les affiche
            #if(type_symbologie_actif()=="unique"){#cas du symbole unique
            couleur_fill <- eval(parse(text = paste("copie_symbologie",name_couche_symbologie,   "couleur_symbole",  sep="$") ))
            print(paste("copie_symbologie",name_couche_symbologie,   "couleur_symbole",  sep="$"))
             couleur_remplissage_symbole_actif(couleur_fill)


            #opacity_fill <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"opacity_fill",  sep="$") ))
            #opacity_fill_actif(opacity_fill)


            #opacity_border <- eval(parse(text = paste("copie_couche", mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",name_couche_symbologie,"opacity_border",  sep="$") ))
            #opacity_border_actif(opacity_border)

            style_trait <- eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"style_trait",  sep="$") ))
            style_trait_actif(style_trait)

            style_fill_symbologie <- eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"style_fill_symbologie",  sep="$") ))
            style_fill_symbologie_actif(style_fill_symbologie)

            couleur_trait <- eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"couleur_trait",  sep="$") ))
            couleur_trait_actif(couleur_trait)

            epaisseur_trait <- eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"epaisseur_trait",  sep="$") ))
            epaisseur_trait_actif(epaisseur_trait)

            #les informations sur les effets
            statut_effet <- eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"statut_effet",  sep="$") ))
            statut_effet_actif(statut_effet)


            #les opions du pattern de la symbologie


            #les options sur les effets appliqués à cette couche
            #options_effets_symbologie<- eval(parse(text = paste("copie_couche",mame_couche_actif(),"options_symbologie_couche", "options_symbologie_unique",paste0(couche_symbologie_actif()),"effects",  sep="$") ))


        })


        ###### Elaboration du graphique de La prévisualisation des choix de la symbologie #########
        previsulationSymbologieUI <- reactive({

          #on copie les options de symbologie de la couche
          options_symbologie = options_symbologies_couche_actif()

          positions<- sapply(options_symbologie, function(x) x$position )

          options_symbologie<- options_symbologie[order(positions, decreasing = TRUE)]#on fait pour que la premìère couche de symbologie soit en haut de la liste

          #Le jeu de données de visualisation pour les polygones
          data_graph <- rbind( c(0.0), c(1,0), c(1,1), c(0,1), c(0,0) )#les données du graphique  ==>> la couche
          polygone <- st_polygon(list(data_graph))
          sfc_obj <- st_sfc(polygone)
          data_points <- data.frame(names=c("A", "B", "C", "D"))
          data_points$geometry <- sfc_obj
          data_points <- st_as_sf(data_points)

          #initialisation du graphique
          graphique <- ggplot()

          print(options_symbologie)

          code_symbologies_couches_graph <- generer_code_symbologies("data_points", type_symbologie_actif(), "POLYGON", options_symbologie, position_couche_actif() )

          #print(code_symbologies_couches_graph)

          graphique <- eval(parse(text =paste("ggplot()", code_symbologies_couches_graph, sep = "+") ))  + eval(parse(text = theme_graphique))


          graphique
        })



        ###### On renvoie l'image de la prévisualisation (interactive) ########
        output$previsualisation_symbologie_ui <- renderImage({
          req(options_symbologies_couche_actif())

          graphique <- previsulationSymbologieUI()

          outfile<- tempfile(fileext = "png")
          png(outfile, width =160, height =160, res = resolution_page_actif() )
          print(graphique)
          dev.off()
          list(src=outfile)

        }, deleteFile=TRUE)



        ######Affichage des Options de contrôle sur les symbologies de la couche (prévisualisation, liste et controles) #####
        output$couches_symbologie <- renderUI({
          req(input$selec_type_symbole)#Eviter les valeurs nuls

              tagList(
                fluidRow(class="ligne_contenu_modal",
                  column(width = 3, style="height:170px;",  imageOutput(ns("previsualisation_symbologie_ui"))),#place pour la visualisation du rendu final de la symbologie sur toutes les couches
                  column(width = 8, class="list-items-couches",
                         tagList(
                           tags$p("Remplissage"),
                           tags$ul(
                             couches_symbologieUI()#liste des couches de sumbologie pour le type sélectionné
                           )
                         )

                         ),#liste des couches de symbologies
                  column(width = 1, style="height:170px;",
                         tagList(
                           actionButton(ns("ajout_symbologie"), "", icon = icon("add"), class="btn-primary btn-sm"),
                           uiOutput(ns("ui_dupliquer_couche_symbologie")),
                           uiOutput(ns("ui_suppression_couche_symbologie")),
                           uiOutput(ns("ui_monter_couche_symbologie")),
                           uiOutput(ns("ui_descendre_couche_symbologie"))

                         )

                         )#options de controle de la liste des symbologies
                )
              )#fin taglist



        })



        ####on renvoie les caractéristiques de la gestion des options de contrôle  sur la symbologie ####


        ######On  rend à l'utilisateur les options de  modification de la symbologie en cours#######
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


                #on met ici les options de gestion des motifs de remplissage
                fluidRow(style="width:85%;position:relative ;left: 10%;right: 10%;", class="ligne_sous_details_modal",
                  uiOutput(ns("gestionnaire_pattern_couche_ui"))
                ),



                #Couleur de trait
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Couleur de trait")
                         ),
                         column(width = 8,
                                colourInput(ns("select_couleur_trait"), label = NULL, value=couleur_trait_actif() , allowTransparent = TRUE, palette = "square", closeOnClick = TRUE  )
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

                fluidRow(class="ligne_contenu_modal",
                    checkboxInput(ns("select_effet_symbologie"), "Effects", value = statut_effet_actif() )
                ),

                fluidRow(style="width:85%;position:relative ;left: 10%;right: 10%;",
                    uiOutput(ns("gestion_effets_symbologie_ui"))
                )


                #fin fluid row
              )#Fin withTag


            })



          }




        })


        ######Suivi des modification sur les paramètres de definition d'une symbologie#########
        observe({

          req(couche_symbologie_actif()) #il faut qu'une couche soit activée ou sélectionnée

          options_symbologie = options_symbologies_couche_actif()

          #suivi de la modification des coukeurs de remplissage (polygones)
          req(input$select_couleur_symbole)
          updateColourInput(session, "select_couleur_symbole", value=input$select_couleur_symbole)
          couleur_remplissage_symbole_actif(input$select_couleur_symbole)
          gauche_req_couleur_symbole <- paste("options_symbologie", couche_symbologie_actif(), "couleur_symbole", sep = "$")
          eval(parse(text = paste(gauche_req_couleur_symbole, paste0("'", input$select_couleur_symbole, "'"), sep="<-"  ) ))#on applique les mofications directement
          options_symbologies_couche_actif(options_symbologie)

          #Suivi de de la modification du style de remplissage
          req(input$select_style_fill_symbologie)
          updateSelectInput(session, "select_style_fill_symbologie", label = NULL, choices =list("Continue"="continu",
                                                                                                 "Pas de remplissage"="blank",
                                                                                                 "Motif"="motif"),  selected = input$select_style_fill_symbologie    )
          style_fill_symbologie_actif(input$select_style_fill_symbologie)#actualisation du reactif
          gauche_req_style_fill_symbologie <- paste("options_symbologie", couche_symbologie_actif(), "style_fill_symbologie", sep = "$")
          eval(parse(text = paste(gauche_req_style_fill_symbologie, paste0("'", input$select_style_fill_symbologie, "'"), sep="<-"  ) ))#on applique les mofications directement
          options_symbologies_couche_actif(options_symbologie)



          #suivi du couleur des traits
          req(input$select_couleur_trait)
          updateColourInput(session, "select_couleur_trait", value=input$select_couleur_trait)
          couleur_trait_actif(input$select_couleur_trait)
          gauche_req_couleur_trait <- paste("options_symbologie", couche_symbologie_actif(), "couleur_trait", sep = "$")
          eval(parse(text = paste(gauche_req_couleur_trait, paste0("'", input$select_couleur_trait, "'"), sep="<-"  ) ))#on applique les mofications directement
          options_symbologies_couche_actif(options_symbologie)


          #Suivi de la largeur des traits
          req(input$select_epaisseur_trait)
          updateNumericInput(session, "select_epaisseur_trait", min = 0, max=NA, value=input$select_epaisseur_trait)
          epaisseur_trait_actif(input$select_epaisseur_trait)
          gauche_req_epaisseur_trait <- paste("options_symbologie", couche_symbologie_actif(), "epaisseur_trait", sep = "$")
          eval(parse(text = paste(gauche_req_epaisseur_trait, input$select_epaisseur_trait, sep="<-"  ) ))#on applique les mofications directement
          options_symbologies_couche_actif(options_symbologie)


          #Suivi du type d etrait
          req(input$select_couleur_symbole)
          updateColourInput(session, "select_couleur_symbole", value=input$select_couleur_symbole)
          couleur_remplissage_symbole_actif(input$select_couleur_symbole)
          gauche_req_couleur_symbole <- paste("options_symbologie", couche_symbologie_actif(), "couleur_symbole", sep = "$")
          eval(parse(text = paste(gauche_req_couleur_symbole, paste0("'", input$select_couleur_symbole, "'"), sep="<-"  ) ))#on applique les mofications directement
          options_symbologies_couche_actif(options_symbologie)

          #Suivi de de la modification du style de remplissage
          req(input$select_style_trait)
          updateSelectInput(session, "select_style_trait", label = NULL, choices =list("Ligne continue"="solid",
                                                                                                 "Pas de ligne"="blank",
                                                                                                 "Ligne en tiret"="longdash",
                                                                                                 "Ligne en pointillet"="dotted",
                                                                                                 "Ligne en tiret-point"="dotdash",
                                                                                                 "Ligne en tiret-point-point"="twodash",
                                                                                                 "Tirets"="dashed"),  selected = input$select_style_trait   )
          style_trait_actif(input$select_style_trait)#actualisation du reactif
          gauche_req_style_trait <- paste("options_symbologie", couche_symbologie_actif(), "style_trait", sep = "$")
          eval(parse(text = paste(gauche_req_style_trait, paste0("'", input$select_style_trait, "'"), sep="<-"  ) ))#on applique les mofications directement
          options_symbologies_couche_actif(options_symbologie)




        })




        #####Controle des couches de symbologie ##########################
        ###### Ajout des couches de symbologie ##########################
        observeEvent(input$ajout_symbologie,{

          req(input$ajout_symbologie)
          copie_symbologie_couche=options_symbologies_couche_actif()

          #on ajoute une nouvelle couche en fond blanc et bords noirs d’épaisseur 1
          nouvelle_couche_symbologie = list(
            name=paste("symbologie",length(copie_symbologie_couche)+1, sep="_" ),
            label=paste("Remplissage simple",length(copie_symbologie_couche)+1, sep=" " ),
            position=length(copie_symbologie_couche)+1,

            couleur_symbole= "#4F484800",#transparent avec le alpha
            style_fill_symbologie="continu",
            legende="",
            couleur_trait="#4F4848",
            style_trait="solid",
            epaisseur_trait=1,
            statut_effet=FALSE,
            effects=list(),
            patterns=list(
              pattern="stripe",
              pattern_spacing=0.009,#Esapce entre deux motifs
              pattern_density=0.1,#Densité
              pattern_angle=45 , #Angle du motif,
              pattern_size=0.5,#taille du motif
              pattern_colour="#4682B445",#la couleur de bordure du patterne
              pattern_linetype="solid",##le type de ligne du pattern

              #pour le pattern gradient
              pattern_frequency=5,
              pattern_fill2="green",
              pattern_orientation="horizontal",

              #pour le pattern
              pattern_type="squish",#Tile (repété), fit (ajusté), squish (déformé), expand (avec dégradé)
              pattern_filename="./www/image.jpg",
              pattern_scale=1
            )
          )

          copie_symbologie_couche <- append(
            copie_symbologie_couche, list(nouvelle_couche_symbologie)
          )

          names(copie_symbologie_couche)[length(copie_symbologie_couche)] <- paste("symbologie",length(copie_symbologie_couche), sep="_" )

          print(copie_symbologie_couche)

          #on applique
          options_symbologies_couche_actif(copie_symbologie_couche)

        })

        ###### Dupliquer couches de symbologie ##########################
        output$ui_dupliquer_couche_symbologie  <- renderUI({
          req(couche_symbologie_actif())
          actionButton(ns("dupliquer_symbologie"), "", icon = icon("clone"), class="btn-primary btn-sm")
        })

        observeEvent(input$dupliquer_symbologie,{
          req(input$dupliquer_symbologie)

          copie_symbologie_couche=options_symbologies_couche_actif()
          copie_couche <- eval(parse(text = paste( "copie_symbologie_couche", couche_symbologie_actif(), sep = "$" )   ))


          label_copie_couche <- eval(parse(text = paste( "copie_couche","label",  sep = "$" )   ))

          #on modifie le label avec la mention copie
          gauche<-  paste( "copie_couche", "label", sep = "$"  )
          eval(parse(text = paste( gauche,  paste0("'", label_copie_couche, " copie","'"), sep = "<-"  )   ))

          #on modifie le name en meme temps
          gauche_name<-  paste( "copie_couche", "name", sep = "$"  )
          eval(parse(text = paste( gauche_name,  paste0("'", paste("symbologie",length(copie_symbologie_couche)+1, sep="_" ), "'" )  , sep = "<-"  )   ))

          #on modifie l'ordre ou la position
          gauche_position<-  paste( "copie_couche", "position", sep = "$"  )
          eval(parse(text = paste( gauche_position,  length(copie_symbologie_couche)+1  , sep = "<-"  )   ))


          copie_symbologie_couche <- append(
            copie_symbologie_couche, list(copie_couche)
          )

          names(copie_symbologie_couche)[length(copie_symbologie_couche)] <- paste("symbologie",length(copie_symbologie_couche), sep="_" )

          options_symbologies_couche_actif(copie_symbologie_couche)

        })



        ###### Supprimer des couches de symbologie ##########################
          output$ui_suppression_couche_symbologie <- renderUI({
            req(length(options_symbologies_couche_actif())> 1 )
            req(couche_symbologie_actif())

            actionButton(ns("delete_symbologie"), "", icon = icon("trash"), class="btn-primary btn-sm")
          })

        observeEvent(input$delete_symbologie, {
          req(input$delete_symbologie)#On evite les exécutions sur les valeurs nuls

          copie_symbologie_couche=options_symbologies_couche_actif()


          gauche<- paste( "copie_symbologie_couche", couche_symbologie_actif(), sep = "$" )
          eval(parse(text = paste(gauche, "NULL", sep = "<-")  ))

          options_symbologies_couche_actif(copie_symbologie_couche)#on implémente la suppression dans la liste des couches


        })


        #uiOutput("ui_monter_couche_symbologie"),
        #uiOutput("ui_descendre_couche_symbologie")





        ##Gestion des patterns (motifs de remplissage)#####

        pattern_couche_actif<- reactiveVal(NULL)
        pattern_spacing_actif<- reactiveVal(NULL)
        pattern_density_actif<- reactiveVal(NULL)
        pattern_angle_actif<- reactiveVal(NULL)
        pattern_size_actif <- reactiveVal(NULL)
        pattern_colour_actif <- reactiveVal(NULL)
        pattern_linetype_actif<-reactiveVal(NULL)
        pattern_fill2_actif <- reactiveVal(NULL)
        pattern_frequency_actif <- reactiveVal(NULL)
        pattern_orientation_actif <- reactiveVal(NULL)
        pattern_type_actif <- reactiveVal(NULL)
        pattern_filename_actif <- reactiveVal(NULL)
        pattern_scale_actif <- reactiveVal(NULL)

        ###On renvoit le selecteur du type du motif######
        output$gestionnaire_pattern_couche_ui <- renderUI({
          req(input$select_style_fill_symbologie=="motif")#seulement au cas òu le type de rempolissge choisi correspond à un motif
          copie_symbologie <- options_symbologies_couche_actif()

          #### On récupère les paramètres
          pattern_couche <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern",  sep="$") ))
          pattern_couche_actif(pattern_couche)

          pattern_spacing <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_spacing",  sep="$") ))
          pattern_spacing_actif(pattern_spacing)

          pattern_density <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_density",  sep="$") ))
          pattern_density_actif(pattern_density)

          pattern_angle <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_angle",  sep="$") ))
          pattern_angle_actif(pattern_angle)

          pattern_size <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_size",  sep="$") ))
          pattern_size_actif(pattern_size)

          pattern_colour <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_colour",  sep="$") ))
          pattern_colour_actif(pattern_colour)

          pattern_linetype <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_linetype",  sep="$") ))
          pattern_linetype_actif(pattern_linetype)

          pattern_frequency <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "frequency",  sep="$") ))
          pattern_frequency_actif(pattern_frequency)

          pattern_fill2 <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_fill2",  sep="$") ))
          pattern_fill2_actif(pattern_fill2)

          pattern_orientation <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_orientation",  sep="$") ))
          pattern_orientation_actif(pattern_orientation)

          pattern_type <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_type",  sep="$") ))
          pattern_type_actif(pattern_type)


          pattern_filename <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_filename",  sep="$") ))
          pattern_filename_actif(pattern_filename)

          pattern_scale <- eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_scale",  sep="$") ))
          pattern_scale_actif(pattern_scale)


          tagList(
            fluidRow(class="form-group ligne_contenu_modal option_pattern",
              column(width = 4,
                     tags$label("Type de motif ")
              ),
              column(width = 8,
                     selectInput(ns("select_style_pattern"), label = NULL,
                                 choices = list("Lignes parallèles"="stripe",
                                                "Lignes croisées"="crosshatch",
                                                "Cercles / Point circulaire "="circle",
                                                "Gradient"="gradient",
                                                "Image"="image",
                                                "Vagues"="wave"), selected = pattern_couche_actif(), width = "80%" )
              )

            ),

            uiOutput(ns("options_controle_type_pattern_ui"))

          )

        })

        ###On renvoie les options de controle pour le type de motif # choisi par l'utilsateur#####
        output$options_controle_type_pattern_ui <- renderUI({
            req(input$select_style_pattern)

            if(input$select_style_pattern=="stripe"){
              tagList(

                #pattern_spacing
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Espacement")
                         ),
                         column(width = 8,
                                numericInput(ns("select_pattern_spacing"), label = NULL, min = 0, max=NA, step = 0.01, width = "100px", value =  pattern_spacing_actif() )
                         )
                ),

                #pattern_angle
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Angle (en degré))")
                         ),
                         column(width = 8,
                                numericInput(ns("select_pattern_angle"), label = NULL, min = 0, max=360, width = "100px", value =  pattern_angle_actif() )
                         )
                ),

                #pattern_size
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Taille")
                         ),
                         column(width = 8,
                                numericInput(ns("select_pattern_size"), label = NULL, min = 0, max=NA, width = "100px", step = 0.01, value =  pattern_size_actif() )
                         )
                ),



                #pattern_colour
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Couleur de trait")
                         ),
                         column(width = 8,
                                colourInput(ns("select_pattern_colour"), label = NULL, value=pattern_colour_actif() , allowTransparent = TRUE, palette = "square", closeOnClick = TRUE  )
                         )
                ),

                #pattern_linetype
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Style de trait ")
                         ),
                         column(width = 8,
                                selectInput(ns("select_pattern_linetype"), label = NULL,
                                            choices = list("Ligne continue"="solid",
                                                           "Pas de ligne"="blank",
                                                           "Ligne en tiret"="longdash",
                                                           "Ligne en pointillet"="dotted",
                                                           "Ligne en tiret-point"="dotdash",
                                                           "Ligne en tiret-point-point"="twodash",
                                                           "Tirets"="dashed"), selected = pattern_linetype_actif(), width = "80%" )
                         )


                )

              )#fin du taglist
            }else if (input$select_style_pattern %in% c("crosshatch", "circle")){
              #ils ont une surface, donc important choisir une couleur de remplissage et la densité
              tagList(

                #pattern_spacing
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Espacement")
                         ),
                         column(width = 8,
                                numericInput(ns("select_pattern_spacing"), label = NULL, min = 0, max=NA, step = 0.01, width = "100px", value =  pattern_spacing_actif() )
                         )
                ),


                # pattern_density
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Densité")
                         ),
                         column(width = 8,
                                numericInput(ns("select_pattern_density"), label = NULL, min = 0, max=NA, step = 0.1, width = "100px", value =  pattern_density_actif() )
                         )
                ),



                #pattern_angle
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Angle (en degré))")
                         ),
                         column(width = 8,
                                numericInput(ns("select_pattern_angle"), label = NULL, min = 0, max=360, width = "100px", value =  pattern_angle_actif() )
                         )
                ),

                #pattern_size
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Taille")
                         ),
                         column(width = 8,
                                numericInput(ns("select_pattern_size"), label = NULL, min = 0, max=NA, width = "100px", step = 0.01, value =  pattern_size_actif() )
                         )
                ),



                #pattern_colour
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Couleur de trait")
                         ),
                         column(width = 8,
                                colourInput(ns("select_pattern_colour"), label = NULL, value=pattern_colour_actif() , allowTransparent = TRUE, palette = "square", closeOnClick = TRUE  )
                         )
                ),

                #pattern_linetype
                fluidRow(class="form-group ligne_contenu_modal option_pattern",
                         column(width = 4,
                                tags$label("Style de trait ")
                         ),
                         column(width = 8,
                                selectInput(ns("select_pattern_linetype"), label = NULL,
                                            choices = list("Ligne continue"="solid",
                                                           "Pas de ligne"="blank",
                                                           "Ligne en tiret"="longdash",
                                                           "Ligne en pointillet"="dotted",
                                                           "Ligne en tiret-point"="dotdash",
                                                           "Ligne en tiret-point-point"="twodash",
                                                           "Tirets"="dashed"), selected = style_trait_actif(), width = "80%" )
                         )


                )

              )#fin du taglist

            }else if(input$select_style_pattern=="gradient"){

                tagList(

                  #pattern_size
                  fluidRow(class="form-group ligne_contenu_modal option_pattern",
                           column(width = 4,
                                  tags$label("Fréquence")
                           ),
                           column(width = 8,
                                  numericInput(ns("select_pattern_frequency"), label = NULL, min = 0, max=NA, width = "100px", step = 0.01, value =  pattern_frequency_actif() )
                           )
                  ),

                  #pattern_colour
                  fluidRow(class="form-group ligne_contenu_modal option_pattern",
                           column(width = 4,
                                  tags$label("Couleur2")
                           ),
                           column(width = 8,
                                  colourInput(ns("select_pattern_fill2"), label = NULL, value=  pattern_fill2_actif() , allowTransparent = TRUE, palette = "square", closeOnClick = TRUE  )
                           )
                  ),


                  fluidRow(class="form-group ligne_contenu_modal option_pattern",
                           column(width = 4,
                                  tags$label("Orientation ")
                           ),
                           column(width = 8,
                                  selectInput(ns("select_pattern_orientation"), label = NULL,
                                              choices = list("Radial"="radial",
                                                             "Horizontal"="horizontal",
                                                             "Vertical"="vertical",
                                                             "Diagonal"="diagonal"), selected =  pattern_orientation_actif(), width = "80%" )
                           )


                  )

                )
            }#

              #"image"={},



        })


        ### Suivi des modifiactions effectuées au niveau des otptions de controle des patterns#####
        observeEvent(input$select_style_pattern,{
          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          #suivi du type de patterne de la symbologie
          req(input$select_style_pattern)
          pattern_couche_actif(input$select_style_pattern)
          updateSelectInput(session, "select_style_pattern",choices = list("Lignes parallèles"="stripe",
                                                                           "Lignes croisées"="crosshatch",
                                                                           "Cercles / Point circulaire "="circle",
                                                                           "Gradient"="gradient",
                                                                           "Image"="image",
                                                                           "Vagues"="wave"), selected = pattern_couche_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern <- input$select_style_pattern
          options_symbologies_couche_actif(copie_symbologie)
        })




        #sapcing
        observeEvent(input$select_pattern_spacing, {
          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          #Espacements
          req(input$select_pattern_spacing)
          pattern_spacing_actif(input$select_pattern_spacing)
          updateNumericInput(session, "select_pattern_spacing", label = NULL, min = 0, max=NA, value =  pattern_spacing_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_spacing <- input$select_pattern_spacing
          options_symbologies_couche_actif(copie_symbologie)

        })

        #density
        observeEvent(input$select_pattern_density, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          req(input$select_pattern_density)
          pattern_density_actif(input$select_pattern_density)
          updateNumericInput(session, "select_pattern_density", label = NULL, min = 0, max=NA, value =  pattern_density_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_density <- input$select_pattern_density

          options_symbologies_couche_actif(copie_symbologie)
        })

        #Angle

        observeEvent(input$select_pattern_angle, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()
          req(input$select_pattern_angle)
          pattern_angle_actif(input$select_pattern_angle)
          updateNumericInput(session, "select_pattern_angle", label = NULL, min = 0, max=NA, value =  pattern_angle_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_angle <- input$select_pattern_angle

          options_symbologies_couche_actif(copie_symbologie)
        })

        #size
        observeEvent(input$select_pattern_size,{

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          req(input$select_pattern_size)
          pattern_size_actif(input$select_pattern_size)
          updateNumericInput(session, "select_pattern_size", label = NULL, min = 0, max=NA, value =  pattern_size_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_size <- input$select_pattern_size

          options_symbologies_couche_actif(copie_symbologie)
        })


        #Pattern colour

        observeEvent(input$select_pattern_colour, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()


          req(input$select_pattern_colour)
          pattern_colour_actif(input$select_pattern_colour)
          updateColourInput(session, "select_pattern_colour", label = NULL, value =  pattern_colour_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_colour <- input$select_pattern_colour
          options_symbologies_couche_actif(copie_symbologie)

        })



        observeEvent(input$select_pattern_linetype, {
          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          #Type de ligne du motif
          req(input$select_pattern_linetype)
          pattern_linetype_actif(input$select_pattern_linetype)
          updateSelectInput(session, "select_pattern_linetype", label = NULL, choices = list("Ligne continue"="solid",
                                                                                             "Pas de ligne"="blank",
                                                                                             "Ligne en tiret"="longdash",
                                                                                             "Ligne en pointillet"="dotted",
                                                                                             "Ligne en tiret-point"="dotdash",
                                                                                             "Ligne en tiret-point-point"="twodash",
                                                                                             "Tirets"="dashed"), selected  =  pattern_linetype_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_linetype <- input$select_pattern_linetype

          options_symbologies_couche_actif(copie_symbologie)
        })


        #frequence
        observeEvent(input$select_pattern_frequency, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          req(input$select_pattern_frequency)
          pattern_frequency_actif(input$select_pattern_frequency)
          updateNumericInput(session, "select_pattern_frequency", label = NULL, min = 0, max=NA, value =  pattern_frequency_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_frequency <- input$select_pattern_frequency
          options_symbologies_couche_actif(copie_symbologie)

        })


        #fill2
        observeEvent(input$select_pattern_fill2, {
          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          req(input$select_pattern_fill2)
          pattern_fill2_actif(input$select_pattern_fill2)
          updateColourInput(session, "select_pattern_fill2", label = NULL, value =  pattern_fill2_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_fill2 <- input$select_pattern_fill2

          options_symbologies_couche_actif(copie_symbologie)

        })

        observeEvent(input$select_pattern_orientation, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- options_symbologies_couche_actif()

          req(input$select_pattern_orientation)
          pattern_orientation_actif(input$select_pattern_orientation)
          updateSelectInput(session, "select_pattern_orientation", label = NULL, choices = list("Radial"="radial",
                                                                                                "Horizontal"="horizontal",
                                                                                                "Vertical"="vertical",
                                                                                                "Diagonal"="diagonal"), selected  =  pattern_orientation_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_orientation <- input$select_pattern_orientation

          options_symbologies_couche_actif(copie_symbologie)



        })



        ##Gestion des effets #####
        observeEvent(input$select_effet_symbologie,{

          #gestion de l'affichaage des effets des symbologies
          if(input$select_effet_symbologie){
            nouveau_statut_effet<-TRUE

          }else{
            nouveau_statut_effet<-FALSE
          }

          statut_effet_actif(nouveau_statut_effet)

        })


        observeEvent(statut_effet_actif(),{

          req(statut_effet_actif())
          req(couche_symbologie_actif())


          #on modifie les données sur la liste ds effest de la couche de symbologie
          #statut_effet

          options_symbologie = options_symbologies_couche_actif()

          #suivi de la modification des coukeurs de remplissage (polygones)
          options_symbologie[[couche_symbologie_actif()]]$statut_effet <- statut_effet_actif()
          options_symbologies_couche_actif(options_symbologie)



            output$gestion_effets_symbologie_ui <- renderUI({
              if(input$select_effet_symbologie){#si l'on coché la case "effets"
                tagList(
                  fluidRow(class="ligne_contenu_modal option_effet",
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
                  #fluidRow(class="ligne_contenu_modal option_effet",
                    uiOutput(ns("parametres_effets_ui"))
                  #)
                )#fluidpage

              }



              #tagQuery(paste0("#", ns("liste_effets_associes_symbologie")))$empty()$append(uiEffets)$run

            })#Fin actualisation options des effets de la symbologie



        } )



        ### Ajout une couche d'effets ##########
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

                                 #Angle
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4, tags$label("Angle") ),
                                     column(width = 8, numericInput(ns("param_effet_angle"), label = NULL, min = 0, max=360, width = "100px", value = angle_effet_actif() )
                                     )
                                 ),

                                 #distance
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4, tags$label("Distance") ),
                                     column(width = 8, numericInput(ns("param_effet_distance"), label = NULL, min = 0, max=NA, width = "100px", value = distance_effet_actif() )
                                     )
                                 ),

                                 #sigma
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4,  tags$label("Rayon de floutage")),
                                     column(width = 8, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                                 ),

                                 #Alpha
                                 fluidRow(class="form-group ligne_contenu_modal",
                                      column(width = 4,  tags$label("Opacité  ")),
                                      column(width = 8, sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )

                                 ),#,


                                 #Mode de fusion
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4, tags$label("Mode de fusion") ),
                                     column(width = 8, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()   )
                                     )
                                 )

                      )

                    },
                    "drop_shadow_interieure"={

                      tagList(
                          #Angle
                          fluidRow(class="form-group ligne_contenu_modal",
                                   column(width = 4, tags$label("Angle") ),
                                   column(width = 8, numericInput(ns("param_effet_angle"), label = NULL, min = 0, max=360, width = "100px", value = angle_effet_actif() )
                                   )
                          ),

                          #distance
                          fluidRow(class="form-group ligne_contenu_modal",
                                   column(width = 4, tags$label("Distance") ),
                                   column(width = 8, numericInput(ns("param_effet_distance"), label = NULL, min = 0, max=NA, width = "100px", value = distance_effet_actif() )
                                   )
                          ),

                          #sigma
                          fluidRow(class="form-group ligne_contenu_modal",
                                   column(width = 4,  tags$label("Rayon de floutage")),
                                   column(width = 8, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                          ),

                          #Alpha
                          fluidRow(class="form-group ligne_contenu_modal",
                                   column(width = 4,  tags$label("Opacité  ")),
                                   column(width = 8, sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )

                          ),#,


                          #Mode de fusion
                          fluidRow(class="form-group ligne_contenu_modal",
                                   column(width = 4, tags$label("Mode de fusion") ),
                                   column(width = 8, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()   )
                                   )
                          )
                      )

                    },
                    "innner_glow"={

                      tagList(

                                 #Rayon
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4, tags$label("Rayon")),
                                     column(width = 8, numericInput(ns("param_effet_rayon"), label = NULL, min = 0, max=NA, width = "100px", value = rayon_effet_actif() )
                                     )
                                 ),

                                 #sigma
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4,  tags$label("Rayon de floutage")),
                                     column(width = 8, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                                 ),

                                 #Alpha
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4,  tags$label("Opacité  ")),
                                     column(width = 8, sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )

                                 ),#,


                                 #Couleur d
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 3, tags$label("Couleur")),
                                     column(width = 9, colourInput(ns("param_effet_couleur"), label = NULL, value=couleur_effet_actif())
                                     )
                                 ),

                                 #Mode de fusion
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 3, tags$label("Mode de fusion") ),
                                     column(width = 9, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()   )
                                     )
                                 )

                      )

                    },
                    "outer_glow"={

                      tagList(

                                 #Rayon
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4, tags$label("Rayon")),
                                     column(width = 8, numericInput(ns("param_effet_rayon"), label = NULL, min = 0, max=NA, width = "100px", value = rayon_effet_actif() )
                                     )
                                 ),

                                 #sigma
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4,  tags$label("Rayon de floutage")),
                                     column(width = 8, numericInput(ns("param_effet_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_effet_actif() ) )
                                 ),

                                 #Alpha
                                 fluidRow( class="form-group ligne_contenu_modal",
                                     column(width = 4,  tags$label("Opacité  ")),
                                     column(width = 8, sliderInput(ns("param_effet_alpha"), label = NULL, min = 0, max = 1, value=alpha_effet_actif(), sep = 0.1)  )

                                 ),#,


                                 #Couleur d
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4, tags$label("Couleur")),
                                     column(width = 8, colourInput(ns("param_effet_couleur"), label = NULL, value=couleur_effet_actif())
                                     )
                                 ),

                                 #Mode de fusion
                                 fluidRow(class="form-group ligne_contenu_modal",
                                     column(width = 4, tags$label("Mode de fusion") ),
                                     column(width = 8, selectInput(ns("param_effet_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_effet_actif()   )
                                     )
                                 )

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
            p("pas de données selectionnées")
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
              fluidRow(
                p("Veuiller choisir un effet.")
              )

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



        #Suiv des changements sur les options globales d'effets
        observeEvent(options_effets_symbologie_actif(), {
          req(couche_symbologie_actif())

          copie_symbologie <- options_symbologies_couche_actif()

          copie_symbologie[[couche_symbologie_actif()]]$effects <- options_effets_symbologie_actif()

          options_symbologies_couche_actif(copie_symbologie)

        })


        observeEvent(effet_actif(), {

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



        ## Ges du bouton d'aplication des options de la symbollogie et valisation ######
        observeEvent(input$bouton_appliquer_symbologie_couche, {
          copie_couche <- liste_couches()
          #le nom de la couche en cours

          copie_couche[[mame_couche_actif()]]$type_symbologie <- input$selec_type_symbole
          #eval(parse(text = paste0( paste("copie_couche", name_couche ,"type_symbologie", sep="$"), "<-'", input$selec_type_symbole, "'" ) ))

          if(!is.null(input$selec_type_symbole)){
            print("DEDANS")

            if(input$selec_type_symbole=="unique"){#Cas des symbologies uniques
              ##on récupère d'abord les noms des clés
              copie_couche[[mame_couche_actif()]]$options_symbologie_couche$options_symbologie_unique <- options_symbologies_couche_actif()

              #on applique les modifications à la liste principale des couches
              liste_couches(copie_couche)

              print(liste_couches())

            }
          }



          #on réinitialise certains paramètres



          #cas des valeurs
          #if(type_symbologie_actif()=="unique"){#cas du symbole unique
          #couleur_fill <- eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique","couleur_symbole",  sep="$") ))
          #couleur_remplissage_symbole_actif(couleur_fill)


        })



        ## Gestion du bouton ok pour la fenetre de gestion de la symbologie ######
        observeEvent(input$bouton_ok_symbologie_couche, {
          #ferméture de la fenêtre modale
          removeModal()

        })




        #Bou\



        #observeEvent(input$infos_color_symboble_unique, {
          #couleur_remplissage_symbole_actif(input$infos_color_symboble_unique$color_symboble_unique_js)
          #updateTextInput(session, "select_couleur_symbole", value = input$infos_color_symboble_unique$color_symboble_unique_js )
        #})



}#fin serveur
