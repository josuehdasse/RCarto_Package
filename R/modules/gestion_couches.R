#Module de  egstion des couches
library(shiny)


#modifier couche symbologie (elaborer la couche modifiee pour le remplacer dans la liste)
modifier_couche_symbologie <- function(type_symbologie, couche_modifie_symbologie,  couche_symbologie_active, level_gestion_categorie_symbologie, categorie_actif=NULL, intervalle_grade_actif=NULL){
  if(type_symbologie=="unique") {
            nouvelle_couche = couche_modifie_symbologie
    }else if(type_symbologie %in% c("categorise", "graduate")){

            nouvelle_couche = couche_symbologie_active

            if(level_gestion_categorie_symbologie=="symbole"){

              #ap
              nouvelle_couche$symbole <- couche_modifie_symbologie

            }else if(level_gestion_categorie_symbologie=="categories"){


              switch (type_symbologie,
                  "categorise" = {
                    req(categorie_actif)
                    #prendre en compte le nom de la nouvelle categorie
                    nouvelle_couche$categories[[categorie_actif]]$couches_symbologies <- couche_modifie_symbologie
                  },
                  "graduate"={
                    req(intervalle_grade_actif)
                    #prendre en compte le nom de la nouvelle categorie
                    nouvelle_couche$categories[[intervalle_grade_actif]]$couches_symbologies <- couche_modifie_symbologie
                  }
              )



            }




          }


  return(nouvelle_couche)

}



#copier une liste de couches de smbologie en fnction des type de symbologie

copier_liste_couches_symbologies <- function(type_symbologie, couche_symbologie_active, level_gestion_categorie_symbologie, categorie_actif=NULL, intervalle_grade_actif=NULL){



     if(type_symbologie=="unique"){
           copie_symbologie = couche_symbologie_active
      }else{
           req(level_gestion_categorie_symbologie)

           #On gère les deux niveaux de gestion de la symbologie pour le cas gradué
           if(level_gestion_categorie_symbologie=="symbole"){
             copie_symbologie = couche_symbologie_active$symbole


           }else if(level_gestion_categorie_symbologie=="categories"){#gestion au niveau des categories


             #il ya aussi la couche de categorie active
              switch (type_symbologie,
                "categorise" = {
                  req(categorie_actif)
                  copie_symbologie = couche_symbologie_active$categories[[categorie_actif]]$couches_symbologies
                },
                "graduate"={
                  req(intervalle_grade_actif)
                  copie_symbologie = couche_symbologie_active$categories[[intervalle_grade_actif]]$couches_symbologies
                }
              )


              #prints(copie_symbologie)

           }


         }







  return(copie_symbologie)


}



#contruire les div pour ensembles de symbologies
div_ensemble_symbologies <- function(liste_symbologie, geometrie, type_largeur_polygone){

  sortie <- lapply(liste_symbologie, function(j) {

    #Conversion des styles de traits entre ceux du ggplot2 et ceux du CSS

    switch (j$style_trait,
            "solid" = {
              style_trait="solid"

            },
            "blank" = {
              style_trait="none"

            },
            "longdash"={
              style_trait="dashed"

            },
            "dotted"={
              style_trait="dotted"

            },
            "dotdash"  ={
              style_trait="dashed"
            },
            "twodash"  ={
              style_trait="dashed"
            },
            "dashed" ={
              style_trait="dashed"
            }
    )

    #Gestion du style selon les différents types de géométrie de la couche vecteur mère
    switch (geometrie ,
            "POLYGON" = {
              if(type_largeur_polygone=="large"){
                chaine_style_div=paste0("position:absolute;top:",5-(j$epaisseur_trait/0.75+1)/2,"px;left: ",5-(j$epaisseur_trait/0.75+1)/2," px; height: ",20+(j$epaisseur_trait/0.75+1),"px ; width: 80%; z-index:",j$position,"; border: ",j$epaisseur_trait/0.75+1, "px ",  paste0(style_trait), " ", j$couleur_trait, "; background-color: ", j$couleur_symbole,  ";")
              }else{
                chaine_style_div=paste0("position:absolute;top:",5-(j$epaisseur_trait/0.75+1)/2,"px;left: ",5-(j$epaisseur_trait/0.75+1)/2," px; height: ",20+(j$epaisseur_trait/0.75+1),"px ; width: ",50+(j$epaisseur_trait/0.75+1),"px; z-index:",j$position,"; border: ",j$epaisseur_trait/0.75+1, "px ",  paste0(style_trait), " ", j$couleur_trait, "; background-color: ", j$couleur_symbole,  ";")
              }

            },
            "LINESTRING"={
              chaine_style_div=paste0("position:absolute;top: ",15-(j$epaisseur_trait/0.75+1)/2, "px ; height:1px ; width:100%; z-index:",j$position,"; border: ",j$epaisseur_trait/0.75+1, "px ",  paste0(style_trait), " ", j$couleur_trait, "; background-color: ", j$couleur_symbole,  ";")
            }
    )

    tags$div(
      class="symbole_couche",
      style=chaine_style_div
    )

  })


  return(sortie)
}


#fonctions utiles au module
ajouter_nouvelle_couche_symbologie <- function(geometrie_couche, ordre){

  #on initialise une nouvelle couche de symbologie
  couche_symbologie=options_defaut_couche_symbologie


  #le name de la couche
  couche_symbologie$name <- paste0("symbologie_", ordre)


  # Ajout des options par défaut des patterns de la couche symbologie unique par défaut
  couche_symbologie$patterns <- options_defaut_pattern_symbologie

  #on personnalise la configuration des couches de symbologie uniques
  couche_symbologie$position <- ordre#la position de la couche de symbologie unique

  #la couleur de remplissage du symbole (on nuance pour passer à la palettre suivantes)
  position_couleur <- brewer.pal.info[ordre,1]
  matrice_couleurs_prec <- brewer.pal.info %>% slice(1:ordre-1)
  precedants <- sum(matrice_couleurs_prec$maxcolors)
  couche_symbologie$couleur_symbole<- liste_couleurs[precedants + position_couleur]

  #on va choisir une couleur aléatoire qui exclue la palette courante (setdiff)
  couche_symbologie$couleur_trait<- "#000000" #les noiuvelles couches viennent avec une bordure par défaut en nour

  ## Exception du label selon le type de polygone
  switch (geometrie_couche,
          "POLYGON" = {
            #l'étiquette de la couche de symbologie
            couche_symbologie$label <- paste0("Remplissage simple ", ordre)
            #le type de remplissage de la symbologie
            couche_symbologie$style_fill_symbologie <-"continu"
          },
          "LINESTRING"={
            #l'étiquette de la couche de symbologie
            couche_symbologielabel <- paste0("Ligne simple ",ordre)
            #le type de remplissage de la symbologies
            couche_symbologie$style_fill_symbologie <-"ligne_simple"
          },
          "POINT"={

          }
  )#Fin switch

  #on retourne la couche finale obtenue
  return(couche_symbologie)

}





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

               tags$div(class="container-hierarchy",
                    uiOutput(ns("liste_couches_vecteurs"))

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


mod_gestion_couches_server<- function(input, output, session,id_projet_actif, liste_couches, resolution_page_actif){
  ns<- session$ns


            ##Tracking des éléments caractéristques de la symbologie d'une couche####
            name_couche_actif <- reactiveVal(#le nom de la couche active
              NULL
            )

            #le label de a couche active
            label_couche_actif <- reactiveVal(
                NULL
              )

            position_couche_actif <- reactiveVal(#le nom de la couche active
              NULL
            )



            type_geometrie_couche_actif <- reactiveVal(#le type de geometrie de la couche active
              NULL
            )



            type_symbologie_actif <- reactiveVal(
              NULL
            )


            colonne_valeur_symbologie_actif <- reactiveVal(#colonne qui contient les valeurs pour la gestion des cas graduées et categorisee
              NULL
            )


            #la palette des couleurs pour la symbologie categorisee
            palette_couleur_symbologie_actif <- reactiveVal(
              NULL
            )


            #reactif pour l'autorisation du paramétrage des symbologies
            autoriser_paramétrage_symbologie <- reactiveVal(
              FALSE
            )


            #le niveau de gestion du type categorise
            level_gestion_categorise_actif <- reactiveVal(
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



            #La table des données des couches actives
            DataCoucheActive <- reactive({
                #les données courantes
                data_couche= st_drop_geometry(liste_couches()[[name_couche_actif()]]$couche)

                #les joiuntures de la couche
                jointures_couche =liste_couches()[[name_couche_actif()]]$jointures

                if(length(jointures_couche)>0){#s'il ya des jointures
                  for (i in jointures_couche) {

                    table_intermediaire_jointure = get(i$name_table)
                    names(table_intermediaire_jointure) <- paste(i$name_table, snakecase::to_snake_case(names(table_intermediaire_jointure)), sep = "." )

                    data_couche <- merge(data_couche, table_intermediaire_jointure, by.x=i$colonne_couche_cible, by.y=paste(i$name_table, i$colonne_table,sep = "." )  )

                  }
                }



                data_couche


            })


      #Ajout d'une nouvelle couche###################
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

          withSpinner(
              uiOutput(ns("options_import_layer_ui")) )
          ,#on paramètre le contenu de la fenêtre cible ici
          #trigger = "ajouter_couche",
          size="m",
          easyClose = TRUE
        ))
      })


      ###Chargement du contenu de la fenetre modale d'importation des couches ########
        output$options_import_layer_ui <- renderUI({


          #print(liste_couches_final)

          #print("Je sui spassé")
          tagList(
            fluidRow(
              column(width = 5,
                     tags$label("Sélection de la couche ")
              ),
              column(width = 7,
                     selectInput(ns("select_couche"), label = NULL, choices = ListeSfEnv  )
              )


            ),

            fluidRow(
              uiOutput(ns("select_projection_ui"))
            )


          )#Fin taglist





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

                    column(width = 5,
                           tags$label("Projection de la couche ")
                           ),
                    column(width = 7,
                           selectInput(ns("select_projection"), label = NULL, choices = liste_crs, selected = 4326  )
                           )


                  )
                })

                #autoriser_importation_couche(TRUE)

              }else{#si la couche a une projection crs
                output$select_projection_ui <- renderUI({
                  withTags(

                    fluidRow(
                      column(width = 5,
                             tags$label("Projection de la couche ")
                      ),
                      column(width = 7,
                             selectInput(ns("select_projection"), label = NULL, choices = liste_crs, selected = st_crs(couche)$epsg   )
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


        ###Confirmer l'ajout d'une couche et egstion de son importation#########
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

          #on gère en fonction du type de la geométrie de la couche
          type_geometrie_couche <- unique(as.character(st_geometry_type(couche_import)))

          #initialisa tion des caractéristiques de la couche
          couche_courant= options_defaut_couche_vecteur

          #On personnalise les informations de la couche courante
          ## on commence par les options de la couche de vecteur
          couche_courant$couche <- st_simplify(couche_import)
          couche_courant$id_projet <- id_projet_actif()
          couche_courant$crs <- as.numeric(input$select_projection)

          couche_courant$name <- paste0(input$select_couche, nbre_couches_ajoutes+1)
          couche_courant$name_objet <- input$select_couche
          couche_courant$label_couche <- input$select_couche

          couche_courant$type_symbologie <- "unique"#Toutes les couches sont ajoutées avec par défaut un type de symbologie unique
          couche_courant$visible <- TRUE
          couche_courant$geometrie <- type_geometrie_couche
          couche_courant$position_couche <- nbre_couches_ajoutes+1

              #on ajoute les options de symbologie de la couche
              nouvelle_couche_symbologie = ajouter_nouvelle_couche_symbologie(type_geometrie_couche, 1)

              # On incréemente la couche de symbologie sur les autres
              liste_couches_symbologie = append(
                couche_courant$options_symbologie_couche$options_symbologie_unique, list(nouvelle_couche_symbologie)
              )
              #on confirme le nom
              names(liste_couches_symbologie)[1] <- "symbologie_1"


              #on definit dans l'obket courant la liste des symbologie actualisee
              couche_courant$options_symbologie_couche$options_symbologie_unique = liste_couches_symbologie

          #on crémente la couche
          mes_couches <- append(liste_couches(), list(couche_courant))
          names(mes_couches)[nbre_couches_ajoutes+1] <- paste0(input$select_couche, nbre_couches_ajoutes+1)#on combine le nom de l'objet à la position pour éviter les collisions en cas d'ajout multiple du meme objet

          #actualiser la valeur réactive
          liste_couches(mes_couches)

          showNotification(paste0("couche ", input$select_couche, " ajouté à la carte avec succès."), type = "message" )
          #fermeture du modal après traitement
          autoriser_importation_couche(FALSE)
          removeModal()

        })



        ## Liste reactive  des couches avec leurs options de controle  ############



        output$liste_couches_vecteurs <- renderUI({
          if(length(liste_couches() )>0){
            ListeCouchesUI()
          }else{
            p("Aucune couche vecteur ajoutée", style="color:red")
          }

        })


        ListeCouchesUI <- reactive({

          CouchesUI<- lapply(liste_couches() , function(i){ #liste des couche

              switch(i$type_symbologie,
                     "unique" = {
                       copie_symbologie = i$options_symbologie_couche$options_symbologie_unique
                     },
                     "categorise"= {
                       copie_symbologie = i$options_symbologie_couche$options_symbologie_categorise$categories
                     },
                     "graduate"={
                       copie_symbologie = i$options_symbologie_couche$options_symbologie_graduee$categories
                     }
              )#Fin switch


            #on elabore un div pour l'ensemble
            tags$div(class="lignes_couches", style="margin-left:5px;",

                  tagList(

                    #juste l'en-tete
                    tags$div(class="ligne_couche_header",
                             style="display:flex; flex-wrap : nowrap; gap :10px;",#stype pour permettre aux sous-éléments de se disposer sur la meme ligne

                        tagList(
                        #la case à cocher
                            tagList(#Debur case à cocher
                            if(i$visible){
                                tags$div(
                                  tags$input(
                                    type="checkbox",
                                    checked="checked",
                                    class="visibilite_couche",
                                    "data-categorie"=i$name,
                                    id=paste0("checked0_", i$name),
                                    "data-couche"=i$name,
                                    onclick="gestion_visibilite_couche_vecteur(this.dataset.categorie, this.checked)",
                                    style="height: 20px ; width:20px;"
                                  )
                                )

                              }else{
                                tags$div(
                                  tags$input(
                                    type="checkbox",
                                    class="visibilite_couche",
                                    "data-categorie"=i$name,
                                    id=paste0("checked0_", i$name),
                                    "data-couche"=i$name,
                                    onclick="gestion_visibilite_couche_vecteur(this.dataset.categorie, this.checked)",
                                    style="height: 20px ; width:20px;"
                                  )#fin input
                                )


                              }

                             ),#fin case à cocher

                            #Les couches de symbologie liées à la couhe vecteur
                            tagList(
                              tags$div(class="liste_symbologies_vecteur", style="position:relative;width:50px;",
                                       tagList(
                                          #on utiiise la fonction pour produrie les div des symbologies
                                         if (i$type_symbologie=="unique") {
                                             div_ensemble_symbologies(copie_symbologie, i$geometrie, "medium")
                                           }else if (i$type_symbologie %in% c("categorise", "graduate")){
                                             if(i$geometrie=="POLYGON"){
                                               tags$div(style="border:1px solid #000000;background:#DCDCDC; height:25px;top:5px")
                                             }else if(i$geometry=="LINESTRING"){
                                               tags$div(style="border:1px solid #000000;background:#DCDCDC; height:1px;")
                                             }

                                           }

                                       )

                              ),
                              tags$div( style="position:relative;width:100px;",
                                tags$p(i$label_couche)#On considère ici le label du couche qui peut varier
                              )
                            ),#Fin taglist de la symbologie



                                  tags$div(
                                    tags$div(class="btn-group",
                                             #liste des boutons
                                             tagList(

                                                 tags$button(type="button", class="btn btn-default",
                                                             "Options"
                                                 ),
                                                 tags$button(type="button", class="btn btn-default dropdown-toggle",
                                                             "data-toggle"="dropdown",
                                                             tagList(
                                                               tags$span(class="caret"),
                                                               tags$span(class="sr-only", "Toggle Dropdown")
                                                             )
                                                 ),


                                                 tags$ul(class="dropdown-menu",role="menu",

                                                         tagList(#liste des li du ul
                                                           tags$li(tags$a(href="#", "data-couche"=i$name, id=paste0("symbologie", i$name), onclick="symbologie_couche_vecteur(this.dataset.couche)"  , "Symbologie") ),
                                                           tags$li(tags$a(href="#", "Etiquettes","data-couche"=i$name,onclick="gestion_etiquettes_couche_vecteur(this.dataset.couche)")),
                                                           tags$li(tags$a(href="#","data-couche"=i$name, onclick="jointures_couche_vecteur(this.dataset.couche)"  , "Jointures")),
                                                           tags$li(tags$a(href="#", "Diagrammes"))

                                                         )#Fin taglist des li du ul
                                                 )


                                             )#Fin de la liste des boutons de controle



                                    )


                                  )#Fin taglist des sous options de gestion de la couche vecteur





                        )



                    ),#Fin en-tete
                    tags$div(class="sous_categories_layer", style="margin-left:15px",
                      tagList(
                        if(i$type_symbologie !="unique"){
                          #de but de la iste des sisu couches de symbologie
                          lapply(copie_symbologie , function(j){ #liste des couche
                            #on elabore un div pour l'ensemble
                            tags$div(class="lignes_couches",
                                     #juste l'en-tete
                                     tagList(
                                       tags$div(class="ligne_couche_header",
                                                style="display:flex; flex-wrap : nowrap; gap:10px; margin-left:15px;",#stype pour permettre aux sous-éléments de se disposer sur la meme ligne

                                                tagList(
                                                  #la case à cocher
                                                  tagList(#Debur case à cocher
                                                    if(j$visible){
                                                      tags$div(
                                                        tags$input(
                                                          type="checkbox",
                                                          checked="checked",
                                                          class="visibilite_categories",
                                                          id=paste0("checked0_", j$name),
                                                          "data-couche"=i$name,
                                                          "data-categorie"=j$name,
                                                          onclick="gestion_visibilite_categories_dListeCouche(this.dataset.couche, this.dataset.categorie, this.checked)",
                                                          style="height: 20px ; width:20px;"
                                                        )
                                                      )

                                                    }else{
                                                      tags$div(
                                                        tags$input(
                                                          type="checkbox",
                                                          class="visibilite_categories",
                                                          id=paste0("checked0_", j$name),
                                                          "data-couche"=i$name,
                                                          "data-categorie"=j$name,
                                                          onclick="gestion_visibilite_categories_dListeCouche(this.dataset.couche, this.dataset.categorie, this.checked)",                                                          style="height: 20px ; width:20px;"
                                                        )#fin input
                                                      )

                                                    }

                                                  ),#fin case à cocher

                                                  #Les couches de symbologie liées à la couhe vecteur
                                                  tagList(
                                                    tags$div( style="position:relative;width:50px;",
                                                             id=paste0("gestionnaire_symbologie_categorie", j$name),
                                                             tagList(
                                                               #on utiiise la fonction pour produrie les div des symbologies
                                                               div_ensemble_symbologies(j$couches_symbologies, i$geometrie, "medium")
                                                             )

                                                    ),
                                                    tags$div(style="position:relative; margin-left:5px;",#le label de la catégorie
                                                             tags$p(j$legende)
                                                    )
                                                  )#Fin taglist de la symbologie


                                                )

                                       )#Fin en-tete
                                     )
                            )
                          })
                        }




                      )#fin de la liste des div pour l'enumération des sous catégories de symbologies
                    )#Fin des catégories pour une couche
                  )

            )
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
          name_couche_actif(name_couche)


          #label des couches active
          label_couche <- liste_couches()[[name_couche]]$label_couche
          label_couche_actif(label_couche)


          #on reinitialise les informations sur le choix de la couche de symbologie
          couche_symbologie_actif(NULL)

          #duplication de la couche
          copie_couche <- liste_couches()

          #### resultat requete: On actualise les valeurs de la symbologie suivant la  couche sélectionnée #############################
          type_symbologie <- copie_couche[[name_couche]]$type_symbologie# eval(parse(text = paste("copie_couche", name_couche ,"type_symbologie", sep="$") ))
          type_symbologie_actif(type_symbologie)

          #on prend la position
          position_symbologie <- copie_couche[[name_couche]]$position# eval(parse(text = paste("copie_couche", name_couche ,"position", sep="$") ))
          position_couche_actif(position_symbologie)


          #on prend aussi la geometrie de la couche active
          #type_geometrie_couche_actif
          type_geometrie_couche <- copie_couche[[name_couche]]$geometrie# eval(parse(text = paste("copie_couche", name_couche ,"geometrie", sep="$") ))
          type_geometrie_couche_actif(type_geometrie_couche)

          #Elaboration de la liste des couches de symbologie
          switch (type_symbologie,
                  "unique" = {
                    options_symbologies_couche <- copie_couche[[name_couche]]$options_symbologie_couche$options_symbologie_unique#   eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique",  sep="$") ))
                    options_symbologies_couche_actif(options_symbologies_couche)
                  },
                  "categorise" ={
                    options_symbologies_couche <- copie_couche[[name_couche]]$options_symbologie_couche$options_symbologie_categorise
                    options_symbologies_couche_actif(options_symbologies_couche)

                    #la colonne de gestin de la categorie
                    colonne_valeur_symbologie <- options_symbologies_couche$colonne_valeur_symbologie
                    colonne_valeur_symbologie_actif(colonne_valeur_symbologie)

                    #la palette des coueleurs
                    palette_couleur_symbologie <- options_symbologies_couche$palette_couleurs
                    palette_couleur_symbologie_actif(palette_couleur_symbologie)


                  }, #fin gestion de la symbologie des cas categorisés
                  "graduate"={
                    options_symbologies_couche <- copie_couche[[name_couche]]$options_symbologie_couche$options_symbologie_graduee
                    options_symbologies_couche_actif(options_symbologies_couche)

                    #la colonne de gestin de la categorie
                    colonne_valeur_symbologie <- options_symbologies_couche$colonne_valeur_symbologie
                    colonne_valeur_symbologie_actif(colonne_valeur_symbologie)

                    #la palette des coueleurs
                    palette_couleur_symbologie <- options_symbologies_couche$palette_couleurs
                    palette_couleur_symbologie_actif(palette_couleur_symbologie)
                  }
          )







          ### On donne accès à la fenêtre modale permettant de gérer les options de la symbologie de la couche courante#####################
          showModal(modalDialog(
            title = paste0("Options de la symbologie de la couche ", label_couche),
            footer=tagList(
              div(class="div_footer_modal",
                  actionButton(ns("bouton_ok_symbologie_couche"), "Fermer", class="btn-success"),
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
                  uiOutput(ns("categories_symbologie"))#La liste ds catégories pour la gestion des cas catégorisée et graduees
                ),
                fluidRow(
                  uiOutput(ns("couches_symbologie"))#liste des couches de la symbologie (en reactif)
                ),
                tags$hr(width="80%"),
                fluidRow(
                  #UIParametresSymbologie()
                  uiOutput(ns("options_symbologie_ui"))#gestionnaire du contrôle  de l'apparence des symboles
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
                      selectInput(ns("select_type_symbole"), NULL, choices = list("symbole unique"="unique", "catégorisé"="categorise", "gradué"="graduate"), selected = type_symbologie_actif() )
                  )

            )
          )

        })



        #### Ce qui se passe lorsque l'on change le type de symbologie de la couche vecteur (passer de uique à categoriser, etc)#################################################################
        observeEvent(input$select_type_symbole, {

          #om reinitialise les options de controle concernant l'autorisation du paramérage de la symbologie ds couches
          autoriser_paramétrage_symbologie(FALSE)

          req(input$select_type_symbole)

          copie_couche=liste_couches()

          #on mute le type de symbologie
          type_symbologie_actif(input$select_type_symbole)


          if(input$select_type_symbole=="unique"){
                options_symbologies_couche <- copie_couche[[name_couche_actif()]]$options_symbologie_couche$options_symbologie_unique#   eval(parse(text = paste("copie_couche", name_couche,"options_symbologie_couche", "options_symbologie_unique",  sep="$") ))
                options_symbologies_couche_actif(options_symbologies_couche)

                #on autorise le paramétrage de la symbologie
                autoriser_paramétrage_symbologie(TRUE)

            }else if(input$select_type_symbole %in% c("categorise", "graduate")) {

              if(input$select_type_symbole=="categorise"){
                  options_symbologies_couche <- copie_couche[[name_couche_actif()]]$options_symbologie_couche$options_symbologie_categorise
              }else if(input$select_type_symbole=="graduate"){
                  options_symbologies_couche <- copie_couche[[name_couche_actif()]]$options_symbologie_couche$options_symbologie_graduee
              }

              #on initialise la couche 1 de symbologie pour les symboles si cela n'existe pas encore
              couches_symbologies_symble <- length(options_symbologies_couche$symbole)

              if(couches_symbologies_symble==0){
                nouvelle_couche_smbologie <- ajouter_nouvelle_couche_symbologie(type_geometrie_couche_actif(), 1)

                liste_couches_symboles = list(
                    symbologie_1=nouvelle_couche_smbologie
                )

                #on ajoute alors à la liste
                options_symbologies_couche$symbole <- liste_couches_symboles

              }

              options_symbologies_couche_actif(options_symbologies_couche)

              #on actualise la colonne des categories
              colonne_valeur_symbologie_actif(options_symbologies_couche$colonne_valeur_symbologie)

            } #fin gestion de la symbologie des cas categorisés

        })





        #### On affiche d'abord les couches de la symbologie  avec leurs oprions de controles et visualisations##########

        ###### Elaborer La liste des reactives des couches de symbologie ############
        couches_symbologieUI <- reactive({

          couches_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(), categorie_symbologie_actif(),intervalle_graduation_actif() )


          symbologieUI <- lapply(couches_symbologie , function(i){ #les effets qui sont ajoutés à la couche courante

            #Conversion des styles de traits entre ceux du ggplot2 et ceux du CSS

              switch (i$style_trait,
                      "solid" = {
                        style_trait="solid"

                      },
                      "blank" = {
                        style_trait="none"

                      },
                      "longdash"={
                        style_trait="dashed"

                      },
                      "dotted"={
                        style_trait="dotted"

                      },
                      "dotdash"  ={
                        style_trait="dashed"
                      },
                      "twodash"  ={
                        style_trait="dashed"
                      },
                      "dashed" ={
                        style_trait="dashed"
                      }
              )

              #Gestion du style selon les différents types de géométrie de la couche vecteur mère
              switch (type_geometrie_couche_actif() ,
                      "POLYGON" = {
                        chaine_style_div=paste0(" height: 20px ; width: 75px; border: ",i$epaisseur_trait/0.75+1, "px ",  paste0(style_trait), " ", i$couleur_trait, "; background-color: ", i$couleur_symbole,  ";")
                      },
                      "LINESTRING"={
                        chaine_style_div=paste0("position:absolute;top:15px ; height:1px ; width:75px; border: ",i$epaisseur_trait/0.75+1, "px ",  paste0(style_trait), " ", i$couleur_trait, "; background-color: ", i$couleur_symbole,  ";")
                      }
              )


              tags$li(
                class="list_item",  style="display:flex; flex-wrap : nowrap; gap :10px;",#les sous elements de tiennent sur la meme ligne
                tagList(
                  tags$div(class="symbol_container",class="symbole_couche",
                        style=chaine_style_div
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



        ###### Gestion du clic sur une couche de symbologie #####
        observeEvent(input$choix_couche_symbologie, {

          couche_symbologie_actif(NULL)

          req(input$choix_couche_symbologie)
          req(options_symbologies_couche_actif())

          #on actualise la valeur de la synbologie en cours tel que choisi par l'utlisateu
            data_couche_select<- fromJSON(input$choix_couche_symbologie)
            name_couche_symbologie <-data_couche_select$name
            couche_symbologie_actif(name_couche_symbologie)


            #la geometrie
            #type_geometrie_couche <- data_couche_select$geometrie
            #type_geometrie_couche_actif(type_geometrie_couche)#on actualise

            #print(couche_symbologie_actif())


            #peut déjà ici propager les réactives à utiliser dans l'application
            #copie d ela couche des valeurs pour la symbologie


            #on élabore la couhe desymbologie en fonction du type de symbologie choisi

            copie_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(), categorie_symbologie_actif(),intervalle_graduation_actif() )


            #on fait une copie de la couche des otpions de la symbologie
            #options_symbologies_couche =options_symbologies_couche_actif()

            #on echerche les valeurs de la couche de symbologie et on les affiche
            #if(type_symbologie_actif()=="unique"){#cas du symbole unique
            couleur_fill <- copie_symbologie[[name_couche_symbologie]]$couleur_symbole#       eval(parse(text = paste("copie_symbologie",name_couche_symbologie,   "couleur_symbole",  sep="$") ))
            #print(paste("copie_symbologie",name_couche_symbologie,   "couleur_symbole",  sep="$"))
             couleur_remplissage_symbole_actif(couleur_fill)


            style_trait <-copie_symbologie[[name_couche_symbologie]]$style_trait#       eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"style_trait",  sep="$") ))
            style_trait_actif(style_trait)

            style_fill_symbologie <-copie_symbologie[[name_couche_symbologie]]$style_fill_symbologie#   eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"style_fill_symbologie",  sep="$") ))
            style_fill_symbologie_actif(style_fill_symbologie)

            couleur_trait <- copie_symbologie[[name_couche_symbologie]]$couleur_trait#    eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"couleur_trait",  sep="$") ))
            couleur_trait_actif(couleur_trait)

            epaisseur_trait <- copie_symbologie[[name_couche_symbologie]]$epaisseur_trait#    eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"epaisseur_trait",  sep="$") ))
            epaisseur_trait_actif(epaisseur_trait)

            #les informations sur les effets
            statut_effet <- copie_symbologie[[name_couche_symbologie]]$statut_effet#    eval(parse(text = paste("copie_symbologie",name_couche_symbologie,"statut_effet",  sep="$") ))
            statut_effet_actif(statut_effet)



        })


        ###### Elaboration du graphique de La prévisualisation des choix de la symbologie #########
        previsulationSymbologieUI <- reactive({
            req(input$select_type_symbole)

            #On elabore la couche de symbologie en fonction du type de symbologie choisi

          options_symbologie= copier_liste_couches_symbologies(input$select_type_symbole, options_symbologies_couche_actif(), level_gestion_categorise_actif(), categorie_symbologie_actif(),intervalle_graduation_actif()  )


          #print("previsuaisation")
          #print(input$select_type_symbole)
          #print(categorie_symbologie_actif())
          #print(options_symbologie)

          req(options_symbologie)


            #Le jeu de données de visualisation pour les polygones
            switch (type_geometrie_couche_actif() ,
                    "POLYGON" = {
                      data_graph <- rbind( c(0.0), c(1,0), c(1,1), c(0,1), c(0,0) )#les données du graphique  ==>> la couche
                      polygone <- st_polygon(list(data_graph))

                      sfc_obj <- st_sfc(list(polygone))

                    },
                    "LINESTRING"={
                      data_graph <- rbind( c(0,0.5), c(1,0.5) )#les données du graphique  ==>> la couche
                      polygone <- st_linestring(data_graph)

                      sfc_obj <- st_sfc(list(polygone))

                    },
                    "POINT"={

                    }
            )

          data_points <- data.frame(names=c("A"))
          data_points$geometry <- sfc_obj
          data_points <- st_as_sf(data_points)

          #initialisation du graphique
          #graphique <- ggplot()

          print( sprintf("type_symbologie_actif: %s, type_geometrie_couche_actif: %s, position_couche_actif: %f  ", type_symbologie_actif(), type_geometrie_couche_actif(),position_couche_actif()       ) )

          code_symbologies_couches_graph <- generer_code_type_symbologie_unique("data_points", type_geometrie_couche_actif(), options_symbologie, position_couche_actif() )

          print(paste0("Code : ", code_symbologies_couches_graph ))

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



        afficher_sous_options_categories <- reactiveVal("Non")

        #Le recatif du contenu des categories de gestion de la symbologie
        categoriesUI <- reactive({

          #En focntion du paramètre d'autorisation des
          switch (afficher_sous_options_categories(),
              "Non" = {
                  #On recupère les données de la couche de symbologie
                  copie_couche <- liste_couches()
                  data_couche_actif <- copie_couche[[name_couche_actif()]]$couche

                  #On importe les options de symbologie
                  copie_options_symbologie <- options_symbologies_couche_actif()

                  #on actualise/recupère les valeurs par défaut
                  colonne_valeur_symbologie_actif(copie_options_symbologie$colonne_valeur_symbologie)
                  palette_couleur_symbologie_actif(copie_options_symbologie$palette_couleurs)

                  #print(options_symbologies_couche_actif())
                  #les graduées nécessitent uniquement les champs numériques
                  if(type_symbologie_actif()=="graduate"){
                    couche_data <- DataCoucheActive() %>% select(where(is.numeric))#On ne selectionne ici que les champs numéeriques de la table
                  }else{
                    couche_data <- DataCoucheActive()
                  }



                  tagList(
                    fluidRow(class="ligne_contenu_modal options_param_categories",
                             tagList(
                               fluidRow(class="form-group ligne_contenu_modal",#le choix de la valeur pour les noms des colonnes à utiliser lors de la categorisation de la symbologie
                                        column(width = 4,tags$label("Valeur") ),
                                        column(width = 8,
                                               selectInput(ns("select_colonne_valeur_symbologie"), label = NULL, choices = c("",names(couche_data)),  selected = colonne_valeur_symbologie_actif() )
                                        )
                               ),
                               fluidRow(class="form-group ligne_contenu_modal",
                                        column(width = 4,tags$label("Symbole") ),
                                        column(width = 8, #définir le symbole à utiliser pour toutes catégories
                                               tags$div(class="liste_symbologies_symbole_categorie", style="position:relative;top:0",
                                                        tagList(
                                                          div_ensemble_symbologies(copie_options_symbologie$symbole, type_geometrie_couche_actif(), "large")
                                                        ),
                                                        onclick="parametrer_symbole_categorise()"
                                               )



                                        )#colonne des 8

                               ),
                               fluidRow(class="form-group ligne_contenu_modal",#le choix de la valeur pour les noms des colonnes à utiliser lors de la categorisation de la symbologie
                                        column(width = 4,tags$label("Palette") ),
                                        column(width = 8,
                                               selectInput(ns("select_palette_categories"), label = NULL, choices = c("Aleatoire", rownames(brewer.pal.info)),  selected =palette_couleur_symbologie_actif() )
                                        )

                               )
                             )


                    ),
                    fluidRow(class="ligne_contenu_modal  list-items-categories",
                        tags$div(
                          class="liste_div_categories", style="overflow:auto;max-height:600px",

                          listeCategoriesUI()
                        )
                    ),
                    fluidRow(class="ligne_contenu_modal",
                      tagList(
                        uiOutput(ns("classer_categories_ui")),
                        uiOutput(ns("tout_supprimer_categories_ui")),
                        uiOutput(ns("ajouter_categorie_ui"))
                      )
                    ),
                    fluidRow(class="ligne_contenu_modal",
                       uiOutput(ns("gestionnaire_ajout_intervalle_graduee"))
                    )
                  )
            },
            "Oui"={

              fluidRow(class="ligne_contenu_modal", style="margin-bottom:20px;",
                       #on affiche juste un bouton de retour en arrière qui va permettre la réapparition des options de la categorie

                  actionButton(ns("retour_options_categories"), "Retour aux catégories", class="btn-success")
                  #actionButton(ns("retour_options_categries"), "", icon = icon("glyphicon glyphicon-picture", lib = "glyphicon"), class="btn-primary btn-sm")
              )

            }
          )

        })


        ####Bouton de classement des catégories#################
        output$classer_categories_ui <- renderUI({
          req(type_symbologie_actif()=="categorise")
           req(input$select_colonne_valeur_symbologie)

           actionButton(ns("classer_categories"), "Classer")
        })


        ####Bouton de suppression des categories###################
        output$tout_supprimer_categories_ui <- renderUI({
            req(length(options_symbologies_couche_actif()$categories)>0)
          req(EditionIntervalleGrade()==FALSE )

            actionButton(ns("tout_supprimer_categories"), "Tout Supprimer", icon = icon("trash"))

        })

        output$ajouter_categorie_ui <- renderUI({
          req(type_symbologie_actif()=="graduate")
          req(input$select_colonne_valeur_symbologie)
          req(EditionIntervalleGrade()==FALSE )


          actionButton(ns("ajouter_intervalle"), "Nouvelle intervalle", icon = icon("add") )

        })



        ## Gestion pour le type de symbologie categorise ######################################



        ###On appuie sur le bouton "classer" (on gènrer la listed es couches categorises) ##################
        observeEvent(input$classer_categories, {

          req(input$select_colonne_valeur_symbologie)
          req(input$select_colonne_valeur_symbologie !="")

          couches_symboles = copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), "symbole")


          modalites_colonne <- unique(DataCoucheActive()[[input$select_colonne_valeur_symbologie]])

          #Gestion des couleurs des palettes
          if(input$select_palette_categories=="Aleatoire"){
              couleurs_palette = sample(liste_couleurs, length(modalites_colonne))#on prelève n couleurs
          }else{#le reste des palettes des couleurs
            contenu_palette=brewer.pal( length(modalites_colonne), input$select_palette_categories)
            ecart= length(modalites_colonne)- length(contenu_palette)

            if(ecart>0){
              couleurs_ecart=rep("#FFFFFF", ecart)
              couleurs_palette=c(couleurs_ecart, contenu_palette)
            }else if(ecart==0){#nombre egal de couleurs que la palette
              couleurs_palette=contenu_palette
            }else if(ecart<0){#Si on a moins de modalités que
              couleurs_palette=contenu_palette[-ecart:length(contenu_palette)]
            }

          }


          #on initialise la liste des catégories à une liste vide puisqu'on n'emppile
          copie_couches_categorie= list()

          for (i in 1:length(modalites_colonne) ) {
              #on applique d'abord la couleur de la palette
              for (j in 1:length(couches_symboles)) {
                couches_symboles[[paste0("symbologie_", j)]]$couleur_symbole <- couleurs_palette[i]
              }

              #on cree une instance de categorie
              nouvelle_categorie=options_defaut_categories_symbologies
                  #on definit le name
                  nouvelle_categorie$name<- paste0("categorie_", i)

                  #legende
                  nouvelle_categorie$legende<- modalites_colonne[i]

                  #On definit le label
                  nouvelle_categorie$valeur<-modalites_colonne[i]
                  #On leur affecte automatiquement les couches de symbologie disponibles au niveau de symboles
                  nouvelle_categorie$couches_symbologies <- couches_symboles

              #on l'ajoute sur la liste des couches des categories
              copie_couches_categorie = append(copie_couches_categorie, list(nouvelle_categorie))
              #on nomme la couche en cours
              names(copie_couches_categorie)[i] <- paste0("categorie_", i)
          }

            #on va maintenant faire les modification sur la couche principale de symbologie
            couches_symbologies = options_symbologies_couche_actif()
            couches_symbologies$categories <- copie_couches_categorie

            #on inpute aussi le nom dela colonne
            couches_symbologies$colonne_valeur_symbologie <- input$select_colonne_valeur_symbologie
            colonne_valeur_symbologie_actif(input$select_colonne_valeur_symbologie)

            #on impute egalement la plette de couleur
            couches_symbologies$palette_couleurs <- input$select_palette_categories
            palette_couleur_symbologie_actif(input$select_palette_categories)

            #Application de la modificatioon
            options_symbologies_couche_actif(couches_symbologies)
            #print(options_symbologies_couche_actif)

        })


        ###On appuie sur le bouton de retour aux categories#######################
        observeEvent(input$retour_options_categories, {
            req(input$retour_options_categories)
            afficher_sous_options_categories("Non")#pour afficher ou cacher la liste des catégories de valeurs
        })

        ######Affichage des les liste des Categories que le type de symbologie de controle utilisée est categorisee"########################################
        output$categories_symbologie <- renderUI({
          req(input$select_type_symbole !="unique")

          categoriesUI()

        })

        ##### Liste réactive des catégroies  de symboles
        listeCategoriesUI <- reactive({

            #on gère en fonction du type de symbologie
            if(type_symbologie_actif()=="categorise"){
              #print(options_symbologies_couche_actif()$categories)
              tagList(
                #on constuit le header ici
                tags$div(class="ligne_couche_header",
                         style="display:flex; flex-wrap : nowrap; gap:10px; background:#DCDCDC; width:100%;top:0;",
                         tagList(
                           tags$div(style="width:150px","Symbole"),
                           tags$div(style="width:305px","Valeur"),
                           tags$div(style="width:300px","Légende")
                         )
                ),
                lapply(options_symbologies_couche_actif()$categories , function(i){ #liste des couche
                  #on elabore un div pour l'ensemble
                  tags$div(class="lignes_couches",
                           #juste l'en-tete
                           tagList(
                             tags$div(class="ligne_couche_header",
                                      style="display:flex; flex-wrap : nowrap; gap:10px; margin-left:15px;",#stype pour permettre aux sous-éléments de se disposer sur la meme ligne

                                      tagList(
                                        #la case à cocher
                                        tagList(#Debur case à cocher
                                          if(i$visible){
                                            tags$div(
                                              tags$input(
                                                type="checkbox",
                                                checked="checked",
                                                class="visibilite_categories",
                                                id=paste0("checked0_", i$name),
                                                "data-couche"=i$name,
                                                onclick="gestion_visibilite_categories(this.dataset.couche, this.checked)",
                                                style="height: 20px ; width:20px;"
                                              )
                                            )

                                          }else{
                                            tags$div(
                                              tags$input(
                                                type="checkbox",
                                                class="visibilite_categories",
                                                id=paste0("checked0_", i$name),
                                                "data-couche"=i$name,
                                                onclick="gestion_visibilite_categories(this.dataset.couche, this.checked)",
                                                style="height: 20px ; width:20px;"
                                              )#fin input
                                            )

                                          }

                                        ),#fin case à cocher

                                        #Les couches de symbologie liées à la couhe vecteur
                                        tagList(
                                          tags$div(class="liste_symbologies_vecteur", style="position:relative;width:100px;",
                                                   id=paste0("gestionnaire_symbologie_categorie", i$name),
                                                   "data-categorie"=i$name,
                                                   tagList(
                                                     #on utiiise la fonction pour produrie les div des symbologies
                                                     div_ensemble_symbologies(i$couches_symbologies, type_geometrie_couche_actif(), "medium")
                                                   ),
                                                   onclick="gestionnaire_parametres_symbologies_categorie(this.dataset.categorie)"

                                          ),
                                          tags$div( style="position:relative;width:300px; margin-left:10px;",
                                                    tags$p(i$valeur)
                                          ),
                                          tags$div( style="position:relative;width:300px; margin-left:10px;",
                                                    tags$input( style="border:none;",
                                                                type="text",
                                                                value=i$legende,
                                                                id=paste0("legende_categorie", i$name)
                                                    )
                                          )
                                        )#Fin taglist de la symbologie


                                      )

                             )#Fin en-tete
                           )
                  )
                })
              )

            }else if(type_symbologie_actif()=="graduate"){#Liste reactif des cas graduées

              tagList(
                #on constuit le header ici
                tags$div(class="ligne_couche_header",
                         style="display:flex; flex-wrap : nowrap; gap:10px; background:#DCDCDC; width:100%;top:0;",
                         tagList(
                           tags$div(style="width:115px;margin-left:15px;","Symbole"),
                           tags$div(style="width:300px;margin-left:15px;","Minimum"),
                           tags$div(style="width:300px;margin-left:15px;","Maximum"),
                           tags$div(style="width:300px;margin-left:15px;","Légende")
                         )
                ),

                lapply(options_symbologies_couche_actif()$categories , function(i){ #liste des couche
                  #on elabore un div pour l'ensemble
                  tags$div(class="lignes_couches",
                           #juste l'en-tete
                           tagList(
                             tags$div(class="ligne_couche_header",
                                      style="display:flex; flex-wrap : nowrap; gap:10px; margin-left:15px;",#stype pour permettre aux sous-éléments de se disposer sur la meme ligne

                                      tagList(
                                        #la case à cocher
                                        tagList(#Debur case à cocher
                                          if(i$visible){
                                            tags$div(
                                              tags$input(
                                                type="checkbox",
                                                checked="checked",
                                                class="visibilite_categories",
                                                id=paste0("checked0_", i$name),
                                                "data-couche"=i$name,
                                                onclick="gestion_visibilite_categories(this.dataset.couche, this.checked)",
                                                style="height: 20px ; width:20px;"
                                              )
                                            )

                                          }else{
                                            tags$div(
                                              tags$input(
                                                type="checkbox",
                                                class="visibilite_categories",
                                                id=paste0("checked0_", i$name),
                                                "data-couche"=i$name,
                                                onclick="gestion_visibilite_categories(this.dataset.couche, this.checked)",
                                                style="height: 20px ; width:20px;"
                                              )#fin input
                                            )

                                          }

                                        ),#fin case à cocher

                                        #Les couches de symbologie liées à la couhe vecteur
                                        tagList(
                                          tags$div(class="liste_symbologies_vecteur", style="position:relative;width:100px;",
                                                   id=paste0("gestionnaire_symbologie_categorie", i$name),
                                                   "data-categorie"=i$name,
                                                   tagList(
                                                     #on utiiise la fonction pour produrie les div des symbologies
                                                     div_ensemble_symbologies(i$couches_symbologies, type_geometrie_couche_actif(), "medium")
                                                   ),
                                                   onclick="gestionnaire_parametres_symbologies_categorie(this.dataset.categorie)"

                                          ),
                                          tags$div( style="position:relative;width:300px; margin-left:10px;",
                                                    tags$p(i$minimum_intervalle)
                                          ),
                                          tags$div( style="position:relative;width:300px; margin-left:10px;",
                                                    tags$p(i$maximum_intervalle)
                                          ),
                                          tags$div( style="position:relative;width:300px; margin-left:10px;",
                                                    tags$p(i$legende)
                                          ),
                                          tags$div( style="position:relative; margin-left:10px;",
                                                    tags$button(id=paste0("btn_modif", i$name), tags$i(class="fa fa-edit"),
                                                    "data-intervalle"=i$name,
                                                    onclick="gestionnaire_intervalles_symbologie_graduee(this.dataset.intervalle)",
                                                                )
                                          )
                                        )#Fin taglist de la symbologie


                                      )

                             )#Fin en-tete
                           )
                  )
                })
              )



            }#Fin de la liste pour le type graduate




        })


        ##### Gestion des couches de catégorie de symbologie ############################
        ###### Gestion des visibilité des couches de catégorie de symbologie ####################################
        categorie_symbologie_actif <- reactiveVal(
          NULL
        )

        observeEvent(input$select_activation_couche_categorie, {
          req(input$select_activation_couche_categorie)

          data_activation<- fromJSON(input$select_activation_couche_categorie)

          name_couche <-data_activation$name
          valeur_activation <- data_activation$activation

          #on actuailse les donnes
          #categorie_symbologie_actif(name_couche)#On commence par le nom de la couche de catégorie active

            copie_couches_symbologie=options_symbologies_couche_actif()

            copie_couches_symbologie$categories[[name_couche]]$visible <- valeur_activation

            options_symbologies_couche_actif(copie_couches_symbologie)
        })


        ####### Visibilité des couches de catégories de smbologie depuis la liste des couches#################
        observeEvent(input$select_activation_Categorie_dListeCouche,{
          req(input$select_activation_Categorie_dListeCouche)

          data_activation<- fromJSON(input$select_activation_Categorie_dListeCouche)

          nom_couche=data_activation$couche
          categorie_courant=data_activation$categorie
          statut_activation_categorie=data_activation$activation

          #copie des couches
          copie_couches=liste_couches()


          #print(copie_couches[[nom_couche]]$options_symbologie_couche)


          copie_couches[[nom_couche]]$options_symbologie_couche$options_symbologie_categorise$categories[[categorie_courant]]$visible <- statut_activation_categorie

          #on applique sur la couche les modifications
          liste_couches(copie_couches)

        })


        ######Gestion du clic sur le symbole de couche de la catégorie de symbologie ###################
        observeEvent(input$parametres_symbologie_categorie,{
            req(input$parametres_symbologie_categorie)

            data_categorie=fromJSON(input$parametres_symbologie_categorie)


            switch (type_symbologie_actif(),
               "categorise" = {
                 categorie_symbologie_actif(data_categorie$name)
               },
               "graduate"={
                 intervalle_graduation_actif(data_categorie$name)
               }
            )

            level_gestion_categorise_actif(data_categorie$level)

            #on doit aussi actualilser la couche de symbologie courante
           # options_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), data_categorie$level, data_categorie$name )

            #on autorise la modification s
            autoriser_paramétrage_symbologie(TRUE)
            afficher_sous_options_categories("Oui")#pour afficher ou cacher la liste des catégories de valeurs


        })




        ###### Lorsqu'on clique Symbole pour la symbologie de type "categorise" #####################################################
        observeEvent(input$gestion_parametrage_symbole_categorie, {
          req(input$gestion_parametrage_symbole_categorie)

          level_categorisee<- fromJSON(input$gestion_parametrage_symbole_categorie)
          level_categorisee_select <-level_categorisee$level

          #print(level_categorisee_select)

          #on actualise le level de egstion pour le categorise
          level_gestion_categorise_actif(level_categorisee_select)

          #on autorise la modification s
          autoriser_paramétrage_symbologie(TRUE)
          afficher_sous_options_categories("Oui")#pour afficher ou cacher la liste des catégories de valeurs

        })



        ##Affichage des Options de contrôle sur les symbologies de la couche (prévisualisation, liste et controles) #####
        output$couches_symbologie <- renderUI({

          req(input$select_type_symbole)

          if(input$select_type_symbole=="unique") {
               req(input$select_type_symbole)
               req(autoriser_paramétrage_symbologie()==TRUE)
           }else if(input$select_type_symbole %in% c("categorise", "graduate")) {
                req(level_gestion_categorise_actif())
                req(autoriser_paramétrage_symbologie()==TRUE)
                req(afficher_sous_options_categories()=="Oui")


          }#Fin else if pour la gestion des categorise et les graduate



              tagList(
                fluidRow(class="ligne_contenu_modal",
                  column(width = 3, style="height:170px;",  imageOutput(ns("previsualisation_symbologie_ui"))),#place pour la visualisation du rendu final de la symbologie sur toutes les couches
                  column(width = 8, class="list-items-couches",
                         tagList(
                           tags$p("Remplissage"),
                           tags$ul(class="liste_div_symbologies", style="overflow: auto;max-height: 150px;",
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



        ##on renvoie les caractéristiques de la gestion des options de contrôle  sur la symbologie ####


        #####On  rend à l'utilisateur les options de  modification de la symbologie en cours#######
        output$options_symbologie_ui <- renderUI({


          if(type_symbologie_actif() %in% c("categorise","graduate")){
            req(autoriser_paramétrage_symbologie()==TRUE)
            req(afficher_sous_options_categories()=="Oui")
          }


          req(input$choix_couche_symbologie)#Eviter les valeurs nulles du clic de l'utilisateur
          req(input$select_type_symbole)
          req(couche_symbologie_actif())


          #if( input$select_type_symbole=="unique"){##

            #Gestion selon le type de géométrie de la couche
            switch (type_geometrie_couche_actif(),
                "POLYGON" = {

                  isolate({#debut isolate

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
                                      tags$label("Type de symbole")
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
                                      numericInput(ns("select_epaisseur_trait"), label = NULL, min = 0,step = 0.01, max=NA, width = "100px", value =  epaisseur_trait_actif() )
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


                  })#fin isolate



                },
                "LINESTRING"={

                    isolate({
                      tagList(
                        # tagList(

                        #Style de remplissage
                        fluidRow(class="form-group ligne_contenu_modal",
                                 column(width = 4,
                                        tags$label("Type de symbole ligne")
                                 ),
                                 column(width = 8,
                                        selectInput(ns("select_style_fill_symbologie"), label = NULL, choices = list("Ligne simple"="ligne_simple",
                                                                                                                     "Flèche"="fleche",
                                                                                                                     "Ligne de symbole"="ligne_symbole"),  selected = style_fill_symbologie_actif() )
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
                                        numericInput(ns("select_epaisseur_trait"), label = NULL, min = 0,step = 0.01, max=NA, width = "100px", value =  epaisseur_trait_actif() )
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

                    })#fin isolate

                },
                "POINT"={

                }
            )





          #}




        })


        ####Suivi des modification sur les paramètres de definition d'une symbologie#########

            #suivi de la modification des coukeurs de remplissage (polygones)
            observeEvent(input$select_couleur_symbole, {
              req(couche_symbologie_actif()) #il faut qu'une couche soit activée ou sélectionnée


              options_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(), categorie_symbologie_actif(),intervalle_graduation_actif()  )


              req(input$select_couleur_symbole)
              #updateColourInput(session, "select_couleur_symbole", value=input$select_couleur_symbole)
              couleur_remplissage_symbole_actif(input$select_couleur_symbole)

              options_symbologie[[couche_symbologie_actif()]]$couleur_symbole <- input$select_couleur_symbole

              #La couleur de remplissage par defaut est celui de la couche (on fait le remplacement automatique ici)
              options_symbologie[[couche_symbologie_actif()]]$patterns$pattern_colour <-input$select_couleur_symbole# paste0("'", input$select_couleur_symbole, "'")


              #mofification
              nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), options_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())


              options_symbologies_couche_actif(nouvelle_couche)

            })


            #Suivi de de la modification du style de remplissage
            observeEvent(input$select_style_fill_symbologie, {
              req(couche_symbologie_actif()) #il faut qu'une couche soit activée ou sélectionnée

              options_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

              req(input$select_style_fill_symbologie)


              style_fill_symbologie_actif(input$select_style_fill_symbologie)#actualisation du reactif
              options_symbologie[[couche_symbologie_actif()]]$style_fill_symbologie <- input$select_style_fill_symbologie

              #mofification
              nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), options_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

              options_symbologies_couche_actif(nouvelle_couche)
            })



            #suivi du couleur des traits
            observeEvent(input$select_couleur_trait, {
              req(couche_symbologie_actif()) #il faut qu'une couche soit activée ou sélectionnée

              options_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())


              req(input$select_couleur_trait)
              #updateColourInput(session, "select_couleur_trait", value=input$select_couleur_trait)
              couleur_trait_actif(input$select_couleur_trait)

              options_symbologie[[couche_symbologie_actif()]]$couleur_trait <-input$select_couleur_trait

              #mofification
              nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), options_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

              options_symbologies_couche_actif(nouvelle_couche)

            })


            #Suivi de la largeur des traits
            observeEvent(input$select_epaisseur_trait, {
              req(couche_symbologie_actif()) #il faut qu'une couche soit activée ou sélectionnée

              options_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

              req(input$select_epaisseur_trait)
              #updateNumericInput(session, "select_epaisseur_trait", min = 0, max=NA, value=input$select_epaisseur_trait)
              epaisseur_trait_actif(input$select_epaisseur_trait)

              options_symbologie[[couche_symbologie_actif()]]$epaisseur_trait <- input$select_epaisseur_trait

              #mofification
              nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), options_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())


              options_symbologies_couche_actif(nouvelle_couche)
            })


            #Suivi du type d etrait
            observeEvent(input$select_couleur_symbole, {
              req(couche_symbologie_actif()) #il faut qu'une couche soit activée ou sélectionnée

              options_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

              req(input$select_couleur_symbole)
              #updateColourInput(session, "select_couleur_symbole", value=input$select_couleur_symbole)
              couleur_remplissage_symbole_actif(input$select_couleur_symbole)

              options_symbologie[[couche_symbologie_actif()]]$couleur_symbole <- input$select_couleur_symbole

              #mofification
              nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), options_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())


              options_symbologies_couche_actif(nouvelle_couche)
            })





            #Suivi de de la modification du style de remplissage
            observeEvent(input$select_style_trait, {

              req(couche_symbologie_actif()) #il faut qu'une couche soit activée ou sélectionnée

              options_symbologie=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())


              req(input$select_style_trait)

              options_symbologie[[couche_symbologie_actif()]]$style_trait <- input$select_style_trait

              #mofification
              nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), options_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

              options_symbologies_couche_actif(nouvelle_couche)

            })



        ##Controle des couches de symbologie ##########################
        ###### Ajout des couches de symbologie ##########################
        observeEvent(input$ajout_symbologie,{

          req(input$ajout_symbologie)
          req(type_geometrie_couche_actif())


          copie_symbologie_couche=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          #on ajoute une nouvelle couche en fond blanc et bords noirs d’épaisseur 1
          ##on  instancie une instance de couche symbologie
          ordre_couche_symbologie <-length(copie_symbologie_couche)+1
          nouvelle_couche_symbologie = ajouter_nouvelle_couche_symbologie(type_geometrie_couche_actif(), ordre_couche_symbologie )

          # On incréemente la couche de symbologie sur les autres
          copie_symbologie_couche = append(
            copie_symbologie_couche, list(nouvelle_couche_symbologie)
          )

          #on confirme le nom
          names(copie_symbologie_couche)[ordre_couche_symbologie] <- paste("symbologie",ordre_couche_symbologie, sep="_" )#le name


          #mofification de la couche de symbologie principale
          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie_couche,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          #on appliques
          options_symbologies_couche_actif(nouvelle_couche)

        })

        ###### Dupliquer couches de symbologie ##########################
        output$ui_dupliquer_couche_symbologie  <- renderUI({
          req(couche_symbologie_actif())
          actionButton(ns("dupliquer_symbologie"), "", icon = icon("clone"), class="btn-primary btn-sm")
        })

        observeEvent(input$dupliquer_symbologie,{
          req(input$dupliquer_symbologie)

          copie_symbologie_couche=copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())
          copie_couche <- copie_symbologie_couche[[couche_symbologie_actif()]] #eval(parse(text = paste( "copie_symbologie_couche", couche_symbologie_actif(), sep = "$" )   ))


          label_copie_couche <- copie_couche$label# eval(parse(text = paste( "copie_couche","label",  sep = "$" )   ))

          #on modifie le label avec la mention copie
          copie_couche$label <-  paste0(label_copie_couche, " copie")
          #gauche<-  paste( "copie_couche", "label", sep = "$"  )
          #eval(parse(text = paste( gauche,  paste0("'", label_copie_couche, " copie","'"), sep = "<-"  )   ))

          #on modifie le name en meme temps
          copie_couche$name <- paste0(paste("symbologie",length(copie_symbologie_couche)+1, sep="_" ) )
          #gauche_name<-  paste( "copie_couche", "name", sep = "$"  )
          #eval(parse(text = paste( gauche_name,  paste0("'", paste("symbologie",length(copie_symbologie_couche)+1, sep="_" ), "'" )  , sep = "<-"  )   ))

          #on modifie l'ordre ou la position
          copie_couche$position <- length(copie_symbologie_couche)+1
          #gauche_position<-  paste( "copie_couche", "position", sep = "$"  )
          #eval(parse(text = paste( gauche_position,  length(copie_symbologie_couche)+1  , sep = "<-"  )   ))


          copie_symbologie_couche <- append(
            copie_symbologie_couche, list(copie_couche)
          )

          names(copie_symbologie_couche)[length(copie_symbologie_couche)] <- paste("symbologie",length(copie_symbologie_couche), sep="_" )


          #mofification de la couche de symbologie principale
          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie_couche,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(), intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)

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
          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          #### On récupère les paramètres
          pattern_couche <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern# eval(parse(text = paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern",  sep="$") ))
          pattern_couche_actif(pattern_couche)

          pattern_spacing <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_spacing
          pattern_spacing_actif(pattern_spacing)

          pattern_density <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_density
          pattern_density_actif(pattern_density)

          pattern_angle <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_angle
          pattern_angle_actif(pattern_angle)

          pattern_size <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_size
          pattern_size_actif(pattern_size)

          pattern_colour <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_colour
          pattern_colour_actif(pattern_colour)

          pattern_linetype <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_linetype
          pattern_linetype_actif(pattern_linetype)

          pattern_frequency <- copie_symbologie[[couche_symbologie_actif()]]$patterns$frequency
          pattern_frequency_actif(pattern_frequency)

          pattern_fill2 <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_fill2
          pattern_fill2_actif(pattern_fill2)

          pattern_orientation <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_orientation
          pattern_orientation_actif(pattern_orientation)

          pattern_type <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_type
          pattern_type_actif(pattern_type)


          pattern_filename <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_filename
          pattern_filename_actif(pattern_filename)

          pattern_scale <- copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_scale
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
                         column(width = 4, tags$label("Densité")
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

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

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

          #mofification
          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)
        })




        #sapcing
        observeEvent(input$select_pattern_spacing, {
          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          #Espacements
          req(input$select_pattern_spacing)
          pattern_spacing_actif(input$select_pattern_spacing)
          updateNumericInput(session, "select_pattern_spacing", label = NULL, min = 0, max=NA, value =  pattern_spacing_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_spacing <- input$select_pattern_spacing

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)

        })

        #density
        observeEvent(input$select_pattern_density, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          req(input$select_pattern_density)
          pattern_density_actif(input$select_pattern_density)
          updateNumericInput(session, "select_pattern_density", label = NULL, min = 0, max=NA, value =  pattern_density_actif() )


          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_density <- input$select_pattern_density

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)
        })

        #Angle

        observeEvent(input$select_pattern_angle, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          req(input$select_pattern_angle)
          pattern_angle_actif(input$select_pattern_angle)
          updateNumericInput(session, "select_pattern_angle", label = NULL, min = 0, max=NA, value =  pattern_angle_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_angle <- input$select_pattern_angle

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)
        })

        #size
        observeEvent(input$select_pattern_size,{

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          req(input$select_pattern_size)
          pattern_size_actif(input$select_pattern_size)
          updateNumericInput(session, "select_pattern_size", label = NULL, min = 0, max=NA, value =  pattern_size_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_size <- input$select_pattern_size

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)
        })


        #Pattern colour

        observeEvent(input$select_pattern_colour, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())


          req(input$select_pattern_colour)
          pattern_colour_actif(input$select_pattern_colour)
         #updateColourInput(session, "select_pattern_colour", label = NULL, value =  pattern_colour_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_colour <- input$select_pattern_colour

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)

        })



        observeEvent(input$select_pattern_linetype, {
          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

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

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)
        })


        #frequence
        observeEvent(input$select_pattern_frequency, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          req(input$select_pattern_frequency)
          pattern_frequency_actif(input$select_pattern_frequency)
          updateNumericInput(session, "select_pattern_frequency", label = NULL, min = 0, max=NA, value =  pattern_frequency_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_frequency <- input$select_pattern_frequency

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)

        })


        #fill2
        observeEvent(input$select_pattern_fill2, {
          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          req(input$select_pattern_fill2)
          pattern_fill2_actif(input$select_pattern_fill2)
          updateColourInput(session, "select_pattern_fill2", label = NULL, value =  pattern_fill2_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_fill2 <- input$select_pattern_fill2

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(),copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)

        })

        observeEvent(input$select_pattern_orientation, {

          req(couche_symbologie_actif())#il faudra qu'une couche de symbologie soit active

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          req(input$select_pattern_orientation)
          pattern_orientation_actif(input$select_pattern_orientation)
          updateSelectInput(session, "select_pattern_orientation", label = NULL, choices = list("Radial"="radial",
                                                                                                "Horizontal"="horizontal",
                                                                                                "Vertical"="vertical",
                                                                                                "Diagonal"="diagonal"), selected  =  pattern_orientation_actif() )

          copie_symbologie[[couche_symbologie_actif()]]$patterns$pattern_orientation <- input$select_pattern_orientation

          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)



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

          options_symbologie = copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          #suivi de la modification des coukeurs de remplissage (polygones)
          options_symbologie[[couche_symbologie_actif()]]$statut_effet <- statut_effet_actif()

          #mofification
          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), options_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche)


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
                                    p( sprintf("Liste des couches d'effets de %s",  name_couche_actif()  )),
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

          req(input$ajouter_effet)
          req(input$ajouter_effet !="source" )

            effet_choisi <- input$liste_effects_symbologie

            #on charge les options par defaut de l'effet
            options_effet <- options_defaut_effets[[input$liste_effects_symbologie]] #sprintf("%s$%s", "options_defaut_effets", input$liste_effects_symbologie)
              #options_effet<- eval(parse(text = options_effet ))

              nouvelle_liste_effets<- append(options_effets_symbologie_actif(), list(options_effet))
              names(nouvelle_liste_effets)[length(nouvelle_liste_effets)]<- input$liste_effects_symbologie

              #On recharge la liste des effets de la couche
              options_effets_symbologie_actif(nouvelle_liste_effets)



        })


        ### Liste recative des effets ############################
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
                        onclick="alert(this.id)",
                        style="height: 20px ; width:20px;"
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
                        onclick="alert(this.id)",
                        style="height: 20px ; width:20px;"
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
            angle_effet <-effets_couches[[effet_actif()]]$options$angle #eval(parse(text =  paste("effets_couches", effet_actif(), "options", "angle", sep = "$")     ))
            angle_effet_actif(angle_effet)

            distance_effet <- effets_couches[[effet_actif()]]$options$distance  #eval(parse(text =  paste("effets_couches", effet_actif(), "options", "distance", sep = "$")     ))
            distance_effet_actif(distance_effet)

            sigma_effet <-effets_couches[[effet_actif()]]$options$sigma#eval(parse(text =  paste("effets_couches", effet_actif(), "options", "sigma", sep = "$")     ))
            sigma_effet_actif(sigma_effet)

            alpha_effet <-effets_couches[[effet_actif()]]$options$alpha#eval(parse(text =  paste("effets_couches", effet_actif(), "options", "alpha", sep = "$")     ))
            alpha_effet_actif(alpha_effet)

            mode_fusion_effet <-effets_couches[[effet_actif()]]$options$mode_fusion#eval(parse(text =  paste("effets_couches", effet_actif(), "options", "mode_fusion", sep = "$")     ))
            mode_fusion_effet_actif(mode_fusion_effet)

            couleur_effet <-effets_couches[[effet_actif()]]$options$couleur#eval(parse(text =  paste("effets_couches", effet_actif(), "options", "couleur", sep = "$")     ))
            couleur_effet_actif(couleur_effet)

            rayon_effet <-effets_couches[[effet_actif()]]$options$rayon#eval(parse(text =  paste("effets_couches", effet_actif(), "options", "rayon", sep = "$")     ))
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
                                     column(width = 9, colourInput(ns("param_effet_couleur"), label = NULL, value=couleur_effet_actif(),allowTransparent = TRUE, palette = "square", closeOnClick = TRUE)
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


            #slection de l'effet
            observeEvent(input$select_effet_click, {
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

            })


            ##Gestioi des la modification du paramètre angle
            observeEvent(input$param_effet_angle, {
              req(effet_actif())
              req(input$param_effet_angle)

              effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
              effet_en_cours <- effet_actif()

              angle_effet_actif(input$param_effet_angle)
              effets_symbologies[[effet_actif()]]$options$angle <- input$param_effet_angle

              #gauche_angle_effet <- paste("effets_symbologies", effet_actif(),"options",  "angle", sep = "$")
              #eval(parse(text = paste( gauche_angle_effet,angle_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_angle", label = NULL, value = angle_effet_actif(), min = 0, max = 360  )


            })

            ##Gestioi des la modification du paramètre distance
            observeEvent(input$param_effet_distance, {
              req(effet_actif())
              req(input$param_effet_distance)


              effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
              effet_en_cours <- effet_actif()

              distance_effet_actif(input$param_effet_distance)
              effets_symbologies[[effet_actif()]]$options$distance <- input$param_effet_distance

              #gauche_distance_effet <- paste("effets_symbologies", effet_actif(),"options",  "distance", sep = "$")
              #eval(parse(text = paste( gauche_distance_effet,distance_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_distance", label = NULL, value = distance_effet_actif(), min = 0, max = NA  )
            })

            ##Gestioi des la modification du paramètre Rayon de floutage
            observeEvent(input$param_effet_sigma, {
              req(effet_actif())
              req(input$param_effet_sigma)


              effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
              effet_en_cours <- effet_actif()

              sigma_effet_actif(input$param_effet_sigma)
              effets_symbologies[[effet_actif()]]$options$sigma <- input$param_effet_sigma
              #gauche_sigma_effet <- effets_symbologies[[effet_actif()]]$options$sigma#   paste("effets_symbologies", effet_actif(),"options",  "sigma", sep = "$")
              #eval(parse(text = paste( gauche_sigma_effet ,sigma_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_sigma", label = NULL, value = sigma_effet_actif(), min = 0, max = NA  )

            })



             ##Gestioi des la modification du paramètre alpha
             observeEvent(input$param_effet_alpha, {
              req(effet_actif())
              req(input$param_effet_alpha)


              effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
              effet_en_cours <- effet_actif()


                alpha_effet_actif(input$param_effet_alpha)
                effets_symbologies[[effet_actif()]]$options$alpha <- input$param_effet_alpha

                #gauche_alpha_effet <- paste("effets_symbologies", effet_actif(),"options",  "alpha", sep = "$")
                #eval(parse(text = paste( gauche_alpha_effet ,alpha_effet_actif(), sep = "<-" )   ))#on amène la modification
                options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
                updateSliderInput(session, "param_effet_alpha", label = NULL, value = alpha_effet_actif(), min = 0, max = 1, step = 0.1  )

            })


            ##Gestioi des la modification du paramètre mode de fusion
            observeEvent(input$param_effet_mode_fusion,{

              req(effet_actif())
              req(input$param_effet_mode_fusion)


              effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
              effet_en_cours <- effet_actif()


              mode_fusion_effet_actif(input$param_effet_mode_fusion)
              effets_symbologies[[effet_actif()]]$options$mode_fusion <- input$param_effet_mode_fusion
              #gauche_mode_fusion_effet <- paste("effets_symbologies", effet_actif(),"options",  "mode_fusion", sep = "$")
              #eval(parse(text = paste( gauche_mode_fusion_effet ,paste0("'",mode_fusion_effet_actif(),"'"), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateSelectInput(session,"param_effet_mode_fusion", label = NULL,choices = liste_mode_fusion, selected = mode_fusion_effet_actif() )

            })



            ##Gestion des la modification du paramètre Rayons
            observeEvent(input$param_effet_rayon,{
              req(effet_actif())
              req(input$param_effet_rayon)


              effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
              effet_en_cours <- effet_actif()

              rayon_effet_actif(input$param_effet_rayon)
              effets_symbologies[[effet_actif()]]$options$rayon <- rayon_effet_actif()

              #gauche_rayon_effet <- paste("effets_symbologies", effet_actif(),"options",  "rayon", sep = "$")
              #eval(parse(text = paste( gauche_rayon_effet ,rayon_effet_actif(), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateNumericInput(session, "param_effet_rayon", label = NULL, value = rayon_effet_actif(), min = 0, max = NA  )

            })

            ##Gestion des la modification du paramètre couleur
            observeEvent(input$param_effet_couleur,{
              req(effet_actif())
              req(input$param_effet_couleur)


              effets_symbologies <- options_effets_symbologie_actif()#on  preleve les otions en cours
              effet_en_cours <- effet_actif()

              couleur_effet_actif(input$param_effet_couleur)
              effets_symbologies[[effet_actif()]]$options$couleur <- couleur_effet_actif()

              #gauche_couleur_effet <- paste("effets_symbologies", effet_actif(),"options",  "couleur", sep = "$")
              #eval(parse(text = paste( gauche_couleur_effet ,paste0("'",couleur_effet_actif(),"'"), sep = "<-" )   ))#on amène la modification
              options_effets_symbologie_actif(effets_symbologies)#on applique les nouvelles modifications apportées sur la liste des options d'effets
              updateColourInput(session,"param_effet_couleur", label = NULL, value = input$param_effet_couleur )


            })



            #Et si aucun effet n'est choisi
            observe({

              ####Contrôles sur l'effet actif ###########
              if(is.null(effet_actif())){
                  output$parametres_effets_ui<- renderUI({
                    p("Veuiller choisir un effet.")
                  })
              }#fin condition sur les effets

            })



        ###Suivi des changements sur les options globales d'effets ############################
        observeEvent(options_effets_symbologie_actif(), {
          req(couche_symbologie_actif())

          copie_symbologie <- copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          copie_symbologie[[couche_symbologie_actif()]]$effects <- options_effets_symbologie_actif()

          #mofification
          nouvelle_couche <- modifier_couche_symbologie(type_symbologie_actif(), copie_symbologie,options_symbologies_couche_actif(), level_gestion_categorise_actif(),categorie_symbologie_actif(),intervalle_graduation_actif())

          options_symbologies_couche_actif(nouvelle_couche )

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

          copie_couche[[name_couche_actif()]]$type_symbologie <- input$select_type_symbole
          #eval(parse(text = paste0( paste("copie_couche", name_couche ,"type_symbologie", sep="$"), "<-'", input$select_type_symbole, "'" ) ))


          req(input$select_type_symbole)



            if(input$select_type_symbole=="unique"){#Cas des symbologies uniques
              ##on récupère d'abord les noms des clés
              copie_couche[[name_couche_actif()]]$options_symbologie_couche$options_symbologie_unique <- options_symbologies_couche_actif()

              #on applique les modifications à la liste principale des couches
              liste_couches(copie_couche)

              #print(liste_couches())

            }else if(input$select_type_symbole=="categorise"){
              ##on récupère d'abord les noms des clés
              copie_couche[[name_couche_actif()]]$options_symbologie_couche$options_symbologie_categorise <- options_symbologies_couche_actif()

              #on applique les modifications à la liste principale des couches
              liste_couches(copie_couche)

            }else if(input$select_type_symbole=="graduate"){
              ##on récupère d'abord les noms des clés
              copie_couche[[name_couche_actif()]]$options_symbologie_couche$options_symbologie_graduee <- options_symbologies_couche_actif()
              #on applique les modifications à la liste principale des couches
              liste_couches(copie_couche)
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



        #Gestion des Jointures sur les couches#####################################
        observeEvent(input$lancement_jointure_couche,{
            req(input$lancement_jointure_couche)

            #Récupération des paramètres
            data=fromJSON(input$lancement_jointure_couche)

            name_couche=data$name

            #On propage dans toute l'application les données sur la couche en cours
            name_couche_actif(name_couche)




            #On lance la fenetre modale

            showModal(modalDialog(
              title = paste0("Options de Jointure de la couche ", name_couche),
              footer=tagList(
                div(class="div_footer_modal",
                    actionButton(ns("bouton_ok_jointure_couche"), "Ok", class="btn-success"),
                    actionButton(ns("bouton_appliquer_jointure_couche"), "Appliquer", class="btn-success"),
                    modalButton("Annuler")
                )
              ),

              fluidRow(
                withSpinner(
                  uiOutput(ns("options_jointures_layer_ui")) )
              ),#on paramètre le contenu de la fenêtre cible ici
              #trigger = "ajouter_couche",
              #scrollable=TRUE,
              size="l",

              easyClose = TRUE
            ))





        })



        ##Interface de gestion de la jointure sur les couches##############################

        AjoutNouvelleJOinture <- reactiveVal(
          FALSE
        )

        ModifierNouvelleJOinture <- reactiveVal(
          FALSE
        )


        TabeJointureActive <-reactiveVal(NULL)

        ColonneTableJointureActive <- reactiveVal(NULL)

        ColonneCoucheCibleJointureActive <- reactiveVal(NULL)

        JointureActive <- reactiveVal(NULL)

        #liste active des jointures de la couche
        ListeJointuresCoucheActive <- reactiveVal(
          list()
          )


        output$options_jointures_layer_ui <- renderUI({
            tagList(
              fluidRow(
                uiOutput(ns("liste_jointures_ui"))
              ),
              fluidRow(
                uiOutput(ns("options_parametrage_jointure"))
              )
            )
        })




        #Afichage des doption de gestion des jointures
        output$liste_jointures_ui <- renderUI({
          #req(AffichageDetailsJointure()==FALSE)
          req(AjoutNouvelleJOinture()==FALSE && ModifierNouvelleJOinture()==FALSE)

          tagList(
            fluidRow(style="border:1px solid #DCDCDC; min-height:500px;position:relative;", class="ligne_contenu_modal",
                     tags$div(
                       class="liste_div_jointures", style="max-height:300px;width:100%;",
                       ListeJointuresCoucheActiveUI()
                     )

            ),

            fluidRow(class="ligne_contenu_modal",
                     actionButton(ns("ajout_jointure"), "Nouvelle jointure", icon = icon("add"), class="btn-primary btn-sm"),
                     uiOutput(ns("options_edit_jointure_ui"))
            )


          )

        })



        ### Options de détail d'une jointure

        ##Lorsqu'on choisit d'annuler la configuration d'ajout d'une jointure

        observeEvent(input$ajout_jointure, {
            req(input$ajout_jointure)

            #On passe en mode ajout de jointure
            AjoutNouvelleJOinture(TRUE)

        })

        observeEvent(AjoutNouvelleJOinture(),{
          if(AjoutNouvelleJOinture()==TRUE){
            output$options_parametrage_jointure <- renderUI({
              tagList(
                FormulaireGestionJointureUI(),
                tags$div(class="ligne_contenu_modal",
                         tagList(
                           actionButton(ns("btn_ajout_jointure_table"), "Sauvegarder"),
                           actionButton(ns("btn_annuler_jointure_table"), "Annuler"),
                         )


                )
              )

            })
          }else{
            output$options_parametrage_jointure <- renderUI({

            })
          }

        })



        ##### Confirmer l'ajout de la jointure###############
        observeEvent(input$btn_ajout_jointure_table,{
          req(input$btn_ajout_jointure_table)

          #on initialise une nouvelle jointure
          nouvelle_jointure=options_defaut_jointure

          #Personnalisation avec les informations nouvelles


              nouvelle_jointure$name_table <- input$select_table_jointure
              nouvelle_jointure$colonne_table <- snakecase::to_snake_case(input$select_colonne_jointure_table)
              nouvelle_jointure$colonne_couche_cible <- input$select_colonne_jointure_couche


          #on recupère la liste des jointures de la couche
          jointures_couche=liste_couches()[[name_couche_actif()]]$jointures

          nouvelle_jointure$name_jointure <- paste0("jointure_",length(jointures_couche)+1 )

          #on incrémente la liste
          jointures_couche <- append(jointures_couche, list(nouvelle_jointure) )
          names(jointures_couche)[length(jointures_couche)] <- paste0("jointure_",length(jointures_couche) )

          #On applique le changement à la liste des jointures
          ListeJointuresCoucheActive(jointures_couche)

          #On remet les infos à leur place
          AjoutNouvelleJOinture(FALSE)

        })


        ####Annuler l'ajout de la jointure######
        observeEvent(input$btn_annuler_jointure_table, {
          req(input$annuler_ajout_jointure)
          AjoutNouvelleJOinture(FALSE)
        })



        ##### liste reactive des jointures de la couche active########################
        ListeJointuresCoucheActiveUI <- reactive({
              print(ListeJointuresCoucheActive())


              tagList(

                fluidRow(class="ligne_jointure", style="display:flex; flex-wrap : nowrap; gap :10px; background:#DCDCDC;height:50px; position:relative;top:0;",
                         tagList(
                           #l'entete
                           tags$div(style="width:50%", class="col-md-6",
                                    "Table"
                           ),
                           tags$div(style="width:25%", class="col-md-3",
                                    "Colonne de la table"
                           ),
                           tags$div(style="width:25%", class="col-md-3",
                                    "Colonne dans la couche"
                           )
                         )
                ),


                lapply(ListeJointuresCoucheActive(), function(i){

                      fluidRow(class="ligne_jointure", style="overflow:auto; display:flex; flex-wrap : nowrap; gap :10px;position:relative;", "data-jointure"=i$name_jointure,
                               tagList(
                                 tags$div(style="width:50%", class="col-md-6",
                                          i$name_table
                                 ),
                                 tags$div(style="width:25%", class="col-md-3",
                                          i$colonne_table
                                 ),
                                 tags$div(style="width:25%", class="col-md-3",
                                          i$colonne_couche_cible
                                 )
                               ),

                               onclick="gestion_click_jointure(this.dataset.jointure)"
                      )


                })

              )

        })



        #on rend aussi réeactif les chams de gestion pour faciliter la reutilisation selon les cas
        FormulaireGestionJointureUI <- reactive({

              data_couche_courant=st_drop_geometry( liste_couches()[[name_couche_actif()]]$couche)

              tagList(
                #Style de remplissage
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Table de jointure")
                         ),
                         column(width = 8,
                                selectInput(ns("select_table_jointure"), label = NULL, choices = listeOjetsTidyEnv,  selected = TabeJointureActive() )
                         )
                ),
                #Style de remplissage
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Colonne de jointure de la table")
                         ),
                         column(width = 8,
                                selectInput(ns("select_colonne_jointure_table"), label = NULL, choices = list("Continue"="continu",
                                                                                                              "Pas de remplissage"="blank",
                                                                                                              "Motif"="motif"),  selected = ColonneTableJointureActive() )
                         )
                ),
                #Style de remplissage
                fluidRow(class="form-group ligne_contenu_modal",
                         column(width = 4,
                                tags$label("Colonne de jointure sur la couche cible")
                         ),
                         column(width = 8,
                                selectInput(ns("select_colonne_jointure_couche"), label = NULL, choices = colnames(data_couche_courant),  selected = ColonneCoucheCibleJointureActive() )
                         )
                )

              )
        })



        #Modification d'une jointure active ##########

        #####Clic sur la jointure##########
        observeEvent(input$select_jointure_couche,{
            req(input$select_jointure_couche)
            donnees=fromJSON(input$select_jointure_couche)


            #On recupère les informations actives sur la jointure
            table = ListeJointuresCoucheActive()[[donnees$name]]$name_table
            TabeJointureActive(table)

            colonneTable=ListeJointuresCoucheActive()[[donnees$name]]$colonne_table
            ColonneTableJointureActive(colonneTable)

            ColonneCouche=ListeJointuresCoucheActive()[[donnees$name]]$colonne_couche_cible
            ColonneCoucheCibleJointureActive(ColonneCouche)

            JointureActive(donnees$name)
        })


        output$options_edit_jointure_ui <- renderUI({
          req(JointureActive())#eviter les cas NULL

          tagList(
            actionButton(ns("btn_modifier_jointure_table"), "Modifier"),
            actionButton(ns("btn_supprimer_jointure_table"), "Supprimer"),
          )


        })



        #####Modifier une jointure sur la couche active ###############
        observeEvent(input$btn_modifier_jointure_table,{
            req(input$btn_modifier_jointure_table)#Pour eviter le NULL

            ModifierNouvelleJOinture(TRUE)
        })

        ####### On Affiche les options de la gestion de la modification##################################
        observeEvent(ModifierNouvelleJOinture(),{
          if(ModifierNouvelleJOinture()==TRUE){
            output$options_parametrage_jointure <- renderUI({
              tagList(
                FormulaireGestionJointureUI(),
                tags$div(class="ligne_contenu_modal",
                         tagList(
                           actionButton(ns("btn_confirmer_modif_jointure_table"), "Sauvegarder les modifications"),
                           actionButton(ns("btn_annuler_modif_jointure_table"), "Annuler"),
                         )


                )
              )

            })
          }else{
            output$options_parametrage_jointure <- renderUI({

            })
          }

        })

        ##########Si on annule la modification########################################
        observeEvent(input$btn_annuler_modif_jointure_table,{
          req(input$btn_annuler_modif_jointure_table)

          ModifierNouvelleJOinture(FALSE)
        })


        ######Si l'utilisateur Confirme la modification de la jointure######################
        observeEvent(input$btn_confirmer_modif_jointure_table,{
            req(input$btn_confirmer_modif_jointure_table)

            #on copie la couche des jointures
            copie_jointures=ListeJointuresCoucheActive()
            copie_jointures[[JointureActive()]]$name_table <- input$select_table_jointure
            copie_jointures[[JointureActive()]]$colonne_table <- snakecase::to_snake_case(input$select_colonne_jointure_table)
            copie_jointures[[JointureActive()]]$colonne_couche_cible <- input$select_colonne_jointure_couche

            #On applique les modifications sur la liste des jointures de la couche
            ListeJointuresCoucheActive(copie_jointures)

            ModifierNouvelleJOinture(FALSE)

        })





        #########actualisation automatique de la liste des colonnes de la table de jointure###################
        observeEvent(input$select_table_jointure, {
          #req(input$select_table_jointure)
          #req(ModifierNouvelleJOinture()==FALSE)

          #actualisation de la liste ds colones
          updateSelectInput(session, "select_colonne_jointure_table", choices=colnames(get(input$select_table_jointure)), selected = ColonneTableJointureActive() )

        })



        ########Confirmer l'appliation des jointures sur la couche#########################
        observeEvent(input$bouton_appliquer_jointure_couche,{
          req(input$bouton_appliquer_jointure_couche)

          copie_couche=liste_couches()
          copie_couche[[name_couche_actif()]]$jointures <- ListeJointuresCoucheActive()

          liste_couches(copie_couche)

          AjoutNouvelleJOinture(FALSE)
          ModifierNouvelleJOinture(FALSE)

        })



        ######Fermer la boite de dialogue des jointures#############
        observeEvent(input$bouton_ok_jointure_couche, {
            req(input$bouton_ok_jointure_couche)

            removeModal()

        })


        #Gestion de la symbologie graduée (peu diffeérent du catégorise) ##############################
        minimum_intervalle_graduation_actif <- reactiveVal(NULL)
        maximum_intervalle_graduation_actif <- reactiveVal(NULL)
        legende_intervalle_graduation_actif <- reactiveVal(NULL)
        intervalle_graduation_actif <- reactiveVal(NULL)
        AjoutNouvelleIntervalleGrade <- reactiveVal(FALSE)
        ModifierNouvelleIntervalleGrade <- reactiveVal(FALSE)
        EditionIntervalleGrade <- reactiveVal(FALSE)



        ####Lorsqu'on choisit d'ajouter une classe d'intervalle pour la symbologie graduee
        observeEvent(input$ajouter_intervalle,{
          req(input$ajouter_intervalle)

          minimum_intervalle_graduation_actif(NULL)
          maximum_intervalle_graduation_actif(NULL)
          legende_intervalle_graduation_actif(NULL)
          StatutInclusionBorneInferieureIntervalleGrade(NULL)
          StatutInclusionBorneSuperieureIntervalleGrade(NULL)


          EditionIntervalleGrade(TRUE)
          AjoutNouvelleIntervalleGrade(TRUE)

        })


        #on renvoie les options d'une intervalle


        output$gestionnaire_ajout_intervalle_graduee <- renderUI({
           req(EditionIntervalleGrade()==TRUE )



            UIgestionIntervallesGrades()
        })



        #####Ui de gestion des intervalles###########
        StatutInclusionBorneInferieureIntervalleGrade <-reactiveVal(NULL)
        StatutInclusionBorneSuperieureIntervalleGrade <-reactiveVal(NULL)



        UIgestionIntervallesGrades <- reactive({
              tagList(
                fluidRow(class="ligne_contenu_modal ligne_formulaire_intervalles", style="margin-top:20px;",
                  column(width = 6,
                         numericInput(ns("min_intervalle_grade"), label = "Minimum", min = 0, step = 0.01, value =minimum_intervalle_graduation_actif() )
                  ),
                  column(width = 6,
                         numericInput(ns("max_intervalle_grade"), label = "Maximum ", min = 0, step = 0.01, value = maximum_intervalle_graduation_actif())
                  )
                ),
                fluidRow(class="ligne_contenu_modal ligne_formulaire_intervalles",
                  column(width = 6,
                         checkboxInput(ns("IncBorneInfIntervalleGraduation"), "Inclure Borne Inférieure", value = StatutInclusionBorneInferieureIntervalleGrade() )
                  ),
                  column(width = 6,
                         checkboxInput(ns("IncBorneSupIntervalleGraduation"), "Inclure Borne Supérieure", value = StatutInclusionBorneSuperieureIntervalleGrade() )

                  )
                ),

                fluidRow(class="ligne_contenu_modal ligne_formulaire_intervalles",
                  column(width = 5,
                         tags$label("Legende ")
                  ),
                  column(width = 7,
                         textInput(ns("legende_intervalle_grade"), label = NULL, value = legende_intervalle_graduation_actif())
                  )
                ),
                fluidRow(style="display:flex; flex-wrap : nowrap; gap:10px;", class="ligne_contenu_modal ligne_formulaire_intervalles",
                  tagList(
                    uiOutput(ns("action_sauvegarde_intervalle_grade")),
                    actionButton(ns("annuler_action_intervalle"), "Annuler", icon = icon("stop") )
                  )
                )
              )
        })


        output$action_sauvegarde_intervalle_grade<- renderUI({
            if(AjoutNouvelleIntervalleGrade()==TRUE){
              actionButton(ns("enregistrer_intervalle"), "Enregistrer", icon = icon("save") )
            }else if(ModifierNouvelleIntervalleGrade()==TRUE){
              actionButton(ns("enregistrer_modif_intervalle"), "Enregistrer", icon = icon("save") )
            }
        })



        #####Enregistrement d'un intervalle##############
        observeEvent(input$enregistrer_intervalle,{
          req(input$enregistrer_intervalle)

          #on initialise un nouvel intervalle
          nouvel_intervalle=options_defaut_categories_intervalles_symbologies
            #personnaliser les informations
            nouvel_intervalle$minimum_intervalle <- input$min_intervalle_grade
            nouvel_intervalle$maximum_intervalle <- input$max_intervalle_grade
            nouvel_intervalle$legende <- input$legende_intervalle_grade
            nouvel_intervalle$fermeture_borne_inf <- input$IncBorneInfIntervalleGraduation
            nouvel_intervalle$fermeture_borne_sup <- input$IncBorneSupIntervalleGraduation

          #On copie les couches desymbologie du niveau symbole
          couches_symboles = copier_liste_couches_symbologies(type_symbologie_actif(), options_symbologies_couche_actif(), "symbole")


          #On preleve toutes les options de symbologie
          copie_symbologie= options_symbologies_couche_actif()

          copie_intervalles=copie_symbologie$categories

          nbre_intervalles = length(copie_intervalles)#le nombre d'intervalles déjà enregistrés


          nouvel_intervalle$name<-paste0("intervalle_", nbre_intervalles+1)


          #Gestion des couleurs des palettes
          if(input$select_palette_categories=="Aleatoire"){
            couleur_palette = sample(liste_couleurs, 1)#on prelève une couleur au hasard
          }else{#le reste des palettes des couleurs
            contenu_palette=brewer.pal( 20, input$select_palette_categories)#on prend les couleurs de la palette choisie
            couleur_palette=contenu_palette[nbre_intervalles+1]#on avance d'un cran la couleur
          }

          #on applique d'abord la couleur de la palette
          for (j in 1:length(couches_symboles)) {
            couches_symboles[[paste0("symbologie_", j)]]$couleur_symbole <- couleur_palette
          }


          #on applique la symbologie à l'intervalle
          nouvel_intervalle$couches_symbologies <- couches_symboles

          #on l'ajoute sur la liste des couches des intervalles
          copie_intervalles = append(copie_intervalles, list(nouvel_intervalle))
          #on nomme la couche en cours
          names(copie_intervalles)[nbre_intervalles+1] <- paste0("intervalle_", nbre_intervalles+1 )


          copie_symbologie$categories <- copie_intervalles


          #on inpute aussi le nom dela colonne
          copie_symbologie$colonne_valeur_symbologie <- input$select_colonne_valeur_symbologie
          colonne_valeur_symbologie_actif(input$select_colonne_valeur_symbologie)

          #on impute egalement la plette de couleur
          copie_symbologie$palette_couleurs <- input$select_palette_categories
          palette_couleur_symbologie_actif(input$select_palette_categories)

          #Ajout des modifications sur la couche globale des symbologies
          options_symbologies_couche_actif(copie_symbologie)


          #on désactive le volet d'ajout
          AjoutNouvelleIntervalleGrade(FALSE)
          EditionIntervalleGrade(FALSE)

        })


        ##### Annuler l'ajout d'un intervalle de graduations################
        observeEvent(input$annuler_action_intervalle,{
          req(input$annuler_action_intervalle)

          #on désactive le volet d'ajout
          AjoutNouvelleIntervalleGrade(FALSE)
          ModifierNouvelleIntervalleGrade(FALSE)

          EditionIntervalleGrade(FALSE)

        })



        #####Si l’utilisateur clique sur une ligne d'intervalle de symbologie########################
        observeEvent(input$clic_intervalle_graduation_symbologie, {
          req(input$clic_intervalle_graduation_symbologie)

          donnees=fromJSON(input$clic_intervalle_graduation_symbologie)

          #On actualise le level
          level_gestion_categorise_actif(donnees$level)

          #On actuaise la valeur
          intervalle_graduation_actif(donnees$name)

          #On copie les couches de symbologie de l'intervalle en question (level=Categorie)
          copie_intervalle= options_symbologies_couche_actif()$categories[[donnees$name]]

          #Activation de la modification
          EditionIntervalleGrade(TRUE)



          #On renseigne les paramètres utiles au chargement de l'application
          minimum_intervalle_graduation_actif(copie_intervalle$minimum_intervalle)
          maximum_intervalle_graduation_actif(copie_intervalle$maximum_intervalle)
          legende_intervalle_graduation_actif(copie_intervalle$legende)

          StatutInclusionBorneInferieureIntervalleGrade(copie_intervalle$fermeture_borne_inf)
          StatutInclusionBorneSuperieureIntervalleGrade(copie_intervalle$fermeture_borne_sup)

          #appliquer la modification au niveau global
          ModifierNouvelleIntervalleGrade(TRUE)
        })


        #######Enregistrement des modifications faites sur les intervalles de graduations #####
        observeEvent(input$enregistrer_modif_intervalle,{
          req(input$enregistrer_modif_intervalle)

          #On copie la liste complète des intervalles
          liste_intervalles=options_symbologies_couche_actif()$categories

          #on modifie l'intervalle courante
          liste_intervalles[[intervalle_graduation_actif()]]$minimum_intervalle <- input$min_intervalle_grade
          liste_intervalles[[intervalle_graduation_actif()]]$maximum_intervalle <- input$max_intervalle_grade
          liste_intervalles[[intervalle_graduation_actif()]]$legende <- input$legende_intervalle_grade

          #on remet le tout dans couche de symbologie globlale
          copie_symbologies=options_symbologies_couche_actif()
          copie_symbologies$categories <- liste_intervalles

          options_symbologies_couche_actif(copie_symbologies)

          #On restaure les paramètres de gestion de l'édition
          ModifierNouvelleIntervalleGrade(FALSE)
          EditionIntervalleGrade(FALSE)
        })


        #####Supprimer les caégories (S'applqiue aux catériques et règles derivées)
        observeEvent(input$tout_supprimer_categories, {
          req(input$tout_supprimer_categories)

          #on copie toutes les couches de symbolgie de la couche courante
          copie_symbolgie = options_symbologies_couche_actif()

          copie_symbolgie$categories <-list()

          options_symbologies_couche_actif(copie_symbolgie)

        })





        #Gestion des éetiquettes de la couche vecteur##################
        type_etiquette_actif <- reactiveVal(NULL)
        colonne_etiquette_actif <-reactiveVal(NULL)
        police_etiquette_actif <- reactiveVal(NULL)
        style_police_etiquette_actif <- reactiveVal()
        taille_police_etiquette_actif <- reactiveVal(NULL)
        couleur_police_etiquette_actif <- reactiveVal(NULL)
        options_etiquettte_couche_actif <- reactiveVal(NULL)



        observeEvent(input$gestionnaire_etiquette_vecteur,{
          req(input$gestionnaire_etiquette_vecteur)#pour éviter les actions non prévues

          donnees=fromJSON(input$gestionnaire_etiquette_vecteur)
          name_couche=donnees$name
          name_couche_actif(name_couche)

          #On copie la liste des couches
          copie_couches=liste_couches()
          label_couche=copie_couches[[name_couche]]$label_couche
          label_couche_actif(label_couche)

          #Le type d'une étiquette
          type_etiquette=copie_couches[[name_couche]]$options_etiquette$type
          type_etiquette_actif(type_etiquette)

          #La colonne
          colonne_etiquette <- copie_couches[[name_couche]]$options_etiquette$colonne
          colonne_etiquette_actif(colonne_etiquette)

          #on prend la police
          police_etiquette = copie_couches[[name_couche]]$options_etiquette$police
          police_etiquette_actif(police_etiquette)

          #Le style de la police
          style_police_etiquette <- copie_couches[[name_couche]]$options_etiquette$style
          style_police_etiquette_actif(style_police_etiquette)

          #La taille de la police
          taille_police_etiquette <- copie_couches[[name_couche]]$options_etiquette$taille
          taille_police_etiquette_actif(taille_police_etiquette)

          #La couleur
          couleur_police_etiquette <- copie_couches[[name_couche]]$options_etiquette$couleur
          couleur_police_etiquette_actif(couleur_police_etiquette)

          #Les options de paramétrage de l'étiquette
          options_etiquettte_couche_actif(copie_couches[[name_couche]]$options_etiquette)


          showModal(modalDialog(
            title = paste("Option des étiquettes de la couche", label_couche, sep = " "),
            footer=tagList(
              div(class="div_footer_modal",

                  modalButton("Annuler"),
                  actionButton(ns("bouton_ok_etiquette_couche"), "Fermer", class="btn-success"),
                  actionButton(ns("bouton_appliquer_etiquette_couche"), "Appliquer", class="btn-success")
              )
            ),

            fluidRow(
              withSpinner(
                uiOutput(ns("options_gestion_etiquettes_layer_ui")) )
            ),#on paramètre le contenu de la fenêtre cible ici
            #trigger = "ajouter_couche",
            #scrollable=TRUE,
            size="l",

            easyClose = TRUE
          ))







        })


        ###Les  options de gestion des étiquettes d'une couche vecteur#########################

        output$options_gestion_etiquettes_layer_ui <- renderUI({
            tagList(
              fluidRow(class="ligne_contenu_modal",
                  column(width = 3, tags$label("Type ")),
                  column(width = 9,
                         selectInput(ns("select_type_etiquette"), label = NULL, choices = c("Pas d'étiquettes"="pas_detiquettes",
                                                                                            "Etiquettes simples"="etiquettes_simples"),
                                     selected = type_etiquette_actif()
                                     )
                         )

              ),

              uiOutput(ns("options_gestion_etiquettes_layer"))

            )
        })


        paramatrage_supp_etiquette <- reactiveVal("Non")


        ###Affichage des options de personnalisation des étiquetets d'une couche#####################
        output$options_gestion_etiquettes_layer <- renderUI({
          req(input$select_type_etiquette)
          req(input$select_type_etiquette=="etiquettes_simples")

          req(paramatrage_supp_etiquette())

          if(paramatrage_supp_etiquette()=="Non"){



               sortie= tagList(
                  fluidRow(
                    fluidRow(class="ligne_contenu_modal",
                             column(width = 3, tags$label("Colonne")),
                             column(width = 9,
                                    selectInput(ns("select_colonne_etiquette"), label = NULL, choices=colnames(DataCoucheActive()), selected = colonne_etiquette_actif() )
                             )
                    ),
                    fluidRow( style="height:180px;", class="ligne_contenu_modal",
                              tagList(
                                tags$p("Prévisualisation du texte"),
                                imageOutput(ns("PrevisualisationEtiquette_UI"), height = "200px"),#Pour la prévisualisation des inputs
                              )

                    ),

                    fluidRow(class="ligne_contenu_modal",
                             column(width = 3, tags$label("Police")),
                             column(width = 9,
                                    selectInput(ns("select_police_etiquette"), label = NULL, choices=liste_polices, selected = police_etiquette_actif() )
                             )
                    ),

                    fluidRow(class="ligne_contenu_modal",
                             column(width = 3, tags$label("Style")),
                             column(width = 9,
                                    selectInput(ns("select_style_police_etiquette"), label = NULL, choices=NULL, selected = style_police_etiquette_actif() )
                             )
                    ),

                    fluidRow(class="ligne_contenu_modal",
                             column(width = 3, tags$label("Taille")),
                             column(width = 9,
                                    numericInput(ns("taille_police_etiquette"), label = NULL,min = 0, value = taille_police_etiquette_actif() )
                             )
                    ),
                    fluidRow(class="ligne_contenu_modal",
                             column(width = 3, tags$label("Couleur")),
                             column(width = 9,
                                    colourInput(ns("couleur_police_etiquette"), label = NULL, value=couleur_police_etiquette_actif(),allowTransparent = TRUE, palette = "square", closeOnClick = TRUE)
                             )
                    )


                  ),#FIn du fluidrow large
                  fluidRow(class="ligne_contenu_modal",
                           tagList(
                             actionButton(ns("btn_tampon_etiquette"), "Tampon"),
                             actionButton(ns("btn_ombre_etiquette"), "Ombre"),
                             actionButton(ns("btn_position_etiquette"), "Position"),
                             actionButton(ns("btn_background_etiquette"), "Arrière plan")
                           )
                  )

                )


          }else{#on acffiche simplement un bouton de retour aux options globales

            sortie=tagList(
              fluidRow(class="ligne_contenu_modal", style="margin-bottom:15px;",
                       actionButton(ns("btn_retour_options_globales_etiquesttes"), "Retour aux options globales")
                       ),
              fluidRow(class="ligne_contenu_modal",
                       tagList(
                         uiOutput(ns("liste_options_param_supp")),
                         fluidRow(
                           tagList(
                             tags$p("Prévisualisation du texte"),
                             imageOutput(ns("PrevisualisationEtiquette_UI2"), height = "200px"),#Pour la prévisualisation des inputs
                           )
                         )

                       )

              )
            )





          }


          sortie

        })


        option_controle_supp_etiquette_actif <- reactiveVal(NULL)
        rayon_tampon_actif<- reactiveVal(NULL)
        sigma_rayon_actif <- reactiveVal(NULL)
        alpha_tampon_actif <- reactiveVal(NULL)
        couleur_tampon_actif <- reactiveVal(NULL)
        mode_fusion_tampon_actif <- reactiveVal(NULL)
        statut_activation_tampon <- reactiveVal(NULL)

        #les options reactives de controle des etiquettes

        output$liste_options_param_supp <- renderUI({
          req(option_controle_supp_etiquette_actif())

          parametresSuppEtiquettesUI()
        })


        parametresSuppEtiquettesUI <- reactive({
          req(options_etiquettte_couche_actif())

          #On copie les otpions d'étiquettes
          copie_options_etiquettes=options_etiquettte_couche_actif()

          rayon_tampon_actif(copie_options_etiquettes$tampon$rayon)
          sigma_rayon_actif(copie_options_etiquettes$tampon$sigma)
          alpha_tampon_actif(copie_options_etiquettes$tampon$alpha)
          couleur_tampon_actif(copie_options_etiquettes$tampon$couleur)
          mode_fusion_tampon_actif(copie_options_etiquettes$tampon$mode_fusion)
          statut_activation_tampon(copie_options_etiquettes$tampon$statut)

          switch (option_controle_supp_etiquette_actif(),
              "Tampon" = {#Opn de controle des effets de tanpon

                sortie=tagList(
                      #stataut de validation des options du tampon
                      fluidRow(class="form-group ligne_contenu_modal",
                               checkboxInput(ns("select_statut_tampon"), "Activer les options le tampon", value = statut_activation_tampon() )
                      ),

                      fluidRow(
                        uiOutput(ns("details_options_tampon_etiquette"))
                      )


                )

              }
          )



          sortie

        })


        ###Gestion des tampons sur les étiquettes
        observeEvent(input$btn_tampon_etiquette,{
          req(input$btn_tampon_etiquette)
          option_controle_supp_etiquette_actif("Tampon")
          paramatrage_supp_etiquette("Oui")
        })




        ###Les options du tampon



        ###Bouton de retour aux options de controle##################
        observeEvent(input$btn_retour_options_globales_etiquesttes, {
          req(input$btn_retour_options_globales_etiquesttes)

          #on réinitialise les paramètres
          option_controle_supp_etiquette_actif(NULL)
          paramatrage_supp_etiquette("Non")

        })


        ###On choisit d'activer les tampons#########
        observeEvent(input$select_statut_tampon,{

          copie_options_etiquettes=options_etiquettte_couche_actif()
          copie_options_etiquettes$tampon$statut <- input$select_statut_tampon
          #statut_activation_tampon(input$select_statut_tampon)

          options_etiquettte_couche_actif(copie_options_etiquettes)
          updateCheckboxInput(session, "select_statut_tampon", value = input$select_statut_tampon  )

          output$details_options_tampon_etiquette <- renderUI({
            req(input$select_statut_tampon)

            tagList(
              #Rayon
              fluidRow(class="form-group ligne_contenu_modal",
                       column(width = 4, tags$label("Rayon")),
                       column(width = 8, numericInput(ns("param_rayon_tampon_etiquette"), label = NULL, min = 0, max=NA, width = "100px", value = rayon_tampon_actif() )
                       )
              ),

              #sigma
              fluidRow(class="form-group ligne_contenu_modal",
                       column(width = 4,  tags$label("Rayon de floutage")),
                       column(width = 8, numericInput(ns("param_tampon_sigma"), label = NULL, min = 0, max=NA, width = "100px", value = sigma_rayon_actif() ) )
              ),

              #Alpha
              fluidRow( class="form-group ligne_contenu_modal",
                        column(width = 4,  tags$label("Opacité  ")),
                        column(width = 8, sliderInput(ns("param_tampon_alpha"), label = NULL, min = 0, max = 1, value=alpha_tampon_actif(), sep = 0.1)  )

              ),#,

              #Couleur d
              fluidRow(class="form-group ligne_contenu_modal",
                       column(width = 4, tags$label("Couleur")),
                       column(width = 8, colourInput(ns("param_tampon_couleur"), label = NULL, value=couleur_tampon_actif())
                       )
              ),

              #Mode de fusion
              fluidRow(class="form-group ligne_contenu_modal",
                       column(width = 4, tags$label("Mode de fusion") ),
                       column(width = 8, selectInput(ns("param_tampon_mode_fusion"), label = NULL,  choices = liste_mode_fusion, selected = mode_fusion_tampon_actif()   )
                       )
              )

            )
          })

        })



        observeEvent(input$param_rayon_tampon_etiquette, {
          req(input$param_rayon_tampon_etiquette)

          copie_options=options_etiquettte_couche_actif()
          copie_options$tampon$rayon=input$param_rayon_tampon_etiquette

          updateSelectInput(session,"param_rayon_tampon_etiquette", selected = input$param_rayon_tampon_etiquette )
          options_etiquettte_couche_actif(copie_options)
        })


        observeEvent(input$param_tampon_sigma, {
          req(input$param_tampon_sigma)

          copie_options=options_etiquettte_couche_actif()
          copie_options$tampon$sigma=input$param_tampon_sigma

          updateSelectInput(session,"param_tampon_sigma", selected = input$param_tampon_sigma )
          options_etiquettte_couche_actif(copie_options)
        })


        observeEvent(input$param_tampon_alpha, {
          req(input$param_tampon_alpha)

          copie_options=options_etiquettte_couche_actif()
          copie_options$tampon$alpha=input$param_tampon_alpha

          updateNumericInput(session,"param_tampon_alpha", value = input$param_tampon_alpha )
          options_etiquettte_couche_actif(copie_options)
        })


        observeEvent(input$param_tampon_couleur, {
          req(input$param_tampon_couleur)

          copie_options=options_etiquettte_couche_actif()
          copie_options$tampon$couleur=input$param_tampon_couleur

          updateColourInput(session,"param_tampon_couleur", label=NULL, value = input$param_tampon_couleur)
          options_etiquettte_couche_actif(copie_options)
        })


        observeEvent(input$param_tampon_mode_fusion, {
          req(input$param_tampon_mode_fusiona)

          copie_options=options_etiquettte_couche_actif()
          copie_options$tampon$sigma=input$param_tampon_mode_fusion

          updateSelectInput(session,"param_tampon_sigma", selected = input$param_tampon_mode_fusion )
          options_etiquettte_couche_actif(copie_options)
        })




        ###Prévisualisation des étiquettes#######################
        output$PrevisualisationEtiquette_UI <- renderImage({

          graphique <- PrevisualisationEtiquetteUI()

          outfile<- tempfile(fileext = "png")
          png(outfile, width =800, height =100, res = resolution_page_actif() )
          print(graphique)
          dev.off()
          list(src=outfile)


        })



        output$PrevisualisationEtiquette_UI2 <- renderImage({

          graphique <- PrevisualisationEtiquetteUI()

          outfile<- tempfile(fileext = "png")
          png(outfile, width =800, height =100, res = resolution_page_actif() )
          print(graphique)
          dev.off()
          list(src=outfile)


        })



        PrevisualisationEtiquetteUI <-reactive({
             copie_couche =liste_couches()

             type_geometrie=copie_couche[[name_couche_actif()]]$geometrie
             type_geometrie_couche_actif(type_geometrie)


            #on concoit les polygones de visualisation de la couche
             switch (type_geometrie,
                     "POLYGON" = {
                       data_graph <- rbind( c(0.0), c(800,0), c(800,100), c(0,100), c(0,0) )#les données du graphique  ==>> la couche
                       polygone <- st_polygon(list(data_graph))

                       sfc_obj <- st_sfc(list(polygone))

                     },
                     "LINESTRING"={
                       data_graph <- rbind( c(0,0.5), c(1,0.5) )#les données du graphique  ==>> la couche
                       polygone <- st_linestring(data_graph)

                       sfc_obj <- st_sfc(list(polygone))

                     },
                     "POINT"={

                     }
             )

             data_points <- data.frame(titre=c("Titre d'Essai"))#Le mot d'essai
             data_points$geometry <- sfc_obj
             data_points <- st_as_sf(data_points)

             #On esssaie de construire un graphique
             texte_graphique<- paste0('ggplot() +
                          geom_sf(data=data_points, fill=NA, colour="#000000", linewidth=1) ')

             texte2 <- generer_codes_couche_etiquette_unique("data_points", options_etiquettte_couche_actif(), "preview", 1 )

             texte <- paste(texte_graphique, texte2, sep = "+")

                          #La couche de texte

             graphique= eval(parse(text = texte)) +
                          eval(parse(text = theme_graphique))

             graphique

        })




        ###Suivi des modifications sur les caractéristiques de base d'une couche d'étiquettes################
        observeEvent(input$select_colonne_etiquette,{
              req(input$select_colonne_etiquette)

              #copie des option d'étiquettes
              opions_etiquettes=options_etiquettte_couche_actif()

              opions_etiquettes$colonne <- input$select_colonne_etiquette

              colonne_etiquette_actif(input$select_colonne_etiquette)
              options_etiquettte_couche_actif(opions_etiquettes)

        })


        ###modification de la police###################
        observeEvent(input$select_police_etiquette, {
              req(input$select_police_etiquette)

              #copie des option d'étiquettes
              opions_etiquettes=options_etiquettte_couche_actif()

              opions_etiquettes$police <- input$select_police_etiquette

              police_etiquette_actif(input$select_police_etiquette)
              options_etiquettte_couche_actif(opions_etiquettes)
              updateSelectInput(session, "select_police_etiquette", choices = liste_polices, selected = input$select_police_etiquette )


              #on actualise également la liste des styles liées à la police choisie
              styles_police = system_fonts() %>% filter(family==input$select_police_etiquette) %>% select(style) %>% pull()

              updateSelectInput(session, "select_style_police_etiquette", choices = styles_police )

        })

        ###modification du style de police############
        observeEvent(input$select_style_police_etiquette,{
          req(input$select_style_police_etiquette)

          #copie des option d'étiquettes
          opions_etiquettes=options_etiquettte_couche_actif()

          opions_etiquettes$style <- input$select_style_police_etiquette

          style_police_etiquette_actif(input$select_style_police_etiquette)
          options_etiquettte_couche_actif(opions_etiquettes)

          #on actualise également la liste des styles liées à la police choisie
          styles_police = system_fonts() %>% filter(family==input$select_police_etiquette) %>% select(style) %>% pull()

          updateSelectInput(session, "select_style_police_etiquette", choices = styles_police, selected = input$select_style_police_etiquette )

        })


        ###Modification de la taille##########
        observeEvent(input$taille_police_etiquette,{
          req(input$taille_police_etiquette)

          opions_etiquettes=options_etiquettte_couche_actif()

          opions_etiquettes$taille <- input$taille_police_etiquette


          options_etiquettte_couche_actif(opions_etiquettes)

          updateNumericInput(session,"taille_police_etiquette", value = input$taille_police_etiquette )

          taille_police_etiquette_actif(input$taille_police_etiquette)

        })


        #####Modification de la couleur############
        observeEvent(input$couleur_police_etiquette, {
          req(input$couleur_police_etiquette)
          isolate({
            opions_etiquettes=options_etiquettte_couche_actif()

            opions_etiquettes$couleur <- input$couleur_police_etiquette

            updateColourInput(session,"couleur_police_etiquette", value = input$couleur_police_etiquette )

            options_etiquettte_couche_actif(opions_etiquettes)

            couleur_police_etiquette_actif(input$couleur_police_etiquette)
          })



        })

        ###Mdification sur le type detiqette#########
        observeEvent(input$select_type_etiquette, {
          req(input$select_type_etiquette)

          opions_etiquettes=options_etiquettte_couche_actif()

          opions_etiquettes$type <- input$select_type_etiquette
          type_etiquette_actif(input$select_type_etiquette)
          options_etiquettte_couche_actif(opions_etiquettes)

        })



        ###Appliquer les modifications faites sur les textes des etiquettes#############
        observeEvent(input$bouton_appliquer_etiquette_couche,{
          req(input$bouton_appliquer_etiquette_couche)

          copie_couche=liste_couches()

          copie_couche[[name_couche_actif()]]$options_etiquette <- options_etiquettte_couche_actif()

          liste_couches(copie_couche)

        })


        ###Bouton fermer de la boite de dialogue#############
        observeEvent(input$bouton_ok_etiquette_couche, {
          req(input$bouton_ok_etiquette_couche)

          removeModal()

        })


        ###Suin des modifications sur les options du tampon#####################


























        #Bou\



        #observeEvent(input$infos_color_symboble_unique, {
          #couleur_remplissage_symbole_actif(input$infos_color_symboble_unique$color_symboble_unique_js)
          #updateTextInput(session, "select_couleur_symbole", value = input$infos_color_symboble_unique$color_symboble_unique_js )
        #})



}#fin serveur
