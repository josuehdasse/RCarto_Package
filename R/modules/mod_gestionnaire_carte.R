library(shiny)

#Ce module permet d’interagir avec les objets graphiques de la carte produite

gestionnaire_objet_carte_ui <- function(id){
  ns<- NS(id)

  tagList(
    p("Liste des objets"),
    fluidRow( #liste des objets de la carte
        uiOutput(ns("liste_objets_carte_ui"))
    ),

    fluidRow(class="details_zone_travail",#détails ICI
      uiOutput(ns("details_objets_carte_ui"))
    ),

    #on injecte les names spaces du module dans Javascript ici
    tagList(
      tags$script(HTML(sprintf(
        "
        window.gestion_objets_carteNS='%s';//Stocke le namespace dans une variabel globale",
        ns("")#ns("") renvoie l'id du module courant
      )))
    )

  )


}




gestionnaire_objet_carte_server <- function(input, output, session, liste_objets_mise_en_page ) {
  ns<- session$ns


  name_objet_carte_select_actif <- reactiveVal(NULL)


  #Paramètres généraux de position des sobjets du composeur###############
  type_objet_actif <- reactiveVal(NULL)
  xmin_objet_actif <- reactiveVal(NULL)
  xmax_objet_actif <- reactiveVal(NULL)
  ymin_objet_actif <- reactiveVal(NULL)
  ymax_objet_actif <- reactiveVal(NULL)


  #DOnnes d'emprise
  emprise_xmin_objet_actif <- reactiveVal(NULL)
  emprise_xmax_objet_actif <- reactiveVal(NULL)
  emprise_ymin_objet_actif <- reactiveVal(NULL)
  emprise_ymax_objet_actif <- reactiveVal(NULL)


  #Paramètres des objets de type "carte"###########################################################

  ##Cadres et grilles#################################################
  statut_cadre_carte_actif <- reactiveVal(FALSE)
  statut_grille_carte_actif <- reactiveVal(FALSE)

  #Gestion des apparences du cadre de la carte
  PanelborderColor_actif <- reactiveVal("#000000")
  PanelBackground_actif <- reactiveVal("#ffffff")
  PanelborderSize_actif <- reactiveVal(1)
  PanelLinetype_actif <- reactiveVal("solid")


  #Liste des dobjets ###############################################
  ##la liste réactive des objets du composeur #############################################################################
  listeObjetsUI <- reactive({

      listeObjets<- lapply(liste_objets_mise_en_page(), function(i){
        if(i$visible){
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
                onclick="details_objets_carte(this.id)",
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
                onclick="details_objets_carte(this.id)",
                i$label)
            )
            #i$label
          )#fin li
        }
      })
  })


  ##Affichage de la liste des objets du composuer ###############################################
  output$liste_objets_carte_ui <- renderUI({
    req(length(liste_objets_mise_en_page())>=1)

    #print(liste_objets_mise_en_page())

    #on renvoie l'objet réactif
    tags$ul(
      listeObjetsUI()
    )

  })



  #Récupération de l'objet sur lequel on a cliqué et affichage des détails #######
  statut_fleche_nord_actif <- reactiveVal(NULL)
  statut_legende_actif <- reactiveVal(NULL)
  statut_echelle_carte_actif <- reactiveVal(FALSE)


  observeEvent(input$name_objet_carte_select, {
    req(input$name_objet_carte_select)

    data_name_objet<- fromJSON(input$name_objet_carte_select)
    name_objet_select <-data_name_objet$name
    name_objet_carte_select_actif(name_objet_select)

    copie_liste_objets=liste_objets_mise_en_page()

    #recupération des caractéristiques de l'objet
    type_objet<- copie_liste_objets[[name_objet_select]]$type         #eval(parse(text =  paste("copie_liste_objets", name_objet_select, "type", sep = "$" )   ))
    type_objet_actif(type_objet)
    xmin_objet<- copie_liste_objets[[name_objet_select]]$positions$xmin #eval(parse(text =  paste("copie_liste_objets", name_objet_select, "xmin", sep = "$" )   ))
    xmin_objet_actif(xmin_objet)
    xmax_objet<- copie_liste_objets[[name_objet_select]]$positions$xmax #eval(parse(text =  paste("copie_liste_objets", name_objet_select, "xmax", sep = "$" )   ))
    xmax_objet_actif(xmax_objet)
    ymin_objet<- copie_liste_objets[[name_objet_select]]$positions$ymin# eval(parse(text =  paste("copie_liste_objets", name_objet_select, "ymin", sep = "$" )   ))
    ymin_objet_actif(ymin_objet)
    ymax_objet<-copie_liste_objets[[name_objet_select]]$positions$ymax# eval(parse(text =  paste("copie_liste_objets", name_objet_select, "ymax", sep = "$" )   ))
    ymax_objet_actif(ymax_objet)


    #Les donnes de l'emprise de l'objet (carte)
    emprise_xmin_objet<- copie_liste_objets[[name_objet_select]]$emprise_carte$xmin #eval(parse(text =  paste("copie_liste_objets", name_objet_select, "xmin", sep = "$" )   ))
    emprise_xmin_objet_actif(emprise_xmin_objet)
    emprise_xmax_objet<- copie_liste_objets[[name_objet_select]]$emprise_carte$xmax #eval(parse(text =  paste("copie_liste_objets", name_objet_select, "xmax", sep = "$" )   ))
    emprise_xmax_objet_actif(emprise_xmax_objet)
    emprise_ymin_objet<- copie_liste_objets[[name_objet_select]]$emprise_carte$ymin# eval(parse(text =  paste("copie_liste_objets", name_objet_select, "ymin", sep = "$" )   ))
    emprise_ymin_objet_actif(emprise_ymin_objet)
    emprise_ymax_objet<-copie_liste_objets[[name_objet_select]]$emprise_carte$ymax# eval(parse(text =  paste("copie_liste_objets", name_objet_select, "ymax", sep = "$" )   ))
    emprise_ymax_objet_actif(emprise_ymax_objet)



    #Etat de l'activation du cadre
    statut_cadre_carte<- copie_liste_objets[[name_objet_select]]$statut_cadre
    statut_cadre_carte_actif(statut_cadre_carte)

    #Etat de l'activation des grilles
    statut_grille_carte<- copie_liste_objets[[name_objet_select]]$statut_grille
    statut_grille_carte_actif(statut_grille_carte)

    #les propriétés de la carte (néecessaires)
    statut_fleche_nord<- copie_liste_objets[[name_objet_select]]$statut_fleche_nord
    statut_fleche_nord_actif(statut_fleche_nord)

    statut_legende<- copie_liste_objets[[name_objet_select]]$statut_legende
    statut_legende_actif(statut_legende)

    #l’échelle
    statut_echelle_carte <- copie_liste_objets[[name_objet_select]]$statut_echelle
    statut_echelle_carte_actif(statut_echelle_carte)


    ##Afficage des otptions de paramétrage (détails) d'un objet du graphique#############################
    output$details_objets_carte_ui <- renderUI({
      req(name_objet_carte_select_actif())

      switch (type_objet_actif(),
              "carte" = { ###Affichage des déetails des objets de type "carte"######################
                tagList(
                  fluidRow(class="form-group ligne_desc_obj",
                           column(width = 6,
                                  numericInput(ns("select_xmin"), label = "Min X", width = "100px",  min = - 10, max =NA , step = 0.5,  value =  xmin_objet_actif() )
                           ),
                           column(width = 6,
                                  numericInput(ns("select_xmax"), label = "Max X",  width = "100px", min = - 10 , max =NA ,step = 0.5, value =  xmax_objet_actif() )
                           )
                  ),
                  fluidRow(class="form-group ligne_desc_obj",
                           column(width = 6,
                                  numericInput(ns("select_ymin"), label = "Min Y", width = "100px", min = -10, max =NA ,step = 0.5, value =  ymin_objet_actif() )
                           ),
                           column(width = 6,
                                  numericInput(ns("select_ymax"), label = "Max Y", width = "100px", min = -10, max =NA ,step = 0.5, value =  ymax_objet_actif() )
                           )
                  ),
                  p("Données d'emprise de la carte :"),
                  fluidRow(class="form-group ligne_desc_obj",
                           column(width = 6,
                                  numericInput(ns("emprise_select_xmin"), label = "Min X", width = "100px",  min = - 10, max =NA , step = 0.5,  value =  emprise_xmin_objet_actif() )
                           ),
                           column(width = 6,
                                  numericInput(ns("emprise_select_xmax"), label = "Max X",  width = "100px", min = - 10 , max =NA ,step = 0.5, value =  emprise_xmax_objet_actif() )
                           )
                  ),
                  fluidRow(class="form-group",
                           column(width = 6,
                                  numericInput(ns("emprise_select_ymin"), label = "Min Y", width = "100px", min = -10, max =NA ,step = 0.5, value =  emprise_ymin_objet_actif() )
                           ),
                           column(width = 6,
                                  numericInput(ns("emprise_select_ymax"), label = "Max Y", width = "100px", min = -10, max =NA ,step = 0.5, value =  emprise_ymax_objet_actif() )
                           )
                  ),


                  hr(width="80%"),

                  fluidRow(class="form-group ligne_desc_obj",
                           checkboxInput(ns("select_statut_cadre_carte"), "Cadre", value = statut_cadre_carte_actif() )
                  ),

                  uiOutput(ns("cadre_carte_ui")),

                  hr(width="80%"),

                  fluidRow(class="form-group ligne_desc_obj",
                           checkboxInput(ns("select_statut_grille_carte"), "Grille", value = statut_grille_carte_actif() )
                  ),

                  uiOutput(ns("grille_carte_ui")),

                  fluidRow(class="form-group ligne_desc_obj",
                           checkboxInput(ns("select_statut_fleche_nord"), "Fleche Nord", value = statut_fleche_nord_actif() )
                  ),

                  uiOutput(ns("options_fleche_nord_carte")),

                  fluidRow(class="form-group ligne_desc_obj",
                           checkboxInput(ns("select_statut_echelle_carte"), "Echelle", value = statut_echelle_carte_actif() )
                  ),

                  uiOutput(ns("options_echelle_carte")),

                  fluidRow(class="form-group ligne_desc_obj",
                           checkboxInput(ns("select_statut_legende"), "Légende", value = statut_legende_actif() )
                  ),
                )

              }
      )

    })







  })


  #Suivi des modifications des parametres de position d'un objet ################################################

  ## paramètres de positinnement des objets de la comoistion graphique################################################
        observeEvent(input$select_xmin, {
          req(input$select_xmin)
          copie_liste_objets=liste_objets_mise_en_page()
         # xmin_objet_actif()
          #req( input$select_xmin)
          isolate({
            if(input$select_xmin != xmin_objet_actif()){
              copie_liste_objets[[name_objet_carte_select_actif()]]$positions$xmin <- input$select_xmin
              #updateNumericInput(session, "select_xmin", min = -10, max =NA ,step = 0.5, value = input$select_xmin )
              #xmin_objet_actif(input$select_xmin)
              liste_objets_mise_en_page(copie_liste_objets)
            }

          })


        }, ignoreInit = TRUE, ignoreNULL = TRUE)





        observeEvent(input$select_xmax, {
          req(input$select_xmax)
          copie_liste_objets=liste_objets_mise_en_page()


          copie_liste_objets[[name_objet_carte_select_actif()]]$positions$xmax <- input$select_xmax
          #updateNumericInput(session, "select_xmax", min = -10, max =NA ,step = 0.5, value = input$select_xmax )
          liste_objets_mise_en_page(copie_liste_objets)
        }, ignoreInit = TRUE, ignoreNULL = TRUE)


        observeEvent(input$select_ymin, {
          req(input$select_ymin)
          copie_liste_objets=liste_objets_mise_en_page()


          copie_liste_objets[[name_objet_carte_select_actif()]]$positions$ymin <- input$select_ymin
          #updateNumericInput(session, "select_ymin", min = -10, max =NA ,step = 0.5, value = input$select_ymin )
          liste_objets_mise_en_page(copie_liste_objets)
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        observeEvent(input$select_ymax, {
          req(input$select_ymax)
          copie_liste_objets=liste_objets_mise_en_page()


          copie_liste_objets[[name_objet_carte_select_actif()]]$positions$ymax <- input$select_ymax
          #updateNumericInput(session, "select_ymax", min = -10, max =NA ,step = 0.5, value = input$select_ymax )
          liste_objets_mise_en_page(copie_liste_objets)
        }, ignoreInit = TRUE, ignoreNULL = TRUE)



  ##Emprise de la carte################################
        observeEvent(input$emprise_select_xmin, {
          req(input$emprise_select_xmin)
          copie_liste_objets=liste_objets_mise_en_page()
          # xmin_objet_actif()
          #req( input$select_xmin)

              copie_liste_objets[[name_objet_carte_select_actif()]]$emprise_carte$xmin <- input$emprise_select_xmin
              #updateNumericInput(session, "select_xmin", min = -10, max =NA ,step = 0.5, value = input$select_xmin )
              #xmin_objet_actif(input$select_xmin)
              liste_objets_mise_en_page(copie_liste_objets)

        }, ignoreInit = TRUE, ignoreNULL = TRUE)


        observeEvent(input$emprise_select_ymin, {
          req(input$emprise_select_ymin)
          copie_liste_objets=liste_objets_mise_en_page()
          # xmin_objet_actif()
          #req( input$select_xmin)

          copie_liste_objets[[name_objet_carte_select_actif()]]$emprise_carte$ymin <- input$emprise_select_ymin
          #updateNumericInput(session, "select_xmin", min = -10, max =NA ,step = 0.5, value = input$select_xmin )
          #xmin_objet_actif(input$select_xmin)
          liste_objets_mise_en_page(copie_liste_objets)

        }, ignoreInit = TRUE, ignoreNULL = TRUE)


        observeEvent(input$emprise_select_xmax, {
          req(input$emprise_select_xmax)
          copie_liste_objets=liste_objets_mise_en_page()
          # xmin_objet_actif()
          #req( input$select_xmin)

          copie_liste_objets[[name_objet_carte_select_actif()]]$emprise_carte$xmax <- input$emprise_select_xmax
          #updateNumericInput(session, "select_xmin", min = -10, max =NA ,step = 0.5, value = input$select_xmin )
          #xmin_objet_actif(input$select_xmin)
          liste_objets_mise_en_page(copie_liste_objets)

        }, ignoreInit = TRUE, ignoreNULL = TRUE)



        observeEvent(input$emprise_select_ymax, {
          req(input$emprise_select_ymax)
          copie_liste_objets=liste_objets_mise_en_page()
          # xmin_objet_actif()
          #req( input$select_xmin)

          copie_liste_objets[[name_objet_carte_select_actif()]]$emprise_carte$ymax <- input$emprise_select_ymax
          #updateNumericInput(session, "select_xmin", min = -10, max =NA ,step = 0.5, value = input$select_xmin )
          #xmin_objet_actif(input$select_xmin)
          liste_objets_mise_en_page(copie_liste_objets)

        }, ignoreInit = TRUE, ignoreNULL = TRUE)


  ##Gestion spécifique aux objets de type carte#################################
  ###affichage des options du cadre d'un objet de type carte############################################

      ####Activation ou désactivation des options de cadre des objets de tyepe carte ##########
      observeEvent(input$select_statut_cadre_carte, {
        #req(input$select_statut_cadre_carte)

        copie_liste_objets=liste_objets_mise_en_page()
        copie_liste_objets[[name_objet_carte_select_actif()]]$statut_cadre <- input$select_statut_cadre_carte

        statut_cadre_carte_actif(input$select_statut_cadre_carte)
        liste_objets_mise_en_page(copie_liste_objets)

      })

  ####Options détailles paramètres des objets de type de cadre##########################
      output$cadre_carte_ui <- renderUI({
        req(statut_cadre_carte_actif())

        copie_liste_objets=liste_objets_mise_en_page()

        #recupération des caractéristiques de l'objet

        PanelborderColor<- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderColor
        #PanelborderColor_actif(PanelborderColor)

        PanelBackground <- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelBackground
        #PanelBackground_actif(PanelBackground)


        PanelborderSize<- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderSize
        #PanelborderSize_actif(PanelborderSize)


        PanelLinetype<- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelLinetype
        #PanelLinetype_actif(PanelLinetype)

        isolate({
          tagList(#début du cadre
            fluidRow(class="form-group",
              colourInput(ns("select_PanelborderColor"), label = "Couleur des traits ", value=PanelborderColor , allowTransparent = TRUE, palette = "square", closeOnClick = FALSE  )

            ),

            fluidRow(class="form-group ligne_desc_obj",
                column(width = 12,
                       numericInput(ns("select_PanelborderSize"), label = "Epaisseur du cadre", min = 0, max=NA, step = 0.01, value =  PanelborderSize )
                )

            ),
            #Le style de graphique
            fluidRow(class="form-group",
                            selectInput(ns("select_PanelLinetype"), label = "Style de trait ",
                                        choices = list("Ligne continue"="solid",
                                                       "Pas de ligne"="blank",
                                                       "Ligne en tiret"="a1",
                                                       "Ligne en pointillet"="31",
                                                       "Ligne en tiret-point"="dotdash",
                                                       "Ligne en tiret-point-point"="twodash",
                                                       "Tirets"="dashed"), selected = PanelLinetype, width = "80%" )


            ),

            fluidRow(class="form-group ligne_desc_obj",
                     colourInput(ns("select_PanelBackground"), label ="Fond de carte ", value=PanelBackground , allowTransparent = TRUE, palette = "square", closeOnClick = FALSE  )
            )


          )#Fin du taglist
        })


      })



  ####Suivi des modifications des parametres d'un objet de type carte ###############

  #####Modification sur les couleurs des bordures de la carte #####
      observeEvent(input$select_PanelBackground, {

        copie_liste_objets=liste_objets_mise_en_page()

        req(input$select_PanelBackground )

        isolate({

          copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelBackground <- input$select_PanelBackground

          #updateColourInput(session, "select_PanelBackground", value=input$select_PanelBackground)

          liste_objets_mise_en_page(copie_liste_objets)
        })

      }, ignoreInit = TRUE, ignoreNULL = TRUE)


  #####Modifcation sur les type de bordure de la carte###################

      observeEvent(input$select_PanelborderColor, {

        req(input$select_PanelborderColor )

          copie_liste_objets=liste_objets_mise_en_page()
          copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderColor <- input$select_PanelborderColor

          #updateColourInput(session, "select_PanelborderColor", value=input$select_PanelborderColor)

          liste_objets_mise_en_page(copie_liste_objets)



      }, ignoreInit = TRUE, ignoreNULL = TRUE)


  #####Modification sur l'épaissaeur des bordures de la carte #####
      observeEvent(input$select_PanelborderSize, {
        req(input$select_PanelborderSize)

        copie_liste_objets=liste_objets_mise_en_page()
        copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderSize <- input$select_PanelborderSize
        #updateNumericInput(session, "select_PanelborderSize", min = 0, max=NA, step = 0.5, value = input$select_PanelborderSize )
        #PanelborderSize_actif(input$select_PanelborderSize)

        liste_objets_mise_en_page(copie_liste_objets)


      },ignoreInit = TRUE, ignoreNULL = TRUE )


  #####Modification sur le type de ligne des bordures de la carte #####
      observeEvent(input$select_PanelLinetype, {

        req(input$select_PanelLinetype != PanelLinetype_actif() )

        copie_liste_objets=liste_objets_mise_en_page()
        copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelLinetype <- input$select_PanelLinetype
        #updateSelectInput(session, "select_PanelLinetype",  choices = list("Ligne continue"="solid",
                                                                          # "Pas de ligne"="blank",
                                                                           #"Ligne en tiret"="a1",
                                                                           #"Ligne en pointillet"="31",
                                                                           #"Ligne en tiret-point"="dotdash",
                                                                           #"Ligne en tiret-point-point"="twodash",
                                                                           #"Tirets"="dashed") , selected = input$select_PanelLinetype)
        #PanelLinetype_actif(input$select_PanelLinetype)
        liste_objets_mise_en_page(copie_liste_objets)
      },ignoreInit = TRUE, ignoreNULL = TRUE)


  ### Gestion des options de la grille d'un objet de type carte #####
      observeEvent(input$select_statut_grille_carte, {
        copie_liste_objets=liste_objets_mise_en_page()

        copie_liste_objets=liste_objets_mise_en_page()
        copie_liste_objets[[name_objet_carte_select_actif()]]$statut_grille <- input$select_statut_grille_carte
        #statut_grille_carte_actif(input$select_statut_grille_carte)
        liste_objets_mise_en_page(copie_liste_objets)


      },ignoreInit = TRUE, ignoreNULL = TRUE)


  ###### Options de controle de la grille #############

  #Les intervalles pour agestion des affiches des axes de coordonnées
  intervalleX_actif <- reactiveVal(1)
  intervalleY_actif <- reactiveVal(1)
  EspacementCadre_actif <- reactiveVal(0)


  ###### Ui des options de controle de la grille de la carte##########
  output$grille_carte_ui <- renderUI({
    req(statut_grille_carte_actif())

    copie_liste_objets=liste_objets_mise_en_page()
    intervalleX <- copie_liste_objets[[name_objet_carte_select_actif()]]$grille$intervalleX
    intervalleX_actif (intervalleX)

    intervalleY <- copie_liste_objets[[name_objet_carte_select_actif()]]$grille$intervalleY
    intervalleY_actif (intervalleY)

    EspacementCadre <- copie_liste_objets[[name_objet_carte_select_actif()]]$grille$EspacementCadre
    EspacementCadre_actif(EspacementCadre)

    tagList(

      fluidRow(class="form-group ligne_desc_obj",
               column(width = 4,
                      tags$label("Intervalle X")
               ),
               column(width = 8,
                      numericInput(ns("select_intervalleX"), label = NULL, min = 1, max=NA, width = "100px", value =  intervalleX_actif() )
               )
      ),

      fluidRow(class="form-group ligne_desc_obj",
               column(width = 4,
                      tags$label("Intervalle Y")
               ),
               column(width = 8,
                      numericInput(ns("select_intervalleY"), label = NULL, min = 1, max=NA, width = "100px", value =  intervalleY_actif() )
               )
      ),

      fluidRow(class="form-group ligne_desc_obj",
               column(width = 4,
                      tags$label("Espacement du cadre")
               ),
               column(width = 8,
                      numericInput(ns("select_EspacementCadre"), label = NULL, min = 1, max=NA, width = "100px", value =  EspacementCadre_actif() )
               )
      )

    )



  })



  ###### gestion des modifications des paramètres de la grille ####
  observeEvent(input$select_intervalleX, {
      req(input$select_intervalleX)
      copie_liste_objets <- liste_objets_mise_en_page()
      copie_liste_objets[[name_objet_carte_select_actif()]]$grille$intervalleX <- input$select_intervalleX
      liste_objets_mise_en_page(copie_liste_objets)

  })

  observeEvent(input$select_intervalleY, {
    req(input$select_intervalleY)
    copie_liste_objets <- liste_objets_mise_en_page()
    copie_liste_objets[[name_objet_carte_select_actif()]]$grille$intervalleY <- input$select_intervalleY
    liste_objets_mise_en_page(copie_liste_objets)
  })

  observeEvent(input$select_EspacementCadre, {
    req(input$select_EspacementCadre)
    copie_liste_objets <- liste_objets_mise_en_page()
    copie_liste_objets[[name_objet_carte_select_actif()]]$grille$EspacementCadre <- input$select_EspacementCadre
    liste_objets_mise_en_page(copie_liste_objets)
  })





  #Gestion de la fleche NOrd########################################
  width_fleche_nord_actif <- reactiveVal(NULL)
  height_fleche_nord_actif <- reactiveVal(NULL)

  observeEvent(input$select_statut_fleche_nord, {
    copie_liste_objets <- liste_objets_mise_en_page()


    copie_liste_objets[[name_objet_carte_select_actif()]]$statut_fleche_nord <- input$select_statut_fleche_nord
    liste_objets_mise_en_page(copie_liste_objets)

    #print(copie_liste_objets)
    statut_fleche_nord_actif(input$select_statut_fleche_nord)
  })


  output$options_fleche_nord_carte <- renderUI({
    req(input$select_statut_fleche_nord)


    copie_liste_objets <- liste_objets_mise_en_page()
    width_fleche_nord <- copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$width
    height_fleche_nord <- copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$height
    select_location_fleche_nord <- copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$select_location_fleche_nord
    pad_x_fleche_nord <- copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$pad_x
    pad_y_fleche_nord <- copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$pad_y
    select_style_fleche_nord <- copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$style

    tagList(
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("width_fleche_nord"), label = "Largeur", min = 0, max=NA, step = 0.01, value =  width_fleche_nord )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("height_fleche_nord"), label = "Hauteur", min = 0, max=NA, step = 0.01, value =  height_fleche_nord )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
        selectInput(ns("select_location_fleche_nord"), label = "Location",
                    choices = list("Haut gauche"="tl",#top left
                                   "Haut droit"="tr",#Top right
                                   "Bas gauche"="bl",#Bottom left
                                   "Bottom right"="br"#Bottom right
                                   ), selected = select_location_fleche_nord, width = "80%" )
      ),
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("pad_x_fleche_nord"), label = "Decalage X", min = 0, max=NA, step = 0.01, value =  pad_x_fleche_nord )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("pad_y_fleche_nord"), label = "Decalage Y", min = 0, max=NA, step = 0.01, value =  pad_y_fleche_nord )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
               selectInput(ns("select_style_fleche_nord"), label = "Style",
                           choices = c("north_arrow_fancy_orienteering", "north_arrow_orienteering", "north_arrow_nautical", "north_arrow_minimal"), selected = select_style_fleche_nord, width = "80%" )
      )



    )



  })


  ##Suivi des modifications sur les options de modification des options de la fleche nord#############################

  ###Largeur de la fleche Nord#################
  observeEvent(input$width_fleche_nord,{
    print(name_objet_carte_select_actif())
    req(input$width_fleche_nord)

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$width <- input$width_fleche_nord

    liste_objets_mise_en_page(copie_liste_objets)

  })


  ###Hauteur de la fleche Nord######################
  observeEvent(input$height_fleche_nord,{
    req(input$height_fleche_nord)

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$height <- input$height_fleche_nord
    liste_objets_mise_en_page(copie_liste_objets)

  })


  ###Location########################
  observeEvent(input$select_location_fleche_nord,{
    req(input$select_location_fleche_nord)

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$location <- input$select_location_fleche_nord
    liste_objets_mise_en_page(copie_liste_objets)

  })




  ###PAdX########################
  observeEvent(input$pad_x_fleche_nord,{
    req(input$pad_x_fleche_nord)

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$pad_x <- input$pad_x_fleche_nord
    liste_objets_mise_en_page(copie_liste_objets)

  })



  ###PAdY########################
  observeEvent(input$pad_y_fleche_nord,{
    req(input$pad_y_fleche_nord)

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$pad_y <- input$pad_y_fleche_nord
    liste_objets_mise_en_page(copie_liste_objets)

  })


  ###Style########################
  observeEvent(input$select_style_fleche_nord,{
    req(input$select_style_fleche_nord)

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$fleche_nord$style <- input$select_style_fleche_nord
    liste_objets_mise_en_page(copie_liste_objets)

  })





  #Gestion de l'échelle d'une carte##########################
  observeEvent(input$select_statut_echelle_carte,{

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$statut_echelle <- input$select_statut_echelle_carte
    statut_echelle_carte_actif(input$select_statut_echelle_carte)

    liste_objets_mise_en_page(copie_liste_objets)

  })


  ##On affiche les options de gestion de l'échelle d ela carte
 output$options_echelle_carte <- renderUI({
    req(input$select_statut_echelle_carte)#seulement si c'est coché


   copie_liste_objets <- liste_objets_mise_en_page()

   width_hint_scale <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$width_hint
   height_echelle <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$height
   location_echelle <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$location
   pad_x_echelle  <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$pad_x
   pad_y_echelle  <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$pad_y
   style_echelle <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$style
   unit_echelle <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$unit
   text_cex_echelle <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$text_cex
   text_col_echelle <- copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$text_col

    tagList(
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("width_hint_scale"), label = "Largeur", min = 0, max=NA, step = 0.01, value =  width_hint_scale )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("height_echelle"), label = "Hauteur", min = 0, max=NA, step = 0.01, value =  height_echelle )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
               selectInput(ns("select_location_echelle"), label = "Location",
                           choices = list("Haut gauche"="tl",#top left
                                          "Haut droit"="tr",#Top right
                                          "Bas gauche"="bl",#Bottom left
                                          "Bottom right"="br"#Bottom right
                           ), selected = location_echelle, width = "80%" )
      ),
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("pad_x_echelle"), label = "Decalage X", min = 0, max=NA, step = 0.01, value =  pad_x_echelle )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("pad_y_echelle"), label = "Decalage Y", min = 0, max=NA, step = 0.01, value =  pad_y_echelle )
               )

      ),
      fluidRow(class="form-group ligne_desc_obj",
               selectInput(ns("select_style_echelle"), label = "Style",
                           choices = c("ticks", "bar"), selected = style_echelle, width = "80%" )
      ),
      fluidRow(class="form-group ligne_desc_obj",
               selectInput(ns("select_unit_echelle"), label = "Unité",
                           choices = c("km", "m","mi", "ft"), selected = unit_echelle, width = "80%" )
      ),
      fluidRow(class="form-group ligne_desc_obj",
               column(width = 12,
                      numericInput(ns("text_cex_echelle"), label = "Taille texte", min = 0, max=NA, step = 0.01, value =text_cex_echelle )
               )
      ),
      fluidRow(class="form-group ligne_desc_obj",
               colourInput(ns("text_col_echelle"), label = "Couleur du texte ", value=text_col_echelle, allowTransparent = TRUE, palette = "square", closeOnClick = FALSE  )

      ),

    )



  })




  ##Suivi des modifications sur l'echelle de la carte####################

 ###La largeur relative
  observeEvent(input$width_hint_scale,{
    req(!is.na(input$width_hint_scale))
    req(input$width_hint_scale >=0.01)

    copie_liste_objets <- liste_objets_mise_en_page()

    copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$width_hint <- input$width_hint_scale

    liste_objets_mise_en_page(copie_liste_objets)

  })


 ###La hauteur de l'echelle
 observeEvent(input$height_echelle,{
   req(input$height_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$height <- input$height_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })



 ###La localisation / emplacement
 observeEvent(input$select_location_echelle,{
   req(input$select_location_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$location <- input$select_location_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })




 ###Decalage en X
 observeEvent(input$pad_x_echelle,{
   req(input$pad_x_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$pad_x <- input$pad_x_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })



 ### Décalage en Y
 observeEvent(input$pad_y_echelle,{
   req(input$pad_y_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$pad_y <- input$pad_y_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })


 ### le stylend el'echelle
 observeEvent(input$select_style_echelle,{
   req(input$select_style_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$style <- input$select_style_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })


 #Unité de l'echelle de la carte
 observeEvent(input$select_unit_echelle,{
   req(input$select_unit_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$unit <- input$select_unit_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })


 #La taille du texte de l'echelle de la carte
 observeEvent(input$text_cex_echelle,{
   req(input$text_cex_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$text_cex <- input$text_cex_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })


 ### La coucleur dz texte
 observeEvent(input$text_col_echelle,{
   req(input$text_col_echelle)

   copie_liste_objets <- liste_objets_mise_en_page()

   copie_liste_objets[[name_objet_carte_select_actif()]]$echelle$text_col <- input$text_col_echelle
   liste_objets_mise_en_page(copie_liste_objets)

 })























  #fin de la partie serveur
  }
