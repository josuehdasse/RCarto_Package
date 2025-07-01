library(shiny)

#Ce module permet d'interacgir avec les objets graphiques de la carte produite


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

  type_objet_actif <- reactiveVal(NULL)
  xmin_objet_actif <- reactiveVal(NULL)
  xmax_objet_actif <- reactiveVal(NULL)
  ymin_objet_actif <- reactiveVal(NULL)
  ymax_objet_actif <- reactiveVal(NULL)

  #Cadres et grilles
  statut_cadre_carte_actif <- reactiveVal(FALSE)
  statut_grille_carte_actif <- reactiveVal(FALSE)

  #Gestion des apparences du cadre de la carte
  PanelborderColor_actif <- reactiveVal("#000000")
  PanelBackground_actif <- reactiveVal("#ffffff")
  PanelborderSize_actif <- reactiveVal(1)
  PanelLinetype_actif <- reactiveVal("solid")
  #la liste des objets de la carte
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
                    onclick="alert(this.id)"
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
                    onclick="alert(this.id)"
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


  output$liste_objets_carte_ui <- renderUI({
    req(length(liste_objets_mise_en_page())>=1)

    #print(liste_objets_mise_en_page())

    #on renvoie l'objet réactif
    tags$ul(
      listeObjetsUI()
    )



  })


  #Récupération de l'objet sur lequel on a cliqué #######
  observeEvent(input$name_objet_carte_select, {
    req(input$name_objet_carte_select)

    data_name_objet<- fromJSON(input$name_objet_carte_select)
    name_objet_select <-data_name_objet$name
    name_objet_carte_select_actif(name_objet_select)

    copie_liste_objets=liste_objets_mise_en_page()

    #recupération des caractéristiques de l'objet
    type_objet<- eval(parse(text =  paste("copie_liste_objets", name_objet_select, "type", sep = "$" )   ))
    type_objet_actif(type_objet)
    xmin_objet<- eval(parse(text =  paste("copie_liste_objets", name_objet_select, "xmin", sep = "$" )   ))
    xmin_objet_actif(xmin_objet)
    xmax_objet<- eval(parse(text =  paste("copie_liste_objets", name_objet_select, "xmax", sep = "$" )   ))
    xmax_objet_actif(xmax_objet)
    ymin_objet<- eval(parse(text =  paste("copie_liste_objets", name_objet_select, "ymin", sep = "$" )   ))
    ymin_objet_actif(ymin_objet)
    ymax_objet<- eval(parse(text =  paste("copie_liste_objets", name_objet_select, "ymax", sep = "$" )   ))
    ymax_objet_actif(ymax_objet)

    #Etat de l'activation du cadre
    statut_cadre_carte<- copie_liste_objets[[name_objet_select]]$statut_cadre
    statut_cadre_carte_actif(statut_cadre_carte)

    #Etat de l'activation des grilles
    statut_grille_carte<- copie_liste_objets[[name_objet_select]]$statut_grille
    statut_grille_carte_actif(statut_grille_carte)


  })

  #Afficage des otptions de paratrage d'un objet du graphique#############################
  output$details_objets_carte_ui <- renderUI({
    req(name_objet_carte_select_actif())

    switch (type_objet_actif(),
      "carte" = {
        tagList(
          fluidRow(class="form-group",
                   column(width = 6,
                          numericInput(ns("select_xmin"), label = "Min X", width = "100px",  min = - 10, max =NA , step = 0.5,  value =  xmin_objet_actif() )
                   ),
                   column(width = 6,
                          numericInput(ns("select_xmax"), label = "Max X",  width = "100px", min = - 10 , max =NA ,step = 0.5, value =  xmax_objet_actif() )
                   )
          ),
          fluidRow(class="form-group",
                   column(width = 6,
                          numericInput(ns("select_ymin"), label = "Min Y", width = "100px", min = -10, max =NA ,step = 0.5, value =  ymin_objet_actif() )
                   ),
                   column(width = 6,
                          numericInput(ns("select_ymax"), label = "Max Y", width = "100px", min = -10, max =NA ,step = 0.5, value =  ymax_objet_actif() )
                   )
          ),


          hr(width="80%"),

          fluidRow(class="form-group",
                   checkboxInput(ns("select_statut_cadre_carte"), "Cadre", value = statut_cadre_carte_actif() )
          ),

          uiOutput(ns("cadre_carte_ui")),

          hr(width="80%"),

          fluidRow(class="form-group",
                   checkboxInput(ns("select_statut_grille_carte"), "Grille", value = statut_grille_carte_actif() )
          ),

          uiOutput(ns("grille_carte_ui"))
        )

      }
    )

  })




  #Suivi des modifications des parametres de position d'un objet ################################################
  observeEvent(input$select_xmin, {
    req(input$select_xmin)
    copie_liste_objets=liste_objets_mise_en_page()
   # xmin_objet_actif()
    #req( input$select_xmin)
    isolate({
      if(input$select_xmin != xmin_objet_actif()){
        copie_liste_objets[[name_objet_carte_select_actif()]]$xmin <- input$select_xmin
        updateNumericInput(session, "select_xmin", min = -10, max =NA ,step = 0.5, value = input$select_xmin )
        xmin_objet_actif(input$select_xmin)
        liste_objets_mise_en_page(copie_liste_objets)
      }

    })


  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  observeEvent(input$select_xmax, {
    req(input$select_xmax)
    copie_liste_objets=liste_objets_mise_en_page()


    copie_liste_objets[[name_objet_carte_select_actif()]]$xmax <- input$select_xmax
    updateNumericInput(session, "select_xmax", min = -10, max =NA ,step = 0.5, value = input$select_xmax )
    liste_objets_mise_en_page(copie_liste_objets)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  observeEvent(input$select_ymin, {
    req(input$select_ymin)
    copie_liste_objets=liste_objets_mise_en_page()


    copie_liste_objets[[name_objet_carte_select_actif()]]$ymin <- input$select_ymin
    updateNumericInput(session, "select_ymin", min = -10, max =NA ,step = 0.5, value = input$select_ymin )
    liste_objets_mise_en_page(copie_liste_objets)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$select_ymax, {
    req(input$select_ymax)
    copie_liste_objets=liste_objets_mise_en_page()


    copie_liste_objets[[name_objet_carte_select_actif()]]$ymax <- input$select_ymax
    updateNumericInput(session, "select_ymax", min = -10, max =NA ,step = 0.5, value = input$select_ymax )
    liste_objets_mise_en_page(copie_liste_objets)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)





  #GEstion spécifique aux objets de type carte#################################
  ##affichage des options du cadre d'un objet de type carte############################################

  ###Activation ou désactivation des options de cadre des objets de tyepe carte ##########
  observeEvent(input$select_statut_cadre_carte, {
    #req(input$select_statut_cadre_carte)

    copie_liste_objets=liste_objets_mise_en_page()
    copie_liste_objets[[name_objet_carte_select_actif()]]$statut_cadre <- input$select_statut_cadre_carte

    statut_cadre_carte_actif(input$select_statut_cadre_carte)
    liste_objets_mise_en_page(copie_liste_objets)


  })

  ###Options détailles paramètres des objets de type de cadre##########################
  output$cadre_carte_ui <- renderUI({
    req(statut_cadre_carte_actif())

    copie_liste_objets=liste_objets_mise_en_page()

    #recupération des caractéristiques de l'objet

    PanelborderColor<- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderColor
    PanelborderColor_actif(PanelborderColor)

    PanelBackground <- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelBackground
    PanelBackground_actif(PanelBackground)


    PanelborderSize<- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderSize
    PanelborderSize_actif(PanelborderSize)


    PanelLinetype<- copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelLinetype
    PanelLinetype_actif(PanelLinetype)

    isolate({
      tagList(#début du cadre
        fluidRow(class="form-group",
                 column(width = 4,    tags$label("Couleur des traits ")     ),
                 column(width = 8, colourInput(ns("select_PanelborderColor"), label = NULL, value=PanelborderColor_actif() , allowTransparent = TRUE, palette = "square", closeOnClick = FALSE  )
                 )
        ),

        fluidRow(class="form-group",
                 column(width = 4,
                        tags$label("Epaisseur du cadre")
                 ),
                 column(width = 8,
                        numericInput(ns("select_PanelborderSize"), label = NULL, min = 0, max=NA, width = "100px", value =  PanelborderSize_actif() )
                 )
        ),
        #Le style de graphique
        fluidRow(class="form-group",
                 column(width = 4,
                        tags$label("Style de trait ")
                 ),
                 column(width = 8,
                        selectInput(ns("select_PanelLinetype"), label = NULL,
                                    choices = list("Ligne continue"="solid",
                                                   "Pas de ligne"="blank",
                                                   "Ligne en tiret"="a1",
                                                   "Ligne en pointillet"="31",
                                                   "Ligne en tiret-point"="dotdash",
                                                   "Ligne en tiret-point-point"="twodash",
                                                   "Tirets"="dashed"), selected = PanelLinetype_actif(), width = "80%" )
                 )


        ),

        fluidRow(class="form-group",
                 column(width = 4,    tags$label("Fond de carte ")     ),
                 column(width = 8, colourInput(ns("select_PanelBackground"), label = NULL, value=PanelBackground_actif() , allowTransparent = TRUE, palette = "square", closeOnClick = FALSE  )
                 )
        )


      )#Fin du taglist
    })


  })



  ###Suivi des modifications des parametres d'un objet de type carte ###############

  ####Modification sur les couleurs des bordures de la carte #####
  observeEvent(input$select_PanelBackground, {

    req(input$select_PanelBackground )

    copie_liste_objets=liste_objets_mise_en_page()
    copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelBackground <- input$select_PanelBackground

    updateColourInput(session, "select_PanelBackground", value=input$select_PanelBackground)

    liste_objets_mise_en_page(copie_liste_objets)



  }, ignoreInit = TRUE, ignoreNULL = TRUE)




  observeEvent(input$select_PanelborderColor, {

    req(input$select_PanelborderColor )

      copie_liste_objets=liste_objets_mise_en_page()
      copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderColor <- input$select_PanelborderColor

      updateColourInput(session, "select_PanelborderColor", value=input$select_PanelborderColor)

      liste_objets_mise_en_page(copie_liste_objets)



  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  ####Modification sur l'épaissaeur des bordures de la carte #####
  observeEvent(input$select_PanelborderSize, {

    req(input$select_PanelborderSize != PanelborderSize_actif() )

    copie_liste_objets=liste_objets_mise_en_page()
    copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelborderSize <- input$select_PanelborderSize
    updateNumericInput(session, "select_PanelborderSize", min = 0, max=NA, step = 0.5, value = input$select_PanelborderSize )
    #PanelborderSize_actif(input$select_PanelborderSize)
    liste_objets_mise_en_page(copie_liste_objets)
  })


  ####Modification sur le type de ligne des bordures de la carte #####
  observeEvent(input$select_PanelLinetype, {

    req(input$select_PanelLinetype != PanelLinetype_actif() )

    copie_liste_objets=liste_objets_mise_en_page()
    copie_liste_objets[[name_objet_carte_select_actif()]]$cadre$PanelLinetype <- input$select_PanelLinetype
    updateSelectInput(session, "select_PanelLinetype",  choices = list("Ligne continue"="solid",
                                                                       "Pas de ligne"="blank",
                                                                       "Ligne en tiret"="a1",
                                                                       "Ligne en pointillet"="31",
                                                                       "Ligne en tiret-point"="dotdash",
                                                                       "Ligne en tiret-point-point"="twodash",
                                                                       "Tirets"="dashed") , selected = input$select_PanelLinetype)
    #PanelLinetype_actif(input$select_PanelLinetype)
    liste_objets_mise_en_page(copie_liste_objets)
  })


  ## Gestion des options de la grille d'un objet de type carte #####
  observeEvent(input$select_statut_grille_carte, {
    copie_liste_objets=liste_objets_mise_en_page()

    copie_liste_objets=liste_objets_mise_en_page()
    copie_liste_objets[[name_objet_carte_select_actif()]]$statut_grille <- input$select_statut_grille_carte
    statut_grille_carte_actif(input$select_statut_grille_carte)
    liste_objets_mise_en_page(copie_liste_objets)


  })


  #### Options de controle de la grille #############

  #Les intervalles pour agestion des affiches des axes de coordonnées
  intervalleX_actif <- reactiveVal(1)
  intervalleY_actif <- reactiveVal(1)
  EspacementCadre_actif <- reactiveVal(0)


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

      fluidRow(class="form-group",
               column(width = 4,
                      tags$label("Intervalle X")
               ),
               column(width = 8,
                      numericInput(ns("select_intervalleX"), label = NULL, min = 1, max=NA, width = "100px", value =  intervalleX_actif() )
               )
      ),

      fluidRow(class="form-group",
               column(width = 4,
                      tags$label("Intervalle Y")
               ),
               column(width = 8,
                      numericInput(ns("select_intervalleY"), label = NULL, min = 1, max=NA, width = "100px", value =  intervalleY_actif() )
               )
      ),

      fluidRow(class="form-group",
               column(width = 4,
                      tags$label("Espacement du cadre")
               ),
               column(width = 8,
                      numericInput(ns("select_EspacementCadre"), label = NULL, min = 1, max=NA, width = "100px", value =  EspacementCadre_actif() )
               )
      )

    )



  })



  #####gestion des modifications des paramètres de la grille ####
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



  #fin de la partie serveur

  }
