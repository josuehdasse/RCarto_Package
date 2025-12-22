geom_sf_pattern(
  data = data,
  pattern=, #Type de motif (stripe, crossbatch, point, circle, none, pch=plotting character, image=pour utiilser des images comme motifs, gradient, magigck, placeholder, plasma, wave)
  pattern_alpha=,#lOpacité
  pattern_fill=, #couleur de remplissage du motif,
  pattern_colour=, #Couleur des lignes du motif
  pattern_angle= , #Angle du motif,
  pattern_density, #Densité du motif (0-1),
  pattern_spacing, #espacement entre les motifs,
  pattern_size= , #Epaisseur des lignes du motif
  stat="sf",
  na.rm = TRUE,
  show.legend = NA,
)

#On cherhce à remplacer le tableau par un ul

tags$thead(
  tags$tr(
    tags$th("#"),
    tags$th("Symboles", scope="col"),
    tags$th("Couches", scope="col"),
    tags$th("Légende", scope="col"),
    tags$th("Options", scope="col")
  )
),
tags$(id=ns("liste_couches_carte"))






graph_essai <- ggplot()+geom_sf(data=carte_admin_1, linetype="solid", colour=alpha("#DF1616", 1),
                                fill=alpha("#4682B4", 1), linewidth=1,
                                show.legend = "line" )  +
                               coord_sf(
                                 datum = st_crs(4326),
                                 label_axes = "ENMS"
                               ) +eval(parse(text =  generer_theme_objets_cartes(statut_cadre = TRUE) ))






  req(input$select_pattern_spacing)
pattern_spacing_actif(input$select_pattern_spacing)
updateNumericInput(session, "select_pattern_spacing", label = NULL, min = 0, max=NA, value =  pattern_spacing_actif() )
gauche_pattern_spacing <- paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_spacing",  sep="$")
eval(parse(text =  paste(gauche_pattern_spacing, input$select_pattern_spacing, sep = "<-")   ))
options_symbologies_couche_actif(copie_symbologie)












  paste0(
    'geom_sf_pattern(data=',couche,',
                  pattern="', pattern_couche,'",
                                                       pattern_spacing=', pattern_spacing, '",
                                                       pattern_angle=',pattern_angle, ' ,
                                                       pattern_size=',pattern_size, ',
                                                       pattern_colour="', paste0(pattern_colour),'",
                                                       pattern_linetype="', paste0(pattern_linetype), '",
                                                       linetype="',style_trait,'", colour="',paste0(couleur_trait),'", linewidth=',epaisseur_trait, ', show.legend = "line" )  ')





  #test avec pattern (stripe) => lignes parralèles

  graph_stripe_pattern <- ggplot()+
      geom_sf_pattern(
        data = carte_admin_1,
        pattern="stripe",
        #pattern_density=1,#Surface que l'on accorde à un élément du motif individuel
        pattern_spacing=0.009,#Esapce entre deux motifs
        pattern_angle=45 , #Angle du motif,
        pattern_size=0.5,#taille du motif
        pattern_colour="#4682B445",#la couleur de bordure du patterne
        pattern_linetype="solid",##le type de ligne du pattern
        #pattern_scale=10,#Echelle

        linetype="solid",
        linewidth=1.5
      )


#essai pour les "Croix"
graph_crossbatch_pattern <- ggplot()+
  geom_sf_pattern(
    data = carte_admin_1,
    pattern="crosshatch",
    pattern_density=0.2,
    pattern_angle=45 , #Angle du motif,
    pattern_size=0.5,
    pattern_spacing=0.05,
    pattern_colour="#4682B4",
    pattern_fill="red",
    pattern_linetype="solid",
    #pattern_scale=2,

    linetype="solid",
    linewidth=1.5
  )


#Elements cericulares ou point
graph_circle_pattern <- ggplot() +
  geom_sf_pattern(
    data = carte_admin_1,
    pattern="circle",
    pattern_density=0.8,#Surface que l'on accorde à un élément du motif individuel
    pattern_spacing=0.09,
    pattern_colour="#4682B4",
    pattern_fill="red",
    pattern_linetype="dashed",

    #éléments de la geometrie
    linetype="solid",
    linewidth=1.5
  )

#Essai avec les elements en gradient
graph_gradient_pattern <- ggplot() +
  geom_sf_pattern(
    data = carte_admin_1,
    pattern="gradient",
    pattern_fill="red",
    pattern_fill2="green",
    pattern_orientation="horizontal"
  )


#Essai des trucs avec image
graph_image_pattern <- ggplot() +
  geom_sf_pattern(
    data = carte_admin_1,
    pattern="image",
    pattern_type="squish",#Tile (repété), fit (ajusté), squish (déformé), expand (avec dégradé)
    pattern_filename="./www/image.jpg",
    pattern_scale=1
  )



#Essai avec le package wave
graph_wave_pattern<- ggplot()+
  geom_sf_pattern(
    data = carte_admin_1,
    pattern="wave",
    pattern_frequency=5,
    pattern_fill="blue",
    pattern_colour="navy",
    pattern_angle=0 , #Angle du motif,
    pattern_spacing=0.02
  )






liste_essai <- list(
  a=list(couleur_symbole= "",
  style_fill_symbologie="continu",
  legende="",
  couleur_trait="#DF1616",
  style_trait="solid",
  epaisseur_trait=1,
  position =0,
  effets=list()),

  b=list(couleur_symbole= "",
         style_fill_symbologie="continu",
         legende="",
         couleur_trait="#DF1616",
         style_trait="solid",
         epaisseur_trait=1,
         position =1),

  c=list(couleur_symbole= "",
         style_fill_symbologie="continu",
         legende="",
         couleur_trait="#DF1616",
         style_trait="solid",
         epaisseur_trait=1,
         position =2),

  d=list(couleur_symbole= "",
         style_fill_symbologie="continu",
         legende="",
         couleur_trait="#DF1616",
         style_trait="solid",
         epaisseur_trait=1,
         position =0)

)


couleur_symbole= liste_couleurs[nbre_couches_ajoutes+1],
style_fill_symbologie="continu",
legende=input$select_couche,
couleur_trait="#DF1616",
style_trait="solid",
epaisseur_trait=1,
position =0



















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









































#La carte de base des repères
base_graph <- ggplot() +
  coord_equal(xlim = c(0, 297), ylim = c(0, 210), expand = TRUE)


#Le code pour l'objet graphique 1
graph_1<- ggplot()+as_reference(
  geom_sf(data=carte_admin_1, linetype="solid", colour="#DF1616", fill="#57B546", linewidth=0.92, show.legend = "line" )  ,
  id= 'reference1_1'
)+with_blend(
  with_shadow(
    geom_sf(data=carte_admin_1, linetype="solid", colour="#DF1616", fill="#57B546", linewidth=0.92, show.legend = "line" )  ,
    sigma = 2.645,
    x_offset = -1.41421356237309 ,
    y_offset =1.4142135623731,
    colour=alpha( '#000000', 1   ),
  ),
  bg_layer = 'reference1_1',
  blend_type = 'multiply',
  stack = TRUE
)+
  coord_sf(crs=st_crs(4326),
           datum=st_crs(4326),
           label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
           xlim=c(14, 27 ),
           ylim=c(2, 11 )
  )+

  scale_x_continuous(breaks=seq(14, 27,by= 1) ) +
  scale_y_continuous(breaks=seq(2, 11,by= 1) ) +

  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.box.background = element_rect(color = "#165984"),
    legend.box.margin = margin(6,6,6,6),
    legend.key = element_rect(fill = "white", colour = "#165984"),
    legend.text = element_text(colour = "#165984" ),
    legend.title = element_text(face="bold"),
    #options des titres des axes X et Y
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),#on supprimes les titres des axes

    #Pas de ticks des axes pour un départ
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),

    #placer la légende à lintérieur
    legend.position = c(0.93,0.88),
    legend.justification = c(0.93,0.88),

    #le panneau et la zone graphique
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0, 0, 0, 0), "lines" ))


#Assemblage des objets graphqiques créees

graph <- combiner_cartes(base_graph, graph_1 , xmin = 0, xmax = 297, ymin = 0, ymax = 210 )

