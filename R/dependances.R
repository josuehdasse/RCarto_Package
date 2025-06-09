#charger les packages
source("R/packages.R")

#charger les données et fonctions
source("R/data.R")
source("R/fonctions.R")

#fonctions de gestion de la carte
source("R/fonctions_cartes.R")

#communication avec Javascript
#source("R/websocket.R")

#liste des couleurs
nb_couleur_ligne=25
liste_couleurs <- c( wheel("steelblue", nb_couleur_ligne -1 ),
                     wheel("cornflowerblue", nb_couleur_ligne ),
                     wheel("firebrick", nb_couleur_ligne),
                     wheel("palegoldenrod",  nb_couleur_ligne ),
                     wheel("forestgreen", nb_couleur_ligne )
)


#thème des cartes

#elements
theme_graphique <-  'theme( panel.background = element_blank(),
  legend.box.background = element_rect(color = "#165984"),
  legend.box.margin = margin(6,6,6,6),
  legend.key = element_rect(fill = "white", colour = "#165984"),
  legend.text = element_text(colour = "#165984" ),
  legend.title = element_text(face="bold"),
  #options des titres des axes X et Y
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text = element_blank(),#on supprime les tick

  #placer la légende à lintérieur
  legend.position = c(0.93,0.88),
  legend.justification = c(0.93,0.88),

 #le panneau et la zone graphique
  panel.margin = unit(0, "lines"),
  plot.margin = unit(c(0, 0, 0, 0), "lines" ))'


#liste des crs à utiliser
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



##Gestion des effets de la symbologie d'une couche##############
options_defaut_effets =list(
  drop_shadow_portee=list(#options de la gestion de l'ombre de portée
    checked=TRUE,
    label="Ombre de portée",
    name="dropshadowportee",
    options=list(
      angle=135,#angle de décalage en X (horizontal)
      distance=2, #distance du décallage en y
      sigma=2.6450,#Rayon de floutage ou intensité de flou
      alpha=1, #Opacité
      couleur="#000000",#la couleur de l'ombre
      mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply)
    )

  ),
  drop_shadow_interieure=list(
    checked=TRUE,
    label="Ombre intérieure",
    name="drop_shadow_interieure",
    options=list(
      angle=135,#angle de décalage en X (horizontal)
      distance=2, #distance du décallage en y
      sigma=2.6450,#Rayon de floutage ou intensité de flou
      alpha=1, #Opacité
      couleur="#000000",#la couleur de l'ombre
      mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))
    )

  ),
  innner_glow=list(#luminaissance interne
    checked=TRUE,
    label="Luminescence interne",
    name="innner_glow",
    options=list(
      rayon=2,#Rayon
      sigma=2.6450,#Rayon de floutage ou intensité de flou
      alpha=0.5, #Opacité
      couleur="#000000",#la couleur de l'ombre,
      palette="",#la palette de couleurs
      mode_fusion="normal"#le mode de fusion de l'ombre (défaut sur normal)
    )

  ),
  outer_glow=list(
    checked=TRUE,
    label="Luminescence externe",
    name="outer_glow",
    options=list(
      rayon=2,#Rayon
      sigma=2.6450,#Rayon de floutage ou intensité de flou
      alpha=0.5, #Opacité
      couleur="#000000",#la couleur de l'ombre,
      palette="",#la palette de couleurs
      mode_fusion="normal"#le mode de fusion de l'ombre (défaut sur normal)
    )

  ),

  source=list(
    checked=TRUE,
    label="Source",
    name="source",
    options=list(
      alpha=1, #Opacité
      couleur="#000000",#la couleur de l'ombre
      mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))
    )

  ),
  transformer=list(
    checked=TRUE,
    label="Transformer",
    name="transformer",
    options=list(
      miroir_horizontal=FALSE,
      miroir_vertical=FALSE,
      cisaille_x=0,
      cisaille_y=0,
      echelle_x=1,
      echelle_y=1,
      rotation=0,
      translation_x=0,
      translation_y=0
    )

  ),
  flou=list(
    checked=TRUE,
    label="Flou",
    name="flou",
    options=list(
      type_flou="empilé",
      sigma=2.6450,#Rayon de floutage ou intensité de flou
      alpha=1, #Opacité
      mode_fusion="normal"#le mode de fusion de l'ombre (défaut sur normal)
    )

  ),
  coloriser=list(
    checked=TRUE,
    label="Coloriser",
    name="coloriser",
    options=list(
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
)

#options des modes de fusions
liste_mode_fusion = list(
  #Standards
  "Normal"="over",
  "Produit"="multiply",
  "Ecran"="screen",
  "Superposition"="overlay",
  "Assombrir"="darken",
  "Eclaircir"="lighten",
  "Densité de couleur -"="color_dodge",
  "Densité de couleur +"="color_burn",
  "Lumière crue"="hard_light",
  "Lumière tamisée"="soft_light",
  "Différence"="difference",
  "Exclusion"="exclusion",

  #basés sur le HSL
  "Teinte"="hue",
  "Saturation"="saturation",
  "Couleur"="color",
  "luminosité"="luminosity",


  #modes avancées (moins courants)
  "Au dessus"="atop",
  "Destination dans"="dest_in",
  "Destination hors"="dest_out",
  "Effacer"="clear",
  "Source"="source",
  "Dans"="in",
  "Hors"="out",
  "Ou exclusif"="xor",
  "Addition"="add",
  "Saturer"="saturate"

)



