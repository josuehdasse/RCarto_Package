#charger les packages
source("R/packages.R")

#charger les données et fonctions
source("R/data.R")
source("R/fonctions.R")

#fonctions de gestion de la carte
source("R/fonctions_cartes.R")

#communication avec Javascript
#source("R/websocket.R")

#liste des couleurs#############
nb_couleur_ligne=25

liste_couleurs <- liste_couleurs_brewer()#on appelle fonction qui permet de gerer cela à l'aide du package Brewer


#les listes objets à utiliser dans l'application
### Liste et boutons de gestion des jointures
ObjetsEnv= ls(envir = globalenv() )#tous les objets de l'env


#La liste des objets data.frame qui ne sont pas de txpe sf
listeOjetsTidyEnv <-  ObjetsEnv[sapply(ObjetsEnv, function(x){
    obj <- get(x)
    (is.data.frame(obj)|| inherits(obj, c("tbl_df", "tbl", "data.table", "tibble")) ) && ! inherits(obj, "sf")
} )]

ListeSfEnv <- ObjetsEnv[sapply(ObjetsEnv, function(x) inherits(get(x), "sf"))]


#les options caractéristiques d'une page d'un composeur d'impression#################
options_pages=list(
  a4=list(
    min_taille=210,
    max_taille=297
  ),
  a6=list(
    min_taille=105,
    max_taille=148
  ),
  a5=list(
    min_taille=148,
    max_taille=210
  ),
  a3=list(
    min_taille=297,
    max_taille=420
  ),
  a2=list(
    min_taille=420,
    max_taille=594
  ),
  a1=list(
    min_taille=594,
    max_taille=841
  ),
  a0=list(
    min_taille=841,
    max_taille=1189
  ),
  b6=list(
    min_taille=125,
    max_taille=176
  ),
  b5=list(
    min_taille=176,
    max_taille=250
  ),
  b4=list(
    min_taille=250,
    max_taille=353
  ),

  b3=list(
    min_taille=353,
    max_taille=500
  ),

  b2=list(
    min_taille=500,
    max_taille=707
  ),

  b1=list(
    min_taille=707,
    max_taille=1000
  ),

  b0=list(
    min_taille=1000,
    max_taille=1414
  ),

  legal=list(
    min_taille=215,
    max_taille=355
  ),

  letter=list(
    min_taille=215,
    max_taille=279
  ),

  ainsi_a=list(
    min_taille=215.400,
    max_taille=279.900
  ),

  ainsi_b=list(
    min_taille=279.400,
    max_taille=431.800
  ),

  ainsi_c=list(
    min_taille=431.800,
    max_taille=558.800
  ),

  ainsi_d=list(
    min_taille=558.800,
    max_taille=863.600
  ),

  ainsi_e=list(
    min_taille=863.600,
    max_taille=1117.600
  ),

  arch_a=list(
    min_taille=228.600,
    max_taille=304.800
  ),

  arch_b=list(
    min_taille=304.800,
    max_taille=457.200
  ),

  arch_c=list(
    min_taille=457.200,
    max_taille=609.600
  ),

  arch_d=list(
    min_taille=609.600,
    max_taille=914.400
  ),

  arch_e=list(
    min_taille=914.400,
    max_taille=1219.200
  ),

  arch_e1=list(
    min_taille=762,
    max_taille=1066.800
  ),

  arch_e2=list(
    min_taille=660.800,
    max_taille=965.600
  ),

  arch_e3=list(
    min_taille=686.800,
    max_taille=991.600
  ),

  format_16_9=list(
    min_taille=91.440,
    max_taille=162.560
  ),

  format_16_10=list(
    min_taille=67.733,
    max_taille=103.373
  ),

  format_4_3=list(
    min_taille=65.024,
    max_taille=86.699
  )

)


liste_choix_options_pages <- list(
  "A6"="a6",  "A5"= "a5", "A4"  ="a4", "A3"= "a3", "A2"=  "a2", "A1"= "a1",   "A0"= "a0",   "B6"=  "b6",   "B5"= "b5",  "B4"="b4",  "B3"= "b3", "B2"="b2", "B1"="b1" , "B0"= "b0",
  "Legal"="legal",  "Letter"="letter",   "Ainsi A"= "ainsi_a",    "Ainsi B"=   "ainsi_b",   "Ainsi C" ="ainsi_c",    "Ainsi D"= "ainsi_d",   "Ainsi E" ="ainsi_e",  "Arch A"= "arch_a",
  "Arch B"="arch_b",  "Arch C"="arch_c",     "Arch D" =  "arch_d", "Arch E"="arch_e",  "Arch E1"= "arch_e1",     "Arch E2"=  "arch_e2",   "Arch E3"="arch_e3",
  "Format 16:9"="format_16_9",   "format 16:10"="format_16_10",  "format 4:3"="format_4_3", "Personnalisé"="personnalise"
)


#thème des cartes##############################

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
      axis.text = element_blank(),#on supprimes les titres des axes

      #Pas de ticks des axes pour un départ
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),

      #placer la légende à lintérieur
      legend.position = c(0.93,0.88),
      legend.justification = c(0.93,0.88),

     #le panneau et la zone graphique
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0, 0, 0, 0), "lines" ))'



generer_theme_objets_cartes <- function(statut_cadre,  statut_grille,  PanelBackground="#ffffff", PanelborderColor="#000000", PanelborderSize=1, PanelLinetype="solid", gridColour="#DCDCDC", gridLinetype="solid", gridSizeLine=0.2, EspacementCadre=0 ){

  if(statut_cadre){

    if(statut_grille){
          theme_objets_cartes <-  paste0('theme(
        panel.background = element_rect(fill="',PanelBackground,'"),
        panel.border = element_rect(colour="',PanelborderColor,'", fill=NA, linewidth=',PanelborderSize,', linetype="',PanelLinetype,'" ),

        panel.grid.major = element_line(colour="',gridColour,'"),
        panel.grid.minor = element_line(colour="',gridColour,'", linetype="',gridLinetype,'", linewidth=',gridSizeLine,'),

        legend.box.background = element_rect(color = "#165984"),
        legend.box.spacing = margin(6,6,6,6),
        legend.key = element_rect(fill = "white", colour = "#165984"),
        legend.text = element_text(colour = "#165984" ),
        legend.title = element_text(face="bold"),

        #options des titres des axes X et Y
        axis.text.x = element_text(colour="blue", size=4, hjust=0, vjust=',EspacementCadre,'),
        axis.text.y = element_text(colour="blue", size=4, hjust=',EspacementCadre,', vjust=0),
        axis.text = element_blank(),#on supprimes les titres des axes

        #Pas de ticks des axes pour un départ
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),

        #placer la légende à lintérieur
        legend.position = c(0.93,0.88),
        legend.justification = c(0.93,0.88),

       #le panneau et la zone graphique
        plot.background=element_rect(fill="',PanelBackground,'"),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0, 0, 0, 0), "lines" ))')

    }else{
      theme_objets_cartes <-  paste0('theme(
    panel.background = element_rect(fill="',PanelBackground,'"),
    panel.border = element_rect(colour="',PanelborderColor,'", fill=NA, size=',PanelborderSize,', linetype="',PanelLinetype,'" ),

    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

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
   plot.background=element_rect(fill="',PanelBackground,'"),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0, 0, 0, 0), "lines" ))')
    }


  }else{

    theme_objets_cartes <-  paste0('theme(
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
    plot.margin = unit(c(0, 0, 0, 0), "lines" ))')
  }



    #on retrourne l'objet crée
    return(theme_objets_cartes)
}


theme_objets_cartes <-  'theme( panel.background = element_blank(),
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
      plot.margin = unit(c(0, 0, 0, 0), "lines" ))'




#liste des crs à utiliser################################
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




# Les options de la definition d'une couche vecteur lors de sa créaction #############

## Les options par défaut d'une couche de vecteur  #############
options_defaut_couche_vecteur = list(
  couche= "donnees_couche_vecteur" ,#element sf d'une couche sf de vecteur
  id_projet=1,#L'id du projet
  crs=4326,#la projectino par défaut d'une couche de vecteur
  name= "nom_cpuche",#le nom de la couche de vecteur
  type_symbologie="unique",#Le type de symbologie de la couche par défaut
  visible=TRUE,#visiilité de la couche
  geometrie= "geometrie_couche", #on controle la geometrie pour gerer la crte plus tard (point, ligne, polygone, etc)
  position_couche = 1, #la position de la couche
  options_symbologie_couche = list() #les option de gestion de la symbologie des couches des vecteurs
)


## Les otpions par défaut des couches de symbologie #########################
options_defaut_symbologies_couche = list(
  options_symbologie_unique=list(),
  options_symbologie_categorise=list(
    colonne_valeur_symbologie="",
    palette_couleurs="Aleatoire",
    symbole=list(),
    categories=list()#les couches des categories avec leurs symbologies
  ),
  options_symbologie_graduee=list(), #pour le niveau gradue des symbologie
  jointures=list()
)




##option oar defaut des categories de symboles des couches#################
options_defaut_categories_symbologies =list(
  name="nom categorie",
  valeur="",
  legende="",
  visible=TRUE,#visiilité de la couche
  couches_symbologies=list()
)




## Les options par defaut d'une couche de symbologie ###########################
options_defaut_couche_symbologie =list(
    name="symbologie_1",
    label="Remplissage simple 1",
    position=1,
    #Gestion des symboles uniques de la couche
    couleur_symbole= 1,
    style_fill_symbologie="continu",
    legende="",
    couleur_trait="#DF1616",
    style_trait="solid",
    epaisseur_trait=1,
    #opacity_fill=1,
    #opacity_border=1,
    statut_effet=FALSE,
    effects=list(),
    patterns= list()
)


## Les options par défaut des patterns des couches de symbologie #################
options_defaut_pattern_symbologie =list(
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




## Les options par défaut des couches des effets ########################################
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
    name="dropshadowinterieure",
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
    name="innnerglow",
    options=list(
      rayon=2,#Rayon
      sigma=2.6450,#Rayon de floutage ou intensité de flou
      alpha=0.5, #Opacité
      couleur="#000000",#la couleur de l'ombre,
      palette="",#la palette de couleurs
      mode_fusion="overlay"#le mode de fusion de l'ombre (défaut sur normal)
    )

  ),
  outer_glow=list(
    checked=TRUE,
    label="Luminescence externe",
    name="outerglow",
    options=list(
      rayon=2,#Rayon
      sigma=2.6450,#Rayon de floutage ou intensité de flou
      alpha=0.5, #Opacité
      couleur="#000000",#la couleur de l'ombre,
      palette="",#la palette de couleurs
      mode_fusion="overlay"#le mode de fusion de l'ombre (défaut sur normal)
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
      couleur="#ff8080",
      niveau_gris="pas_clarte",
      alpha=0.5, #Opacité
      mode_fusion="normal"
    )

  )
)

#options des modes de fusions ######################
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








#Liste des Jointures#################
options_defaut_jointure=list(
  name_table="table",#le nomd ela table à joindre
  colonne_table="colonnne dans la table",#La colonne à utiliser poour établir la jointure
  colonne_couche_cible="La colonne dans la couche cible"
)







