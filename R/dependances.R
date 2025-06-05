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
theme_graphique <-  theme(
  panel.background = element_blank(),
  legend.box.background = element_rect(color = "#165984"),
  legend.box.margin = margin(6,6,6,6),
  legend.key = element_rect(fill = "white", colour = "#165984"),
  legend.text = element_text(colour = "#165984" ),
  legend.title = element_text(face="bold"),


  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text = element_blank(),

  #mise en forme des elements text
  text = element_text(family = "Arial Narrow"),

  #palcer la légende à l'intérieur
  legend.position = c(0.93,0.88),
  legend.justification = c(0.93,0.88),

  #panel.background = element_rect(fill = "red"),
  panel.margin = unit(0, "lines"),
  plot.margin = unit(c(0, 0, 0, 0), "lines")

)


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



