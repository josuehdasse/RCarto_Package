if (!require("pacman")) install.packages("pacman")


pacman::p_load(
  tidyverse,
  purrr,
  shiny,
  shinydashboard,
  shinyjs,
  shinyBS,
  bslib,#bootstrap
  rnaturalearthdata,#Jeux des données du monde
  shinycssloaders,#pour la gestion du loader à l'affichage d'un contenu (appelé plus tard avec wihSpinner)
  RColorBrewer,#pour la gestion des couleurs sur une palette
  colourpicker,
  ggfx,
  htmltools,
  lwgeom,#pour accéder à st_make_valid des couches
  ggpattern,#pour la gestion des patterns
  ggspatial,
  systemfonts,#Pour la gestion des fonts en complément de ceux qui disponibles dans WIndows
  jsonlite,
  ggtext,#pour l'utilisation odu markdown dans les elements graphiques
  stringr
)




