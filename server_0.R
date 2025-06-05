#charger les dépendances (fonctions, données, packages)
source("R/dependances.R")


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

print(names(liste_crs))

shinyServer(
  function(input, output, session){





    #gestin de l'envoi automatique des données


    #on ferme la connexion du serveur à la fin de la session (websocket)
    #session$onSessionEnded(function(){
      #ws$close()

    #})

  }
)
