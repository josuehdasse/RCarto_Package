#charger les packages
source("R/packages.R")

#charger les données et fonctions
source("R/data.R")
source("R/fonctions.R")



#initialisation du serveur websocket
#connexion globale
#ws <- WebSocket$new("ws://localhost:8080")



###Gestion conditionné du démarrage du serveur
if(exists("s")){
  #on verifie si c'est en fonctionnemnt
  if(s$isRunning()!=TRUE){

    s <- startServer("127.0.0.1", 8080,
                     list(
                       onWSOpen = function(ws) {
                         # The ws object is a WebSocket object
                         cat("Connexion au serveur ouvert.\n")
                         ws$onMessage(function(binary, message) {
                           cat("Server received message:", message, "\n")
                           ws$send("Hello client!")
                         })
                         ws$onClose(function() {
                           cat("Connexion au serveur fermé.\n")
                         })
                       }
                     )
    )

  }
}else{
  s <- startServer("127.0.0.1", 8080,
                   list(
                     onWSOpen = function(ws) {
                       # The ws object is a WebSocket object
                       cat("Connexion au serveur ouvert.\n")
                       ws$onMessage(function(binary, message) {
                         cat("Server received message:", message, "\n")
                         ws$send("Hello client!")
                       })
                       ws$onClose(function() {
                         cat("Connexion au serveur fermé.\n")
                       })
                     }
                   )
  )
}




ws<- websocket::WebSocket$new("ws://127.0.0.1:8080/")



#liste des couleurs
nb_couleur_ligne=25
liste_couleurs <- c( wheel("steelblue", nb_couleur_ligne -1 ),
                     wheel("cornflowerblue", nb_couleur_ligne ),
                     wheel("firebrick", nb_couleur_ligne),
                     wheel("palegoldenrod",  nb_couleur_ligne ),
                     wheel("forestgreen", nb_couleur_ligne )
)

