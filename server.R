#charger les dépendances (fonctions, données, packages)
source("R/dependances.R")

shinyServer( function(input, output, session){


    #Element réactif pour la gestion des couches ##########
    liste_couches <- reactiveVal(
      list()
    )


    #on rend aussi dynamique la zone globale de la carte #####
    box_zone_carte <- reactive({

      if(length(liste_couches())>=1){
        base<- reunion_couches(liste_couches()) %>% st_bbox()#le box
      }
    })

    #Appel du module de la gestion des couches
    callModule(mod_gestion_couches_server, "map_ggplot", liste_couches )

    #on met un observateur sur la liste des couches afin de déclencehr des actions relatives ######
    observeEvent(liste_couches(),{

      print("La liste est vide")

      if(length(liste_couches())>=1){

        print("LA liste n'est plus vide")

        print(liste_couches())

        #on envoie la liste des couches à javascript
        session$sendCustomMessage("liste_couches", liste_couches() )

        #resultJs<- fromJSON(input$couleur_unique)

        #on doit sélectionner spécialement les couches visibles
        #couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de base de R

        couches_visibles_app <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de base de R
        #le graphique ici (on produit une version finalisée du graphique pour la présentation)
        data_graph<- finaliser_carte( couches_visibles_app, box_zone_carte() )

        #les données graphiques à transmettre
        graph<-  data_graph$mon_graphique
        code_graph<-  data_graph$code_graphique
        ratio<- data_graph$ratio


        #on n'actualise que si le nbre des couches est >0
        #on actualise aussi la représentation des couches séclectionnés
        callModule(mod_impression_carte_server, "map_ggplot", graph, ratio)

        #on rend le code genré
        callModule(mod_rendu_code_server, "map_ggplot",code_graph )

      }#fin contrôle sur la condition selon que le nombre de couches doit excéder 1

    })




  })#fin de l'application


