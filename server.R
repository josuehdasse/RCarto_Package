#charger les dépendances (fonctions, données, packages)
source("R/dependances.R")

shinyServer( function(input, output, session){



    #Element réactif pour la gestion des couches ##########
    liste_couches <- reactiveVal(
      list()
    )


    #liste des objets ajoutés à la mise en forme dela crte
    liste_objets_mise_en_page <- reactiveVal(
      list()
    )

    #reatifs pour la gestion de la mise en page
    hauteur_page_actif<-reactiveVal(210)
    largeur_page_actif<-reactiveVal(297)
    resolution_page_actif<- reactiveVal(300)
    fond_page_actif <- reactiveVal("#ffffff")
    orientation_page_actif<-reactiveVal("Paysage")


    #on rend aussi dynamique la zone globale de la carte #####
    box_zone_carte <- reactive({
      if(length(liste_couches())>=1){
        base<- reunion_couches(liste_couches()) %>% st_bbox()#le box
      }
    })


    #Appel du module de la gestion des couches
    callModule(mod_gestion_couches_server, "map_ggplot", liste_couches )

    #on recupère les options de la mise en page
    options_mise_en_page <- callModule(mise_en_page_server, "map_ggplot", largeur_page_actif, hauteur_page_actif, resolution_page_actif, fond_page_actif, orientation_page_actif )


    #Module de gestion des options de contrôle
    #On lance la mise en page de la carte
    callModule(mod_controle_impression_server, "map_ggplot", liste_objets_mise_en_page, liste_couches, largeur_page_actif, hauteur_page_actif   )

    #Gestionnaire des objets de la carte
    callModule(gestionnaire_objet_carte_server, "map_ggplot",liste_objets_mise_en_page )


    #la liaison directe avec la carte produite
    observe({
      page_carte <- zone_impression_carte(orientation_carte = orientation_page_actif(), largeur_dimension =  largeur_page_actif(), hauteur_dimension =hauteur_page_actif(), fond = fond_page_actif()       )
      callModule(mod_impression_carte_server, "map_ggplot", liste_objets_mise_en_page(), page_carte,hauteur_page_actif(), largeur_page_actif(), resolution_page_actif(), liste_couches(), box_zone_carte() )

    })



    #on met un observateur sur la liste des couches afin de déclencehr des actions relatives ######
    observeEvent(liste_couches(),{

     # print("La liste est vide")

      if(length(liste_couches())>=1){

       #print("La liste n'est plus vide")
        #on envoie la liste des couches à javascript

        session$sendCustomMessage("liste_couches", liste_couches() )
        #on doit sélectionner spécialement les couches visibles
        #couches_visibles <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de b

        #couches_visibles_app <- Filter( function(x) x$visible==TRUE, liste_couches())#Filter est une fonction de base de R
        #le graphique ici (on produit une version finalisée du graphique pour la présentation)

        #print(them)
        #data_graph<- finaliser_carte( couches_visibles_app, box_zone_carte(), theme_graphique )

        #les données graphiques à transmettre
        #graph<-  data_graph$mon_graphique #+eval(parse(text = theme_graphique  )) #on ajoute le thème ici

        #code_graph<- data_graph$code_graphique  #paste( data_graph$code_graphique, paste0(theme_graphique), sep = "+\n")
        #ratio<- data_graph$ratio

        #on n'actualise que si le nbre des couches est >0
        #on actualise aussi la représentation des couches séclectionnés
        #callModule(mod_impression_carte_server, "map_ggplot", graph, ratio)



        #Gestion de la carte source
        #callModule(gestionnaire_objet_carte_server,"map_ggplot", box_zone_carte() )


        #Gestion des options du contrôle grapgique


      }#fin contrôle sur la condition selon que le nombre de couches doit excéder 1

    })





  })#fin de l'application


