#réunion  des couches mobilisés pour former le box de la zone de carte
reunion_couches <- function(liste_couches){

  couche_1 = liste_couches[1]
  name_couche_1<-names(couche_1)

  base_couche_1 <-  eval(parse(text = paste( "couche_1", name_couche_1, "couche", sep = "$" ) ))


  couche_union <- st_union(base_couche_1)

  for (i in 1:length(liste_couches)) {
    couche_courant = liste_couches[i]
    name_couche_courant<-names(couche_courant)
    base_couche <-  eval(parse(text = paste( "couche_courant",name_couche_courant, "couche", sep = "$" ) ))
    union <- st_union(base_couche)

    couche_union<- st_union(couche_union, union)

  }

  return(couche_union)
}




#fonction qui produit le code du map selon les configurations des listes
#le map est généré ici à la base des couches qui sont dans le système
generer_map <- function( liste_couches){

  #lNB: Les box seront les r;eunions inividuelels des couches


  #initialisation du graphique
  graph<- paste0( 'ggplot()')
  ratio_hauteur=1#on initialise le ration de la hauteur à 1 (s'il n'ya pas de titre qui vienne modifier la donne)

    for (i in 1:length(liste_couches)) {
      #on récupère les informations de la couche courante
      couche_courant = liste_couches[i]
      name_couche <- names(couche_courant)

      ###Récupération des paramètres généraux################
      #on gère suivant la symbologie choisie
      selection <- paste0("liste_couches")
      texte_base <- paste("liste_couches", name_couche, "couche", sep = "$" )
      base_couche <-  eval(parse(text = paste("liste_couches", name_couche, "couche", sep = "$" ) ))
      type_symbologie <-  eval(parse(text = paste( "liste_couches", name_couche, "type_symbologie", sep = "$" ) ))
      geometrie <-  eval(parse(text = paste( "liste_couches", name_couche, "geometrie", sep = "$" ) )) #gemortie (pooijnt, ligne, polygone, etc)
      options_symbologie_couhe <-  eval(parse(text = paste( "liste_couches", name_couche, "options_symbologie_couhe", sep = "$" ) )) #les oprtions d ela symbologie de la couche

      ###Fin récupération des paramètres##########
      couche_symbologies <- generer_code_symbologie(name_couche, type_symbologie, geometrie, options_symbologie_couhe)

      ##Générer le code pour la symbologie choisie #####

      graph<- paste(graph, couche_symbologies, sep = "+" )



    }#fin de la gestion d'une couche individuelle

    #Gestion des options globales au graphique sur toutes les couches vecteurs
  #elements
  theme_map <-  paste0('theme(
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

        #palcer la légende à lintérieur
        legend.position = c(0.93,0.88),
        legend.justification = c(1,1),

        #panel.background = element_rect(fill = "red"),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0, 0, 0, 0), "lines")
      )' )


    graphique <- paste( graph, theme_map, sep = "+")

    print(graphique)

    #print(graphique)

    resultat =list(
     # graphique= eval(parse(text = graphique)),
      code_graphique=graphique,
      ratio_hauteur=ratio_hauteur
    )


  return(resultat)


}

#fonction qui gènère le code pour la partie de la symbologie
generer_code_symbologie <- function(couche, symbologie, geometrie, options ){

  code_symbologie <- ""

  switch (symbologie,
          "Symbole unique" = {

            #Couleur
            couleur_symbologie <-  eval(parse(text = paste( "options", "options_symbologie_unique", "couleur_symbole", sep = "$" ) ))
            couleur_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "couleur_trait", sep = "$" ) ))
            style_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "style_trait", sep = "$" ) ))
            epaisseur_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "epaisseur_trait", sep = "$" ) ))

            #La couche de remplissage de la carte de base
            couche_symbologie <-  paste0( 'geom_sf(data=',couche, ' , alpha=1 , linetype="',style_trait,'", colour="',couleur_trait,'", fill="',couleur_symbologie, '", linewidth=',epaisseur_trait, ', show.legend = "line" )  ')



          },
          "Catégorisé"={

          }
  )#fin switch sur la gestion de la symbologie

  code_symbologie <- paste0(couche_symbologie)

  return(code_symbologie)
}



impression_carte <- function(orientation_carte="paysage", largeur_dimension=5.8, hauteur_dimension=3.3, theme_carte){
  cadre <- ggplot() +
    coord_equal(xlim = c(0, largeur_dimension), ylim = c(0, hauteur_dimension), expand = FALSE) #Travailler avec une image HD sur 16:9

  graph<-cadre + theme_carte
}



combiner_cartes <- function(carte1, carte2, xmin, xmax, ymin, ymax){
  graph<- carte1 + annotation_custom(ggplotGrob(carte2), xmin = xmin, xmax = xmax, ymin = ymin,  ymax = ymax)
  return(graph)
}


