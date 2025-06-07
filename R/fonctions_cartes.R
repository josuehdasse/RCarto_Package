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
generer_map <- function(liste_couches){

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
      options_symbologie_couche <-  eval(parse(text = paste( "liste_couches", name_couche, "options_symbologie_couche", sep = "$" ) )) #les oprtions d ela symbologie de la couche


      #options d'effets
      statut_effet_couche <- eval(parse(text = paste( "options_symbologie_couche","options_symbologie_unique", "statut_effet", sep = "$" ) ))

      #on récupéré les informations sur les effets



      ###Fin récupération des paramètres##########


      ##on on gnère d'abord les codes pour les effets
      if(statut_effet_couche){
        couche_symbologies <- generer_code_sumbologie_avec_effets(name_couche, type_symbologie, geometrie, options_symbologie_couche)
      }else{
        couche_symbologies <- generer_code_symbologie(name_couche, type_symbologie, geometrie, options_symbologie_couche)
      }


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


    graphique <- graph # paste( graph, theme_map, sep = "+")

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
          "unique" = {

            #Couleur
            couleur_symbologie <-  eval(parse(text = paste( "options", "options_symbologie_unique", "couleur_symbole", sep = "$" ) ))
            couleur_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "couleur_trait", sep = "$" ) ))
            style_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "style_trait", sep = "$" ) ))
            epaisseur_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "epaisseur_trait", sep = "$" ) ))

            #opacités
            opacity_fill<- eval(parse(text = paste( "options","options_symbologie_unique", "opacity_fill", sep = "$" ) ))
            opacity_border<- eval(parse(text = paste( "options","options_symbologie_unique", "opacity_border", sep = "$" ) ))

            style_fill_symbologie<- eval(parse(text = paste( "options","options_symbologie_unique", "style_fill_symbologie", sep = "$" ) ))

            print("style_fill_symbologie")
            print(style_fill_symbologie)


            if(style_fill_symbologie=="continu"){
              #La couche de remplissage de la carte de base
              couche_symbologie <-  paste0( 'geom_sf(data=',couche, ', linetype="',style_trait,'", colour=alpha("',couleur_trait,'", ',opacity_border,'), fill=alpha("',couleur_symbologie,'", ',opacity_fill,'), linewidth=',epaisseur_trait, ', show.legend = "line" )  ')

            }



          },
          "Catégorisé"={

          }
  )#fin switch sur la gestion de la symbologie

  code_symbologie <- paste0(couche_symbologie)

  return(code_symbologie)
}


#Fonction qui génère le code de la partie des effets
generer_code_sumbologie_avec_effets <- function(couche, symbologie, geometrie, options ){

  code_effet <- ""

  switch (symbologie,
          "unique" = {

            #Couleur
            couleur_symbologie <-  eval(parse(text = paste( "options", "options_symbologie_unique", "couleur_symbole", sep = "$" ) ))
            couleur_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "couleur_trait", sep = "$" ) ))
            style_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "style_trait", sep = "$" ) ))
            epaisseur_trait <-  eval(parse(text = paste( "options","options_symbologie_unique", "epaisseur_trait", sep = "$" ) ))

            #opacités
            opacity_fill<- eval(parse(text = paste( "options","options_symbologie_unique", "opacity_fill", sep = "$" ) ))
            opacity_border<- eval(parse(text = paste( "options","options_symbologie_unique", "opacity_border", sep = "$" ) ))

            style_fill_symbologie<- eval(parse(text = paste( "options","options_symbologie_unique", "style_fill_symbologie", sep = "$" ) ))

            print("style_fill_symbologie")
            print(style_fill_symbologie)

            if(style_fill_symbologie=="continu"){



              #options d'effets

              statut_effet_couche <- eval(parse(text = paste( "options","options_symbologie_unique", "effects", "statut_effet", sep = "$" ) ))
              effets_couches <- eval(parse(text = paste( "options","options_symbologie_unique", "effects", sep = "$" ) ))#On récupère la liste des effets de la couche

              #on filtre seulement les couches aux effets visibles tels qu'indiqués par les options cohés par l'utilisateur
              effets_actifs <- effets_couches #Filter( function(x) x$options$checkec==TRUE, effets_couches )#Filter est une fonction de base de R



                       #on recupère les informations sur la couche source
                    effets_ligne_source <- eval(parse(text = paste( "effets_actifs","options","source", sep = "$" ) ))
                    opacite_source <- eval(parse(text = paste( "effets_ligne_source","options","alpha", sep = "$" ) ))


                    #La couche de remplissage de la carte de base
                    couche_effet_source <-  paste0( 'geom_sf(data=',couche, ', linetype="',style_trait,'", colour=alpha("',couleur_trait,'", ',opacite_source,'), fill=alpha("',couleur_symbologie,'", ',opacite_source,'), linewidth=',epaisseur_trait, ', show.legend = "line" )  ')



                    ##Couche de refeence
                    couche_reference <- paste0(
                      "as_reference(
                        ",paste0(couche_effet_source ), ",
                        id='reference'
                      )"
                    )

                    #On initialise à couche de reference
                    code_effet<- paste0( couche_reference )






              #on applique d'abord les effers de la couche source
              effets_source <- eval(parse(text = paste( "options","options_symbologie_unique", "effects","source", sep = "$" ) ))

              #Caractéristiques des effets source
              #alpha=1, #Opacité
              #couleur="#000000",#la couleur de l'ombre
              #mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))

              alpha_source <-  eval(parse(text = paste( "effets_source", "couleur_symbole", sep = "$" ) ))
              # couleur_source <-  eval(parse(text = paste( "effets_source ","options_symbologie_unique", "couleur_trait", sep = "$" ) ))
              mode_fusion_source <-  eval(parse(text = paste( "effets_source", "mode_fusion", sep = "$" ) ))

              names_effet <- setdiff( names(effets_actifs), c("source")  )

              print("names")
              print(names_effet)

              #print(effets_actifs)


              #on parcours les sources un à un
              for (i in 1:length(names_effet) ) {

                print(names_effet[i])

                switch (names_effet[i],
                        "source"={#effet de la couche source
                          effets_ligne <- eval(parse(text = paste( "effets_actifs","options",paste0(names_effet[i]), sep = "$" ) ))
                          mode_fusion <- eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

                          code_effet_couche <- paste0(
                            "with_blend(
                          ",paste0(couche_effet_source), ",
                          blend_type = ", paste0(mode_fusion), "
                        )\n"
                          )

                         # code_effet<- paste(code_effet, code_effet_couche , sep = "+" )

                        },

                        "drop_shadow_portee" = {#ombre de portée
                          #on recupere les caractéristiques
                          effets_ligne <- eval(parse(text = paste( "effets_actifs",paste0(names_effet[i]), sep = "$" ) ))

                          print("Actif")
                          print(effets_ligne)

                          angle <- eval(parse(text = paste( "effets_ligne","options","angle", sep = "$" ) ))
                          distance <- eval(parse(text = paste( "effets_ligne","options","distance", sep = "$" ) ))
                          sigma <- eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
                          alpha <- eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
                          couleur <- eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
                          mode_fusion <- eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

                          code_effet_couche <- paste0(
                            "with_blend(
                      with_shadow(
                        ",paste0(couche_effet_source), ",
                        sigma = ",sigma,",
                        x_offset = ",distance*cos(angle*pi/180)," ,
                        y_offset =",distance*sin(angle*pi/180),",
                        colour=alpha( '",paste0(couleur),"', ",alpha,"   ),
                      ),
                       bg_layer = 'reference',
                      blend_type = '",paste0(mode_fusion),"'
                    )\n"
                          )

                          if(code_effet==""){
                            code_effet<-code_effet_couche
                          }else{
                            code_effet<- paste(code_effet, code_effet_couche , sep = "+" )
                          }



                        }






                )#fin switch sur les types de mode de fusion


              }#fin du parcours des couches d'effets


















              }#fin de la gestion avec les couhes continus








          },
          "Catégorisé"={

          }
  )#fin switch sur la gestion de la symbologie

  code_effet <- paste0(code_effet)

  return(code_effet)

}


zone_impression_carte <- function(orientation_carte="paysage", largeur_dimension=5.8, hauteur_dimension=3.3, theme_carte){
  cadre <- ggplot() +
    coord_equal(xlim = c(0, largeur_dimension), ylim = c(0, hauteur_dimension), expand = FALSE) #Travailler avec une image HD sur 16:9

  graph<-cadre + theme_carte
}



combiner_cartes <- function(carte1, carte2, xmin, xmax, ymin, ymax){
  graph<- carte1 + annotation_custom(ggplotGrob(carte2), xmin = xmin, xmax = xmax, ymin = ymin,  ymax = ymax)
  return(graph)
}

#on gère l'affichage de la carte ici
finaliser_carte <- function(liste_couches, box_zone_carte, theme){
  #les dimensions de la carte à imprimer
  largeur_box <- sqrt( (box_zone_carte$xmax- box_zone_carte$xmin)^2   )
  longeur_box <- sqrt( (box_zone_carte$ymax- box_zone_carte$ymin)^2   )
  #estimation de la taille des polygones
  ratio=largeur_box/longeur_box

  #on essaie de produire une carte qui se rapproche le plus possible de la dimension 16:9
  largeur=12
  hauteur=largeur/ratio


  if(length(liste_couches)>=1){
          #tire la carte
          graph_obj <- generer_map(liste_couches ) #on va après voir comment rendre le thème dynamique
          graph<- eval(parse(text = graph_obj$code_graphique )) +theme

          ratio_hauteur_graph <- graph_obj$ratio_hauteur

          #essai d'impression du code
          #print(graph)

          #la différence de hauteur du graphique
          diff_hauteur_obj <- ratio_hauteur_graph*longeur_box
          diff_largeur_obj <- ratio*diff_hauteur_obj


          #la zone d'impression de la carte
          zone_impression <- zone_impression_carte(orientation_carte = "paysage", largeur_dimension = largeur, hauteur_dimension = hauteur, theme_carte = theme_graphique)

          #on procède à l'ajout des éléments à la zone d'impression
          ## la carte principale
          mon_graphique <- combiner_cartes(zone_impression, graph, xmin = -0.8, xmax = largeur+0.8, ymin = 0, ymax = hauteur+0.2 )


          resultat =list(
            mon_graphique=mon_graphique,
            ratio=ratio,
            code_graphique=graph_obj$code_graphique
          )

  }else{
    #la zone d'impression de la carte
    mon_graphique <- zone_impression_carte(orientation_carte = "paysage", largeur_dimension = largeur, hauteur_dimension = hauteur, theme_carte = theme_graphique)


    resultat =list(
      mon_graphique=mon_graphique,
      ratio=ratio,
      code_graphique=""
    )

  }


      return(resultat)

}


