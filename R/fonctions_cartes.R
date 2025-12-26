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
    union <- st_union(st_make_valid(base_couche) )

    couche_union<- st_union(st_make_valid(couche_union) , union)

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


    #on gerenre le code des données
    code_data_couche = genrer_codes_data_couches(liste_couches)


    #On genere les couches de sybologie des données
    for (i in 1:length(liste_couches)) {

      #print(paste0(" je suis sur la couche ", i))

      #on récupère les informations de la couche courante
      couche_courant = liste_couches[i]
      name_couche <- names(couche_courant)

      ###Récupération des paramètres généraux################
      #on gère suivant la symbologie choisie
      selection <- paste0("liste_couches")
      texte_base <- liste_couches[[name_couche]]# paste("liste_couches", name_couche, "couche", sep = "$" )
      base_couche <- liste_couches[[name_couche]]$couche#  eval(parse(text = paste("liste_couches", name_couche, "couche", sep = "$" ) ))
      type_symbologie <- liste_couches[[name_couche]]$type_symbologie#  eval(parse(text = paste( "liste_couches", name_couche, "type_symbologie", sep = "$" ) ))
      geometrie <- liste_couches[[name_couche]]$geometrie# eval(parse(text = paste( "liste_couches", name_couche, "geometrie", sep = "$" ) )) #gemortie (pooijnt, ligne, polygone, etc)

      options_symbologie_couche <- liste_couches[[name_couche]]$options_symbologie_couche#  eval(parse(text = paste( "liste_couches", name_couche, "options_symbologie_couche", sep = "$" ) )) #les oprtions d ela symbologie de la couche



      #on doit se fixer sur les options de symbologie avant de laisser l'application travailler sur les couches
      switch (type_symbologie,
        "unique" = {
          options_symbologie_couche <-  options_symbologie_couche$options_symbologie_unique#  eval(parse(text = paste( "options_symbologie_couche", "options_symbologie_unique", sep = "$" ) ))
          couche_symbologies <- generer_code_type_symbologie_unique(paste0("data_couche", i), geometrie, options_symbologie_couche,i)
                  },
        "categorise"={
          #initialisation du code
          code_initial=""

          #on va generer le code selon la configuration des categories de couches de symbologie
          #L'ensemble des couches des catégories de symbologie
          couches_categories_symbologie= options_symbologie_couche$options_symbologie_categorise$categories
          colonne_categorie=options_symbologie_couche$options_symbologie_categorise$colonne_valeur_symbologie

          #on filtre les catégories visibles seulement
          couches_categories_symbologie_visibles <- Filter( function(x) x$visible==TRUE, couches_categories_symbologie )

          for (j in couches_categories_symbologie_visibles ) {
            #le label de la categorie dans la base
            valeur_categorie_courant=j$valeur

            #On applique le filetre ici sur la colonne
            name_couche_categorie=paste0(paste0("data_couche", i), " %>% filter(",colonne_categorie,'=="',valeur_categorie_courant,'")')

            symbologies_categorie_courant=j$couches_symbologies

            code_couche_categorie= generer_code_type_symbologie_unique(name_couche_categorie, geometrie, symbologies_categorie_courant,i)

            if(code_initial==""){
              code_initial=code_couche_categorie
            }else{
              code_initial=paste(code_initial, code_couche_categorie, sep = "+\n" )
            }


          }

          #on récupère le code final
          couche_symbologies <-  code_initial

        },
        "graduate"={
          #initialisation du code
          code_initial=""

          #on va generer le code selon la configuration des categories de couches de symbologie
          #L'ensemble des couches des catégories de symbologie
          couches_categories_symbologie= options_symbologie_couche$options_symbologie_graduee$categories
          colonne_categorie=options_symbologie_couche$options_symbologie_graduee$colonne_valeur_symbologie

          #on filtre les catégories visibles seulement
          couches_categories_symbologie_visibles <- Filter( function(x) x$visible==TRUE, couches_categories_symbologie )

          for (j in couches_categories_symbologie_visibles ) {
            #le label de la categorie dans la base
            minimum=j$minimum_intervalle
            maximum=j$maximum_intervalle

            #On applique le filetre ici sur la colonne
            name_couche_categorie=paste0(paste0("data_couche", i), " %>% filter(",colonne_categorie,' > ',minimum,' & ', colonne_categorie, '<=',maximum,')')

            symbologies_categorie_courant=j$couches_symbologies

            code_couche_categorie= generer_code_type_symbologie_unique(name_couche_categorie, geometrie, symbologies_categorie_courant,i)

            if(code_initial==""){
              code_initial=code_couche_categorie
            }else{
              code_initial=paste(code_initial, code_couche_categorie, sep = "+\n" )
            }


          }

          #on récupère le code final
          couche_symbologies <-  code_initial

        }

      )


      #print(options_symbologie_couche)

      #on récupéré les informations sur les effets


      ###Fin récupération des paramètres##########


      ##on on gnère d'abord les code selon qu'il ya ou non des effets




      ##Générer le code pour la symbologie choisie #####

      graph<- paste(graph, couche_symbologies, sep = "+" )



    }#fin de la gestion d'une couche individuelle


    graphique <-  paste(code_data_couche,graph, sep="\n" )

    resultat =list(
     # graphique= eval(parse(text = graphique)),
      code_graphique=graphique,
      ratio_hauteur=ratio_hauteur,
      code_couches_symbologie=graph,
      code_data=code_data_couche
    )


  return(resultat)


}


#Fonction qui génère le code de la partie des effets
#generer_code_symbologies
generer_code_type_symbologie_unique <- function(couche, geometrie, liste_symbologies_couche, ordre_couche ){

            code_effet <- ""

            names_couches_symbologies<- names(liste_symbologies_couche)

            print(names_couches_symbologies)

            #on parcourt chaque couche de symbologie  de la couche#####
            for (i in 1:length(names_couches_symbologies)) {

                #incrémentation de l'ordre
                #Couleur
                couleur_symbologie <- liste_symbologies_couche[[names_couches_symbologies[i] ]]$couleur_symbole #    eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "couleur_symbole", sep = "$" ) ))
                couleur_trait <- liste_symbologies_couche[[names_couches_symbologies[i] ]]$couleur_trait#  eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "couleur_trait", sep = "$" ) ))
                style_trait <-  liste_symbologies_couche[[names_couches_symbologies[i] ]]$style_trait# eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "style_trait", sep = "$" ) ))
                epaisseur_trait <-  eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "epaisseur_trait", sep = "$" ) ))

                #opacités
                #opacity_fill<- eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "opacity_fill", sep = "$" ) ))
                #opacity_border<- eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "opacity_border", sep = "$" ) ))


                #Le type de symbole de la couche
                style_fill_symbologie<- eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "style_fill_symbologie", sep = "$" ) ))


                #options d'effets

                statut_effet_couche <- eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "statut_effet", sep = "$" ) ))
                effets_couches <- eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "effects", sep = "$" ) ))#On récupère la liste des effets de la couche

                #on filtre seulement les couches aux effets visibles tels qu'indiqués par les options cohés par l'utilisateur
                effets_actifs <- effets_couches #Filter( function(x) x$options$checkec==TRUE, effets_couches )#Filter est une fonction de base de R

                #on applique d'abord les effers de la couche source
                effets_source <- eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "effects","source", sep = "$" ) ))

                #Caractéristiques des effets source
                #alpha=1, #Opacité
                #couleur="#000000",#la couleur de l'ombre
                #mode_fusion="multiply"#le mode de fusion de l'ombre (défaut sur multiply))

                alpha_source <-  eval(parse(text = paste("effets_source","options", "alpha", sep = "$" ) ))
                # couleur_source <-  eval(parse(text = paste( "effets_source ","options_symbologie_unique", "couleur_trait", sep = "$" ) ))
                mode_fusion_source <-  eval(parse(text = paste( "effets_source","options", "mode_fusion", sep = "$" ) ))


                #print(couleur_symbologie)

                ###on discrimine la gestion des couches ici en fonction du statut des effets####
                if(statut_effet_couche && length(effets_actifs)>0){##gestion avec effet############

                  #on recupère les informations sur la couche source
                  #effets_ligne_source <- eval(parse(text = paste( "effets_actifs","options","source", sep = "$" ) ))
                  #opacite_source <- eval(parse(text = paste( "effets_ligne_source","options","alpha", sep = "$" ) ))

                  # Gestion selon les type de géometrie de la couche
                  switch (geometrie,
                    "POLYGON" = {#Gestion des polygones

                                ##Controle du type de remplissage des couches######
                                switch(style_fill_symbologie,
                                       "continu"={
                                         couche_effet_source <-  paste0( 'geom_sf(data=',couche, ', linetype="',style_trait,'", colour="',couleur_trait,'", fill="',couleur_symbologie,'", linewidth=',epaisseur_trait, ', show.legend = "line" )  ')
                                       },
                                       "motif"={
                                         #on récupère les informations sur les patterns

                                         #on récupéré les informations sur les caractéristiques des patterns de la couche
                                         #### On récupère les paramètres
                                         pattern_couche <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern",  sep="$") ))
                                         pattern_spacing <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_spacing",  sep="$") ))
                                         pattern_angle <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_angle",  sep="$") ))
                                         pattern_size <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_size",  sep="$") ))
                                         pattern_colour <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_colour",  sep="$") ))
                                         pattern_linetype <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_linetype",  sep="$") ))
                                         pattern_fill2 <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_fill2",  sep="$") ))
                                         pattern_orientation <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_orientation",  sep="$") ))
                                         pattern_type <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_type",  sep="$") ))
                                         pattern_filename <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_filename",  sep="$") ))
                                         pattern_scale <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_scale",  sep="$") ))

                                         couche_effet_source <-  paste0('geom_sf_pattern(data=',couche,',
                                                        pattern="',pattern_couche,'",
                                                        pattern_spacing=', pattern_spacing, ',
                                                        pattern_angle=',pattern_angle, ' ,
                                                        pattern_size=',pattern_size, ',
                                                        pattern_colour="', paste0(pattern_colour),'",
                                                        pattern_linetype="', paste0(pattern_linetype), '",
                                                        linetype="',style_trait,'", colour="',couleur_trait,'", linewidth=',epaisseur_trait, ', show.legend = "line"
                                                        )'
                                         )


                                       }

                                )



                    },
                    "LINESTRING"={

                      ##Controle du type de remplissage des couches######
                      switch(style_fill_symbologie,
                             "ligne_simple"={
                               couche_effet_source <-  paste0( 'geom_sf(data=st_simplify(',couche, '), linetype="',style_trait,'", colour="',couleur_trait,'", linewidth=',epaisseur_trait, ', show.legend = "line" )  ')
                             }


                      )


                    }
                  )#fin de l'exception selon le type de géométrie de la couche






                  ##Couche de refeence
                  couche_reference <- paste0(
                    "as_reference(
                        ",paste0(couche_effet_source ), ",
                        id= '",paste0('reference',ordre_couche, "_", i ),"'
                      )"
                  )

                  #On initialise à couche de reference
                  code_effet<- paste0( couche_reference )


                  names_effet <- setdiff( names(effets_actifs), c("source")  )

                  #print("names")
                  #print(names_effet)

                  #print(effets_actifs)


                  #on parcours les sources un à un
                  for (j in 1:length(names_effet) ) {

                    #print(names_effet[i])

                    switch (names_effet[i],
                            "source"={#effet de la couche source
                              effets_ligne <- eval(parse(text = paste( "effets_actifs","options",paste0(names_effet[j]), sep = "$" ) ))
                              mode_fusion <- eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

                              code_effet_couche <- paste0(
                                "with_blend(
                          ",paste0(couche_effet_source), ",
                          blend_type = ", paste0(mode_fusion), "
                        )"
                              )

                              # code_effet<- paste(code_effet, code_effet_couche , sep = "+" )

                            },

                            "drop_shadow_portee" = {#ombre de portée
                              #on recupere les caractéristiques
                              effets_ligne <- eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              #print("Actif")
                              #print(effets_ligne)

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
                       bg_layer = '",paste0('reference',ordre_couche, "_", i ),"',
                      blend_type = '",paste0(mode_fusion),"',
                      stack = TRUE
                    )"
                              )

                              if(code_effet==""){
                                code_effet<-code_effet_couche
                              }else{
                                code_effet<- paste(code_effet, code_effet_couche , sep = "+" )
                              }



                            },
                            "drop_shadow_interieure"={
                              #on recupere les caractéristiques
                              effets_ligne <- eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              #print("Actif")
                              #print(effets_ligne)

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
                                filter = 'inside',
                                sigma = ",sigma,",
                                x_offset = -8 ,
                                y_offset =8,
                                colour=alpha( '",paste0(couleur),"', ",alpha,"   ),
                              ),
                               bg_layer = '",paste0('reference',ordre_couche, "_", i ),"',
                              blend_type = '",paste0(mode_fusion),"',
                              stack = TRUE
                            )"
                              )

                              if(code_effet==""){
                                code_effet<-code_effet_couche
                              }else{
                                code_effet<- paste(code_effet, code_effet_couche , sep = "+" )
                              }


                            },
                            "innner_glow"={
                              #on recupere les caractéristiques
                              effets_ligne <- eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              rayon <- eval(parse(text = paste( "effets_ligne","options","rayon", sep = "$" ) ))
                              sigma <- eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
                              alpha <- eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
                              couleur <- eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
                              mode_fusion <- eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

                              code_effet_couche <- paste0("with_blend(
                          with_inner_glow(
                            ",paste0(couche_effet_source), ",
                            sigma=",sigma,",
                            colour= '",couleur,"',
                            expand = ",rayon," ,
                            x_offset=0,
                            y_offset=0,
                            alpha=",alpha, "
                            ),
                            bg_layer = '",paste0('reference',ordre_couche, "_", i ),"',
                            blend_type = '",paste0(mode_fusion),"',
                            stack = TRUE)")


                              if(code_effet==""){
                                code_effet<-code_effet_couche
                              }else{
                                code_effet<- paste(code_effet, code_effet_couche , sep = "+" )
                              }


                            },
                            "outer_glow"={
                              #on recupere les caractéristiques
                              effets_ligne <- eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              rayon <- eval(parse(text = paste( "effets_ligne","options","rayon", sep = "$" ) ))
                              sigma <- eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
                              alpha <- eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
                              couleur <- eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
                              mode_fusion <- eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

                              code_effet_couche <- paste0("with_blend(
                          with_outer_glow(
                            ",paste0(couche_effet_source), ",
                            sigma=",sigma,",
                            colour= '",couleur,"',
                            expand = ",rayon," ,
                            x_offset=0,
                            y_offset=0,
                            alpha=",alpha, "
                            ),
                            bg_layer = '",paste0('reference',ordre_couche, "_", i ),"',
                            blend_type = '",paste0(mode_fusion),"',
                            stack = TRUE)")


                              if(code_effet==""){
                                code_effet<-code_effet_couche
                              }else{
                                code_effet<- paste(code_effet, code_effet_couche , sep = "+" )
                              }


                            }






                    )#fin switch sur les types de mode de fusion


                  }#fin du parcours des couches d'effets









                }else{
                  ###########gestion sans effet######################


                 # print(style_fill_symbologie)

                  switch (geometrie,
                      "POLYGON" = {

                              ##Controle du type de remplissage des couches######
                              switch(style_fill_symbologie,
                                     "continu"={
                                       couche_symbologie <-  paste0( 'geom_sf(data=st_simplify(',couche, '), linetype="',style_trait,'", colour="',couleur_trait,'", fill="',couleur_symbologie,'", linewidth=',epaisseur_trait, ', show.legend = "line" )  ')
                                     },
                                     "motif"={
                                       #on récupère les informations sur les patterns

                                       #on récupéré les informations sur les caractéristiques des patterns de la couche
                                       #### On récupère les paramètres
                                       pattern_couche <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern",  sep="$") ))
                                       pattern_spacing <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_spacing",  sep="$") ))
                                       pattern_density <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_density",  sep="$") ))
                                       pattern_angle <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_angle",  sep="$") ))
                                       pattern_size <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_size",  sep="$") ))
                                       pattern_colour <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_colour",  sep="$") ))
                                       pattern_linetype <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_linetype",  sep="$") ))
                                       pattern_fill2 <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_fill2",  sep="$") ))
                                       pattern_orientation <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_orientation",  sep="$") ))
                                       pattern_type <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_type",  sep="$") ))
                                       pattern_filename <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_filename",  sep="$") ))
                                       pattern_scale <- eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_scale",  sep="$") ))


                                       if(pattern_couche=="stripe"){
                                         couche_symbologie <-  paste0('geom_sf_pattern(data=',couche,',
                                                        pattern="',pattern_couche,'",
                                                        pattern_spacing=', pattern_spacing, ',
                                                        pattern_angle=',pattern_angle, ' ,
                                                        pattern_size=',pattern_size, ',
                                                        pattern_colour="', paste0(pattern_colour),'",
                                                        pattern_linetype="', paste0(pattern_linetype), '",
                                                        linetype="',style_trait,'", colour="',couleur_trait,'", linewidth=',epaisseur_trait, ', show.legend = "line"
                                                        )'
                                         )

                                       }else if( pattern_couche %in% c("crosshatch", "circle", "wave")){
                                         couche_symbologie <-  paste0('geom_sf_pattern(data=',couche,',
                                                        pattern="',pattern_couche,'",
                                                        pattern_spacing=', pattern_spacing, ',
                                                        pattern_density=', pattern_density, ',
                                                        pattern_angle=',pattern_angle, ' ,
                                                        pattern_size=',pattern_size, ',
                                                        pattern_colour="', paste0(pattern_colour),'",
                                                        pattern_fill="',couleur_symbologie,'",
                                                        pattern_linetype="', paste0(pattern_linetype), '",
                                                        linetype="',style_trait,'", colour="',couleur_trait,'", linewidth=',epaisseur_trait, ', show.legend = "line"
                                                        )'
                                         )

                                       }else if(pattern_couche=="gradient"){
                                         couche_symbologie <-  paste0('geom_sf_pattern(data=',couche,',
                                                        pattern="',pattern_couche,'",
                                                        pattern_fill="',couleur_symbologie,'",
                                                        pattern_fill2="',pattern_fill2,'",
                                                        pattern_orientation="', paste0(pattern_orientation), '",
                                                        linetype="',style_trait,'", colour="',couleur_trait,'", linewidth=',epaisseur_trait, ', show.legend = "line"
                                                        )'
                                         )
                                       }






                                     }

                              )


                      },
                      "LINESTRING"={

                        print("On est ici pour les lignes sans effet")
                            ##Controle du type de remplissage des couches######
                            switch(style_fill_symbologie,
                                   "ligne_simple"={
                                     couche_symbologie <-  paste0( 'geom_sf(data=st_simplify(',couche, '), linetype="',style_trait,'", colour="',couleur_trait,'", linewidth=',epaisseur_trait, ', show.legend = "line" )  ')
                                   }

                            )

                      },
                      "POINT"={

                      }
                  )




                  #La couche de remplissage de la carte de base
                  #if(style_fill_symbologie=="continu"){####Gestion du remplissage uni ou continu des symbologies####
                  #couche_symbologie <-  paste0( 'geom_sf(data=',couche, ', linetype="',style_trait,'", colour="',couleur_trait,'", fill="',couleur_symbologie,'", linewidth=',epaisseur_trait, ', show.legend = "line" )  ')
                  #}


                  if(code_effet==""){
                    code_effet<- couche_symbologie
                  }else{
                    code_effet <- paste(code_effet, couche_symbologie, sep = "+" )
                  }

                }#fin de la discrimination de gestion selon le statut d'activation ou non des effets de la symbologie des couches



                #}#fin de la gestion avec les couhes continus





            }#fin du parcours de la liste des couches de symbologie de la couche en cours pour la couche unique


  code_effet <- paste0(code_effet)

  return(code_effet)

}


#Generer le code qui permet d'obtenir les datas des couches en prenant en compte la configuration des jointures
genrer_codes_data_couches <- function(liste_couches) {

  code =""


  for (i in liste_couches) {#On parccourt les couches

      introduction=paste0("#Données sur le couche : ", i$name)

      couche= paste0( paste0("data_couche", i$position)," =", i$name )#on prend les données de la couche


      if(length(i$jointures)==0){

        code =paste0(code,"\n\n\n",
                     introduction, "\n",
                     couche, "\n\n\n"
               )

      }else{

          for (j in i$jointures) {#On pacourt toute les jointures de un à 1

            table_intermédiaire=paste0(paste0("table_intermediaire_jointure",i$position), "=", j$name_table)

            nommer_table_intermédiaire=paste0("names(",paste0("table_intermediaire_jointure", i$position), ") <- paste('",j$name_table,"', snakecase::to_snake_case(names(",paste0("table_intermediaire_jointure", i$position),")), sep = '.' )")

            merge_tables=paste0(paste0("data_couche", i$position), " <- merge(",paste0("data_couche", i$position),",", paste0("table_intermediaire_jointure", i$position), ", by.x='",j$colonne_couche_cible,"', by.y='",paste(j$name_table, j$colonne_table,sep = '.' ), "' )" )

            code= paste0(code,"\n",
                         introduction, "\n",
                         couche, "\n",
                         #jointures,"\n",
                         table_intermédiaire, "\n",
                         nommer_table_intermédiaire, "\n",
                         merge_tables, "\n\n"
                    )



          }
      }


  }#fin for  sur la liste des couches


  return(code)


}



zone_impression_carte <- function(orientation_carte="paysage", largeur_dimension=5.8, hauteur_dimension=3.3, fond="#ffffff"){



  theme_plot <-  paste0('theme(
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
  plot.margin = unit(c(0, 0, 0, 0), "lines" ),

  panel.background=element_rect(fill="', fond, '"),
  plot.background=element_rect(fill="', fond, '") )


  ')



  cadre <- ggplot() +
    coord_equal(xlim = c(0, largeur_dimension), ylim = c(0, hauteur_dimension), expand = TRUE) #Travailler avec une image HD sur 16:9

  graph<-cadre + eval(parse(text = theme_plot  ))
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
          #print("Nouvelle generation")
          graph_obj <- generer_map(liste_couches ) #on va après voir comment rendre le thème dynamique

          graph<- eval(parse(text = graph_obj$code_graphique )) +eval(parse(text = theme ))

          ratio_hauteur_graph <- graph_obj$ratio_hauteur

          #essai d'impression du code
          #print(graph)

          #la différence de hauteur du graphique
          diff_hauteur_obj <- ratio_hauteur_graph*longeur_box
          diff_largeur_obj <- ratio*diff_hauteur_obj


          #la zone d'impression de la carte
          zone_impression <- zone_impression_carte(orientation_carte = "paysage", largeur_dimension = largeur, hauteur_dimension = hauteur, theme_carte = eval(parse(text = paste0(theme))) )

          #on procède à l'ajout des éléments à la zone d'impression
          ## la carte principale
          mon_graphique <- combiner_cartes(zone_impression, graph, xmin = -0.8, xmax = largeur+0.8, ymin = 0, ymax = hauteur+0.2 )





          resultat =list(
            mon_graphique=mon_graphique,
            ratio=ratio,
            code_graphique= paste(graph_obj$code_graphique, paste0(theme), sep = "+")
          )

  }else{
    #la zone d'impression de la carte
    mon_graphique <- zone_impression_carte(orientation_carte = "paysage", largeur_dimension = largeur, hauteur_dimension = hauteur, theme_carte =  eval(parse(text = paste0(theme))) )


    resultat =list(
      mon_graphique=mon_graphique,
      ratio=ratio,
      code_graphique=""
    )

  }


      return(resultat)

}


