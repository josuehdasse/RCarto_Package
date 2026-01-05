#Equation llineaire des dorites padant par deux points
equation_lineaire_droite <- function(xa, ya, xb, yb) {
    coef_a = (ya- yb) / (xa - xb)
    coef_b = ya - coef_a*xa

    resultat=list(
      coef_a=coef_a,
      coef_b=coef_b
    )

}




#réunion  des couches mobilisés pour former le box de la zone de carte
reunion_couches <- function(liste_couches){

  couche_1 = liste_couches[1]

  name_couche_1<-names(couche_1)
  base_couche_1 <- couche_1[[name_couche_1]]$couche# eval(parse(text = paste( "couche_1", name_couche_1, "couche", sep = "$" ) ))

  couche_union <- st_union(base_couche_1)

  for (i in liste_couches) {
    name_couche_courant<-i$name_objet #la réunion se fait sur les objets qui sont derrière les couches
    base_couche <- i$couche #couche_courant   eval(parse(text = paste( "couche_courant",name_couche_courant, "couche", sep = "$" ) ))
    union <- st_union(st_make_valid(base_couche) )

    couche_union<- st_union(st_make_valid(couche_union) , union)

  }

  return(couche_union)
}


#fonction qui produit le code du map selon les configurations des listes
#le map est généré ici à la base des couches qui sont dans le système
generer_map <- function(liste_couches, box_emprise){

  #lNB: Les box seront les r;eunions inividuelels des couches


  #initialisation du graphique
  graph<- paste0( 'ggplot()')

  ratio_hauteur=1#on initialise le ration de la hauteur à 1 (s'il n'ya pas de titre qui vienne modifier la donne)


    #on gerenre le code des données
    code_data_couche = genrer_codes_data_couches(liste_couches, box_emprise)


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

      #les options d'etiquettes de la couche
      options_etiquettes_couche <- liste_couches[[name_couche]]$options_etiquette
      type_etiquette_couche=options_etiquettes_couche$type



      #on doit se fixer sur les options de symbologie avant de laisser l'application travailler sur les couches
      switch (type_symbologie,
        "unique" = {
          options_symbologie_couche <-  options_symbologie_couche$options_symbologie_unique#  eval(parse(text = paste( "options_symbologie_couche", "options_symbologie_unique", sep = "$" ) ))
          couche_symbologies <- generer_code_type_symbologie_unique(paste0("data_couche", i), geometrie, options_symbologie_couche,i)
          couche_etiquettes <- generer_codes_couche_etiquette_unique(couche =  paste0("data_couche", i), options_etiquette_couche =  options_etiquettes_couche, mode = 1, i)



          },
        "categorise"={
          #initialisation du code
          code_initial=""
          code_etiquette_initial=""

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

            couche_etiquettes_categorie <- generer_codes_couche_etiquette_unique( couche =  name_couche_categorie, options_etiquette_couche =  options_etiquettes_couche, mode = 1, i)

            if(code_initial==""){
              code_initial=code_couche_categorie
            }else{
              code_initial=paste(code_initial, code_couche_categorie, sep = "+\n" )
            }

            if(code_etiquette_initial==""){
              code_etiquette_initial=couche_etiquettes_categorie
            }else{
              code_etiquette_initial=paste(code_etiquette_initial,couche_etiquettes_categorie,sep = "+\n"  )
            }


          }

          #on récupère le code final
          couche_symbologies <-  code_initial
          couche_etiquettes <- code_etiquette_initial

        },
        "graduate"={
          #initialisation du code
          code_initial=""
          code_etiquette_initial=""

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

            IncInf=j$fermeture_borne_inf
            IncSup=j$fermeture_borne_sup

            if(IncInf==TRUE){
              InegaliteInf=">="
            }else{
              InegaliteInf=">"
            }

            if(IncSup==TRUE){
              InegaliteSup="<="
            }else{
              InegaliteSup="<"
            }

            #On applique le filetre ici sur la colonne
            name_couche_categorie=paste0(paste0("data_couche", i), " %>% filter(",colonne_categorie," ", InegaliteInf ,minimum,' & ', colonne_categorie," ", InegaliteSup ,maximum,')')


            symbologies_categorie_courant=j$couches_symbologies

            code_couche_categorie= generer_code_type_symbologie_unique(name_couche_categorie, geometrie, symbologies_categorie_courant,i)
            couche_etiquettes_categorie <- generer_codes_couche_etiquette_unique(name_couche_categorie, options_etiquettes_couche,1,i)

            if(code_initial==""){
              code_initial=code_couche_categorie
            }else{
              code_initial=paste(code_initial, code_couche_categorie, sep = "+\n" )
            }

            if(code_etiquette_initial==""){
              code_etiquette_initial=couche_etiquettes_categorie
            }else{
              code_etiquette_initial=paste(code_etiquette_initial,couche_etiquettes_categorie,sep = "+\n"  )
            }


          }

          #on récupère le code final
          couche_symbologies <-  code_initial
          couche_etiquettes <- code_etiquette_initial


        }

      )





      #print(options_symbologie_couche)

      #on récupéré les informations sur les effets


      ###Fin récupération des paramètres##########


      ##on on gnère d'abord les code selon qu'il ya ou non des effets




      ##Générer le code pour la symbologie choisie #####
      switch (type_etiquette_couche,
        "pas_detiquettes" = {
          graph<- paste(graph, couche_symbologies, sep = "+\n" )
        },
        "etiquettes_simples"={
          graph<- paste(graph, couche_symbologies, couche_etiquettes, sep = "+\n" )
        }
      )




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
                epaisseur_trait <- liste_symbologies_couche[[names_couches_symbologies[i] ]]$epaisseur_trait#  eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "epaisseur_trait", sep = "$" ) ))


                #Le type de symbole de la couche
                style_fill_symbologie<-liste_symbologies_couche[[names_couches_symbologies[i] ]]$style_fill_symbologie#  eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "style_fill_symbologie", sep = "$" ) ))


                #options d'effets

                statut_effet_couche <-liste_symbologies_couche[[names_couches_symbologies[i] ]]$statut_effet#   eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "statut_effet", sep = "$" ) ))
                effets_couches <- liste_symbologies_couche[[names_couches_symbologies[i] ]]$effects#  eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "effects", sep = "$" ) ))#On récupère la liste des effets de la couche

                #on filtre seulement les couches aux effets visibles tels qu'indiqués par les options cohés par l'utilisateur
                effets_actifs <- effets_couches #Filter( function(x) x$options$checkec==TRUE, effets_couches )#Filter est une fonction de base de R

                #on applique d'abord les effers de la couche source
                effets_source <- liste_symbologies_couche[[names_couches_symbologies[i] ]]$effects$source#   eval(parse(text = paste( "liste_symbologies_couche", names_couches_symbologies[i], "effects","source", sep = "$" ) ))

                #Caractéristiques des effets source


                alpha_source <- effets_source$options$alpha# eval(parse(text = paste("effets_source","options", "alpha", sep = "$" ) ))
                # couleur_source <-  eval(parse(text = paste( "effets_source ","options_symbologie_unique", "couleur_trait", sep = "$" ) ))
                mode_fusion_source <-effets_source$options$mode_fusion# eval(parse(text = paste( "effets_source","options", "mode_fusion", sep = "$" ) ))


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
                                         pattern_couche <-liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern# eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern",  sep="$") ))
                                         pattern_spacing <-  liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_spacing#          eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_spacing",  sep="$") ))
                                         pattern_angle <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_angle#          eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_angle",  sep="$") ))
                                         pattern_size <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_size#            eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_size",  sep="$") ))
                                         pattern_colour <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_colour#           eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_colour",  sep="$") ))
                                         pattern_linetype <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_linetype#         eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_linetype",  sep="$") ))
                                         pattern_fill2 <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_fill2#           eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_fill2",  sep="$") ))
                                         pattern_orientation <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_orientation#         eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_orientation",  sep="$") ))
                                         pattern_type <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_type#           eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_type",  sep="$") ))
                                         pattern_filename <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_filename#         eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_filename",  sep="$") ))
                                         pattern_scale <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_scale#        eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_scale",  sep="$") ))

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
                              effets_ligne <- effets_actifs$options[[paste0(names_effet[j])]] #eval(parse(text = paste( "effets_actifs","options",paste0(names_effet[j]), sep = "$" ) ))
                              mode_fusion <-effets_ligne$options$mode_fusion# eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

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
                              effets_ligne <- effets_actifs[[paste0(names_effet[j])]]# eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              #print("Actif")
                              #print(effets_ligne)

                              angle <-effets_ligne$options$angle# eval(parse(text = paste( "effets_ligne","options","angle", sep = "$" ) ))
                              distance <- effets_ligne$options$distance#    eval(parse(text = paste( "effets_ligne","options","distance", sep = "$" ) ))
                              sigma <- effets_ligne$options$sigma# eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
                              alpha <- effets_ligne$options$alpha# eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
                              couleur <- effets_ligne$options$couleur#  eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
                              mode_fusion <- effets_ligne$options$mode_fusion#    eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

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
                              effets_ligne <- effets_actifs[[paste0(names_effet[j])]]# eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              #print("Actif")
                              #print(effets_ligne)

                              angle <-effets_ligne$options$angle# eval(parse(text = paste( "effets_ligne","options","angle", sep = "$" ) ))
                              distance <- effets_ligne$options$distance#    eval(parse(text = paste( "effets_ligne","options","distance", sep = "$" ) ))
                              sigma <- effets_ligne$options$sigma# eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
                              alpha <- effets_ligne$options$alpha# eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
                              couleur <- effets_ligne$options$couleur#  eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
                              mode_fusion <- effets_ligne$options$mode_fusion#    eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

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
                              effets_ligne <- effets_actifs[[paste0(names_effet[j])]]# eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              rayon <- effets_ligne$options$rayon#   eval(parse(text = paste( "effets_ligne","options","rayon", sep = "$" ) ))
                              sigma <- effets_ligne$options$sigma#   eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
                              alpha <- effets_ligne$options$alpha#   eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
                              couleur <- effets_ligne$options$couleur#   eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
                              mode_fusion <- effets_ligne$options$mode_fusion#   eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

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
                              effets_ligne <- effets_actifs[[paste0(names_effet[j])]]# eval(parse(text = paste( "effets_actifs",paste0(names_effet[j]), sep = "$" ) ))

                              rayon <- effets_ligne$options$rayon#   eval(parse(text = paste( "effets_ligne","options","rayon", sep = "$" ) ))
                              sigma <- effets_ligne$options$sigma#   eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
                              alpha <- effets_ligne$options$alpha#   eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
                              couleur <- effets_ligne$options$couleur#   eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
                              mode_fusion <- effets_ligne$options$mode_fusion#   eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))


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

                                       #### On récupère les paramètres
                                       pattern_couche <-liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern# eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern",  sep="$") ))
                                       pattern_spacing <-  liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_spacing#          eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_spacing",  sep="$") ))
                                       pattern_angle <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_angle#          eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_angle",  sep="$") ))
                                       pattern_size <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_size#            eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_size",  sep="$") ))
                                       pattern_colour <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_colour#           eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_colour",  sep="$") ))
                                       pattern_linetype <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_linetype#         eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_linetype",  sep="$") ))
                                       pattern_fill2 <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_fill2#           eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_fill2",  sep="$") ))
                                       pattern_orientation <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_orientation#         eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_orientation",  sep="$") ))
                                       pattern_type <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_type#           eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_type",  sep="$") ))
                                       pattern_filename <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_filename#         eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_filename",  sep="$") ))
                                       pattern_scale <- liste_symbologies_couche[[names_couches_symbologies[i]]]$patterns$pattern_scale#        eval(parse(text = paste("liste_symbologies_couche",names_couches_symbologies[i],  "patterns", "pattern_scale",  sep="$") ))

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



#Generer le code qui permet de gerer les box des couches##########
scalaire_vecteurs <- function(xa, ya, xb, yb) {

  resultat= xa*xb + ya*yb

  return(resultat)
}

#calcul de la distance
distance_points <-function(xa,ya,xb,yb){
  resultat=sqrt( (xa-xb)^2 + (ya-yb)^2 )

  return(resultat)
}


generer_code_data_box_couches <-function(graph, liste_couches) {

  for (i in liste_couches) {
      statut_box_couche = i$box$statut_activation

      if(statut_box_couche){

            liste_box=i$box$liste_box

            for (j in liste_box) {

              #on construit le polygone
              contours=rbind(
                c(j$xmin, j$ymax), c(j$xmax, j$ymax), c(j$xmax, j$ymin), c(j$xmin, j$ymin), c(j$xmin, j$ymax)
              )

              polygone <- st_polygon(list(contours))

              sfc_obj <- st_sfc(list(polygone))

              data_poly <- data.frame(label=j$label, contenu=j$contenu)
              data_poly$geometry <- sfc_obj
              data_poly <- st_as_sf(data_poly)  %>%  st_set_crs(4326 )


              #On construit les connecteurs
              #d'abord ceux de la droite
              if(j$cote=="d"){
                scalaire=scalaire_vecteurs(0, j$ymin-j$xmax, j$xmin-j$x_centroide, (j$ymin + j$ymax)/2 - j$y_centroide)

                if(scalaire==0){
                  ligne=rbind(
                    c(j$x_centroide, j$y_centroide), c(j$xmin, (j$ymin + j$ymax)/2)
                  )
                }else{

                  #on distingue les pairs des impairs et normaux
                  y_milieu=(j$ymin + j$ymax)/2 - 0.08
                  #distance_box_proj=

                  ligne=rbind(
                    c(j$x_centroide, j$y_centroide), c(j$x_centroide,y_milieu  ), c(j$xmin, y_milieu  )
                  )


                }

                obj_ligne=st_linestring(ligne)
                obj_ligne_final = st_sfc(list(obj_ligne))

                data_ligne <- data.frame(label=c("A"))
                data_ligne$geometry <- obj_ligne_final
                data_ligne <- st_as_sf(data_ligne)  %>%  st_set_crs(4326 )

              }



              #On ajoute le polygone
              graph <- graph + geom_sf(data = data_poly, colour="#808080", fill=NA, linewidth=3 )
              #On joint les connecteurs
              graph <- graph + geom_sf(data = data_ligne, colour="#808080", linetype="11", linewidth=2.5)

              #on ajoute les textes du contenu

             graph <- graph + geom_sf_text(
               data=data_poly, aes(label=contenu), size=14,hjust=0, nudge_x=-0.46
             )





            }




      }





  }

  return(graph)


}

#Generer le code qui permet d'obtenir les datas des couches en prenant en compte la configuration des jointures
genrer_codes_data_couches <- function(liste_couches, box_emprise) {


  code =""

  for (i in liste_couches) {#On parccourt les couches

      introduction=paste0("#Données sur le couche : ", i$name)

      couche= paste0( paste0("data_couche", i$position)," =", i$name_objet )#on prend les données de la couche lié à l'objet utilisé dans l'environnement


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


  #on traitel'emprise
  #on generer le box de l'emprise de la carte
  couche_emprise <- paste0('box_emprise_couche = st_polygon(
        list(
          rbind(
            c(',box_emprise$xmin,', ',box_emprise$ymin,'),
            c(',box_emprise$xmin,', ',box_emprise$xmax,'),
            c(',box_emprise$xmax,', ',box_emprise$ymax,'),
            c(',box_emprise$xmax,', ',box_emprise$ymin,'),
            c(',box_emprise$xmin,', ',box_emprise$ymin,')
          )
        )
      ) %>% st_sfc() %>% st_as_sf()  %>% st_set_crs(4326)')



    code_emprise =""

    for (k in liste_couches) {

        couche_intersection = paste0(' ',paste0("data_couche",i$position), ' <- st_intersection( ',paste0("data_couche",i$position), ', box_emprise_couche)')

        code_emprise <- paste(code_emprise, couche_intersection, sep = "\n")
    }


    code_final= paste0(code, "#Emprise des couches", "\n", couche_emprise, "\n",  code_emprise)


  return(code_final)


}




#Générer une couche d'étiquette sur une couche
generer_codes_couche_etiquette_unique <- function(couche, options_etiquette_couche, mode=NULL, ordre_couche){

    #On récupere les options de la couche
    if(!is.null(mode)){
      if(mode=="preview"){
        colonne="titre"
      }else{
        colonne=options_etiquette_couche$colonne
      }
    }else{
      colonne=options_etiquette_couche$colonne
    }

    taille=options_etiquette_couche$taille
    couleur=options_etiquette_couche$couleur
    police=options_etiquette_couche$police
    style=options_etiquette_couche$style

    #Statut des tampons
    statut_tampon= options_etiquette_couche$tampon$statut

    #Statut des ombres
    statut_ombre= options_etiquette_couche$ombre$statut

    #Statut du background
    statut_background = options_etiquette_couche$background$statut

        if(statut_background){
            fill=options_etiquette_couche$background$fill
            alpha=options_etiquette_couche$background$alpha
            arrondi=options_etiquette_couche$background$arrondi
            label_size=options_etiquette_couche$background$label_size
            label_padding=options_etiquette_couche$background$label_padding
            border_colour=options_etiquette_couche$background$border_colour

            code_reference_simple = paste0("geom_sf_label(data = ",couche,", aes(label=",colonne,"), alpha=",alpha,", fill= '",fill,"', label.r=unit(",arrondi,", 'lines'), text.colour='",couleur,"', label.size=",label_size,", label.padding=unit(",label_padding,", 'lines'), border.colour='",border_colour,"' )")

        }else{
          code_reference_simple = paste0("geom_sf_text(data = ",couche,", aes(label=",colonne,"), size=",taille,", colour= '",couleur,"', family='",police,"')")
        }



        couche_reference_effets <- paste0(
          "as_reference(
                            ",paste0(code_reference_simple ), ",
                            id= '",paste0('reference',ordre_couche),"'
                          )"
        )


    #on traite le cas des couches tampons

    if(statut_tampon | statut_ombre ) {
        code_initial_effet=""

      if(statut_tampon){

        #Les parametres du tampon
        rayon=options_etiquette_couche$tampon$rayon
        sigma=options_etiquette_couche$tampon$sigma
        alpha=options_etiquette_couche$tampon$alpha
        couleur=options_etiquette_couche$tampon$couleur
        mode_fusion=options_etiquette_couche$tampon$mode_fusion

        code_effet_tampon <- paste0("with_blend(
                          with_outer_glow(
                            ",code_reference_simple, ",
                            sigma=",sigma,",
                            colour= '",paste0(couleur),"',
                            expand = ",rayon," ,
                            x_offset=0,
                            y_offset=0,
                            alpha=",alpha, "
                            ),
                            bg_layer = '",paste0('reference',ordre_couche ),"',
                            blend_type = '",paste0(mode_fusion),"',
                            stack = TRUE)")

        if(code_initial_effet==""){
          code_initial_effet <- paste(couche_reference_effets,code_effet_tampon, sep = "+\n" )
        }else{
          code_initial_effet <- paste(code_initial_effet,code_effet_tampon, sep = "+\n" )
        }

      }

        if(statut_ombre){

          angle <-options_etiquette_couche$ombre$angle# eval(parse(text = paste( "effets_ligne","options","angle", sep = "$" ) ))
          distance <- options_etiquette_couche$ombre$distance#    eval(parse(text = paste( "effets_ligne","options","distance", sep = "$" ) ))
          sigma <- options_etiquette_couche$ombre$sigma# eval(parse(text = paste( "effets_ligne","options","sigma", sep = "$" ) ))
          alpha <- options_etiquette_couche$ombre$alpha# eval(parse(text = paste( "effets_ligne","options","alpha", sep = "$" ) ))
          couleur <- options_etiquette_couche$ombre$couleur#  eval(parse(text = paste( "effets_ligne","options","couleur", sep = "$" ) ))
          mode_fusion <- options_etiquette_couche$ombre$mode_fusion#    eval(parse(text = paste( "effets_ligne","options","mode_fusion", sep = "$" ) ))

          code_effet_ombre <- paste0(
            "with_blend(
                  with_shadow(
                    ",paste0(code_reference_simple), ",
                    sigma = ",sigma,",
                    x_offset = ",distance*cos(angle*pi/180)," ,
                    y_offset =",distance*sin(angle*pi/180),",
                    colour=alpha( '",paste0(couleur),"', ",alpha,"   ),
                  ),
                    bg_layer = '",paste0('reference',ordre_couche ),"',
                    blend_type = '",paste0(mode_fusion),"',
                    stack = TRUE
                    )"
          )

          if(code_initial_effet==""){
            code_initial_effet <- paste(couche_reference_effets,code_effet_ombre, sep = "+\n" )
          }else{
            code_initial_effet <- paste(code_initial_effet,code_effet_ombre, sep = "+\n" )
          }


        }




        code_reference = code_initial_effet

    }else{#Pas d'effet sur les couces d'etiquettes
      code_reference = code_reference_simple
    }





    code_texte = code_reference

    print(code_texte)

    return(code_texte)
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


