#Fonction qui va serveir à creer la liste des couleurs en se servant du package RBrewerColor

liste_couleurs_brewer <- function(){
  noms_couleurs <- rownames(brewer.pal.info)

  data_couleurs <- brewer.pal.info

  #initialisation
  liste_couleurs <- c()

  for (i in 1:nrow(data_couleurs)) {

    couleurs <- brewer.pal( data_couleurs[i,1 ], noms_couleurs[i])
    liste_couleurs <- c(liste_couleurs, couleurs)

  }


  return(liste_couleurs)

}
