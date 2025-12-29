

  showModal(modalDialog(
    title = "Importer une couche",
    footer=tagList(
      div(class="div_footer_modal",
          uiOutput(ns("bouton_confirmer_ajout_layer_ui")),
          modalButton("Annuler")
      )

    ),

    withSpinner(
      uiOutput(ns("options_import_layer_ui")) )
    ,#on paramètre le contenu de la fenêtre cible ici
    #trigger = "ajouter_couche",
    size="m",
    easyClose = TRUE
  ))







  data_graph <- rbind( c(0.0), c(1,0), c(1,1), c(0,1), c(0,0) )#les données du graphique  ==>> la couche
  polygone <- st_polygon(list(data_graph))

  sfc_obj <- st_sfc( list(polygone))
  data_points <- data.frame(titre=c("Titre d'Essai"))#Le mot d'essai
  data_points$geometry <- sfc_obj
  data_points <- st_as_sf(data_points)


  ggplot()+
    geom_sf(data = data_points )+
    geom_sf_text(data = data_points, aes(label=titre), size=10, colour= '#000000', family='Arial')




  ggplot()+
  geom_sf(data=data_points, fill=NA, colour="#000000", linewidth=1) +
  as_reference(
          geom_sf_text(data = data_points, aes(label=titre), size=10, colour= '#000000', family='Arial'),
          id= 'reference1'
          )+
          with_blend(
                with_outer_glow(
                    geom_sf_text(data = data_points, aes(label=titre), size=10, colour= '#000000', family='Arial'),
                    sigma=6,
                    colour= 'blue',
                    expand = 10 ,
                    x_offset=0,
                    y_offset=0,
                    alpha=1
                    ),
                    bg_layer = 'reference1',
                    blend_type = 'over',
                    stack = TRUE)








