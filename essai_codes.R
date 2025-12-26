#Type de ligne du motif
req(input$select_pattern_orientation)
pattern_orientation_actif(input$select_pattern_orientation)
updateSelectInput(session, "select_pattern_orientation", label = NULL, choices = list("Radial"="radial",
                                                                                      "Horizontal"="horizontal",
                                                                                      "Vertical"="vertical",
                                                                                      "Diagonal"="diagonal"), selected  =  pattern_orientation_actif() )
gauche_pattern_orientation <- paste("copie_symbologie",couche_symbologie_actif(),  "patterns", "pattern_orientation",  sep="$")
eval(parse(text =  paste(gauche_pattern_orientation, paste0("'",input$select_pattern_orientation,"'") , sep = "<-")   ))
options_symbologies_couche_actif(copie_symbologie)




library(grid)

box_essai <- carte_admin_1 %>% st_bbox()

grat<- st_graticule(
  ndiscr =100,
  lon = seq( floor(box_essai$xmin), ceiling(box_essai$xmax), by=2),
  lat = seq(floor(box_essai$ymin), ceiling(box_essai$ymax), by=2)

)



#Fonction pour gerer la conversion des labels decimaux en dégrés, minutes et seconde
decimal_to_dms <- function(x, type) {
  deg <- floor(abs(x))
  min <- floor((abs(x) - deg) * 60)
  sec <- round((abs(x) - deg - min/60) * 3600, 1)

  dir <- ifelse(type %in% c("N", "S"),
                ifelse(x >= 0, "N", "S"),
                ifelse(x >= 0, "E", "W"))

  paste0(deg, "° ", min, "' ", sec, '" ', dir)
}


























































#La carte de base des repères
base_graph <- ggplot() +
  coord_equal(xlim = c(0, 297), ylim = c(0, 210), expand = TRUE)


#Le code pour l'objet graphique 1
graph_1<-


  #Données sur le couche : carte_admin_1
  data_couche1 =carte_admin_1



ggplot()+geom_sf(data=data_couche1 %>% filter(Shape_Area %in% 0:5), linetype="solid", colour="#000000", fill="#F768A1", linewidth=1, show.legend = "line" )  +
  coord_sf(crs=st_crs(4326),
           datum=st_crs(4326),
           label_graticule="ENWS",#la clé qui permet de generer les labels des grilles sur les 4 cotés
           xlim=c(14, 27 ),
           ylim=c(2, 11 )
  )+

  scale_x_continuous(breaks=seq(14, 27,by= 1) ) +
  scale_y_continuous(breaks=seq(2, 11,by= 1) ) +

  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
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
    plot.margin = unit(c(0, 0, 0, 0), "lines" ))


#Assemblage des objets graphqiques créees

graph <- combiner_cartes(base_graph, graph_1 , xmin = 0, xmax = 297, ymin = 0, ymax = 210 )







#fonction pour creer des gratitules avec plus de controle
create_graticule_map <- function(bbox, lon_interval = 1, lat_interval = 1, label_format = "decimal", marge_grille = 0.2) {

  #Appliquer la marge
 # bbox<- bbox+ c(-marge_grille, -marge_grille, marge_grille, marge_grille )


  # Créer la zone d'étude
  zone_etude <- st_sfc(st_polygon(list(rbind(
    c(bbox["xmin"], bbox["ymin"]),
    c(bbox["xmax"], bbox["ymin"]),
    c(bbox["xmax"], bbox["ymax"]),
    c(bbox["xmin"], bbox["ymax"]),
    c(bbox["xmin"], bbox["ymin"])
  ))), crs = 4326 )

  # Générer les graticules

  graticules <- st_graticule(
    ndiscr = 100,
    lon = union(seq(bbox["xmin"], bbox["xmax"], by = lon_interval),  bbox["xmax"] )    ,
    lat = union(seq(bbox["ymin"], bbox["ymax"], by = lat_interval),  bbox["ymax"] )
  )

  # Formater les labels selon le format choisi
  if (label_format == "decimal") {
    graticules$degree_label <- paste0( floor(graticules$degree), "°")
  } else if (label_format == "symbol") {
    graticules$degree_label <- ifelse(graticules$type %in% c("E", "W"),
                                      paste0(graticules$degree, "°", substr(graticules$type, 1, 1)),
                                      paste0(graticules$degree, "°", substr(graticules$type, 1, 1)))
  } else if (label_format == "dms") {
    graticules$degree_label <- mapply(decimal_to_dms, graticules$degree, graticules$type)
  }


  return(graticules)
}



zone_etude_essai <- st_sfc(st_polygon(list(rbind(
  c(box_essai["xmin"], box_essai["ymin"]),
  c(box_essai["xmax"], box_essai["ymin"]),
  c(box_essai["xmax"], box_essai["ymax"]),
  c(box_essai["xmin"], box_essai["ymax"]),
  c(box_essai["xmin"], box_essai["ymin"])
))), crs = 4326)


ma_graticule <- create_graticule_map(box_essai, lon_interval = 1, lat_interval = 1, label_format = "decimal",  marge_grille = 0)

ma_graticule_point <- st_cast(ma_graticule, "POINT", ids = seq_along(x), group_or_split = TRUE)


grat_labels <- grat %>%
  mutate(
  long_label= paste0( abs(degree), "°", ifelse(degree>0, "E", "W")),
  lat_label= paste0( abs(degree), "°", ifelse(degree>0, "N", "S"))
)



zone_buffer_essai <- st_buffer(st_convex_hull(zone_etude_essai), dist = 10)


graph_essai <- ggplot() +
geom_sf(data=carte_admin_1, linetype="solid", colour=alpha("#DF1616", 1),
                                fill=alpha("#4682B4", 1), linewidth=1,
                                show.legend = "line" ) +

  geom_sf(data = zone_etude_essai , color="black", linetype="solid", fill=NA, linewidth=2) +

 # geom_sf(data = zone_buffer_essai , color="cyan", linetype="solid", fill="blue", alpha=0.6) +

  geom_sf(data = filter(ma_graticule, type=="E" ), color="gray80", linetype="solid", fill=NA) +
  geom_sf_text( data = filter(ma_graticule_point, type=="E",      ),  aes(label= degree_label  ), col="red", size=4.5, nudge_y = 0)+ #coord_quickmap()


  geom_sf_text( data = filter(ma_graticule_point, type=="N"),  aes(label= degree_label  ), col="black", size=4.5, nudge_x = 0  )+ #coord_quickmap()
  geom_sf(data = filter(ma_graticule, type=="N"), color="gray80", linetype="solid", fill=NA) +


  coord_sf(
    xlim=c(box_essai$xmin , box_essai$xmax ),
    ylim=c(box_essai$ymin, box_essai$ymax ),
    ndiscr = 0,#annuler les labels automatiques
  ) +

theme(axis.text.x=element_blank(),axis.text.y=element_blank(), axis.text = element_blank(), axis.title = element_blank() )

+
  #coord_sf(label_axes = "ENWS")+
  geom_sf_text( data = filter(grat_labels, type=="E"), aes(label=long_label)) +
geom_sf_text( data = filter(grat_labels, type=="N"), aes(label=lat_label)) +
  coord_sf(ndiscr = 0)














withTags(


  fluidRow( class="cadre_general_app container-fluid",

            fluidRow(
              column(width=3, h3("Essai"))
            ),
            fluidRow(
              column(width=3,class="zones_gauche",

                     fluidRow(
                       mod_gestion_couches_ui("map_ggplot")
                     )

              ),
              column(width=9,class="zones",
                     fluidRow(
                       p("preview graphique")
                     )

              )
            )



  )

)#fin withTags



































showModal(modalDialog(
  title = paste0("Options de la symbologie de la couche ", name_couche),
  footer=tagList(
    div(class="div_footer_modal",
        actionButton(ns("bouton_ok_symbologie_couche"), "Ok", class="btn-success"),
        actionButton(ns("bouton_appliquer_symbologie_couche"), "Appliquer", class="btn-success"),
        modalButton("Annuler")
    )
  ),

  fluidRow(
    withSpinner(
      uiOutput(ns("options_symbologie_layer_ui")) )
  ),#on paramètre le contenu de la fenêtre cible ici
  #trigger = "ajouter_couche",
  #scrollable=TRUE,
  size="l",

  easyClose = TRUE
))



























