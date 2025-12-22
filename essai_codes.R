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









































