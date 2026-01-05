
box_emprise = st_sfc(list( st_polygon(
    rbind(
      c(liste_objets[[i]]$emprise_carte$xmin,  liste_objets[[i]]$emprise_carte$ymin),
      c(liste_objets[[i]]$emprise_carte$xmin,  liste_objets[[i]]$emprise_carte$ymax),
      c(liste_objets[[i]]$emprise_carte$xmax,  liste_objets[[i]]$emprise_carte$ymax),
      c(liste_objets[[i]]$emprise_carte$xmax,  liste_objets[[i]]$emprise_carte$ymin),
      c(liste_objets[[i]]$emprise_carte$xmin,  liste_objets[[i]]$emprise_carte$ymin)
    )

  )
  )
)















#La carte de base des repères
base_graph <- ggplot() +
  coord_equal(xlim = c(0, 297), ylim = c(0, 210), expand = TRUE)


#Le code pour l'objet graphique 1



#Données sur le couche : carte_admin_11
data_couche1 =carte_admin_1


graph_1<- ggplot()+
  geom_sf(data=st_simplify(data_couche1), linetype="solid", colour="#000000", fill="#003C30", linewidth=1, show.legend = "line" )  +
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
















































