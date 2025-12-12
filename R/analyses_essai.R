#importation des fichiers des couches
library(sf)

#importation des cartes ----
carte_admin_1<-st_read("../../Mes données/Humanitaire/Données géographiques/Frontieres Administratives/Adm1/RCA_adm1.shp")
carte_admin_2_0<-st_read("../../Mes données/Humanitaire/Données géographiques/Frontieres Administratives/Adm2/RCA_adm2.shp")
carte_admin_2<-st_read("../../Mes données/Humanitaire/Données géographiques/Frontieres Administratives/Adm2/RCA_adm2.shp")
carte_admin_3<-st_read("../../Mes données/Humanitaire/Données géographiques/Frontieres Administratives/Adm3/RCA_adm3.shp")

#routes
carte_routes<-st_read("../../Mes données/Humanitaire/Données géographiques/old/Routes/RCA_Routes.shp") #%>% filter(fclass=="primary" |  fclass=="secondary")

#routes
carte_villages<-st_read("../../Mes données/Humanitaire/Données géographiques/Villages et Villes/RCA_villes.shp")%>%st_as_sf(crs=4326 )

#Zones habités
carte_zones_habites<-st_read("../../Mes données/Humanitaire/Données géographiques/Old/Zones Habitees/RCA_ZonesHabitees.shp")



