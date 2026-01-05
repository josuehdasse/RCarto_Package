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



#Presence opérationnelle
library(readxl)
ocha_car_3w_presence_operationnelle_t1_2024 <- read_excel("~/Mes données/Humanitaire/presence operationnelle/ocha-car-3w-presence-operationnelle-t1-2024.xlsx")
ocha_car_3w_presence_operationnelle_t1_2024 <- ocha_car_3w_presence_operationnelle_t1_2024 %>% slice(2:n())

liste_sous_prefectures <- unique(ocha_car_3w_presence_operationnelle_t1_2024$SOUSPREFECTURE)

#secteurs
liste_cluster= unique(ocha_car_3w_presence_operationnelle_t1_2024$SECTEUR)

data_pref <- as.data.frame(liste_sous_prefectures)


data_interventions=data.frame()

chaine_intervention <- c()

for (i in liste_sous_prefectures) {

    chaine_clusters=""

    for (j in liste_cluster) {
        liste_organisations=  ocha_car_3w_presence_operationnelle_t1_2024 %>% filter(SOUSPREFECTURE==i, SECTEUR==j ) %>% select(ACTEUR) %>% pull()

        if(length(liste_organisations)  !=0 ){

          chaine_intervention = paste0( j, " : ", paste(unique(liste_organisations), collapse = ", ") )
          chaine_intervention =str_wrap(chaine_intervention , width = 30)

          if(chaine_clusters==""){
              chaine_clusters=chaine_intervention
          }else{
              chaine_clusters=paste(chaine_clusters, chaine_intervention, sep = "\n")
          }

        }

    }

    data=data.frame(spref=i, interventions=paste0( chaine_clusters ))
    data_interventions=rbind(data_interventions, data)

}
