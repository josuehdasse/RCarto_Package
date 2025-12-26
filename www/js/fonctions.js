



function actualiser_liste_effets(ns, id_div, liste_effets){

      console.log("#"+ns+ id_div);

      var ul = $("#"+ns+ id_div);
      ul.empty();

      //La laiste des effets
       var names_effets = Object.keys(liste_effets);

       for (var i = 0; i < names_effets.length; i++) {

          console.log(i);

         //on construit la liste
         var liste=$('<li>',{
              class:"ligne_effets"

         })



              var   active= eval("liste_effets."+names_effets[i]+".checked");
              var   label= eval("liste_effets."+names_effets[i]+".label");
              var   options= eval("liste_effets."+names_effets[i]+".options");//les options de la symbologie en cours

           //input checkbox
           var div_label= $("<label>")

                    if(active){
                      var checkbox_ligne = $('<input>', {
                          type:"checkbox",
                          id:"checkbox_"+name,
                          checked:"checked",
                          width:20,
                          height:20
                      });

                    }else{
                      var checkbox_ligne = $('<input>', {
                          type:"checkbox",
                          id:"checkbox_"+name,
                          width:20,
                          height:20
                      });
                    }

                    checkbox_ligne.change(function(){
                        alert("check "+label);
                    });

                    var span_ecart= $("<span>", {
                      class:"slider"
                    });

                    checkbox_ligne.appendTo(div_label)

                    span_ecart.appendTo(div_label)

              div_label.appendTo(liste)




              var texte=$("<span>").text(label).click(function(){
                alert(label)
              });

                  texte.appendTo(liste);


            console.log(liste);

        liste.appendTo(ul);

       }




}







//Fonctions de gestion des couleurs pour le controle des liste des couches
function hexToRgba(hex, opacity){
  //Supprimer le # si présent
  var hex = hex.replace(/^#/, '');

  //Gestion des formats couleurs
  if(hex.length===3 || hex.length===4){
    hex= hex.split('').map(char=> char +char).join('');
  }


  //extraitre els composantes R, G, B, A
  const r = parseInt(hex.substring(0,2), 16);
  const g = parseInt(hex.substring(2,4), 16);
  const b = parseInt(hex.substring(4,6), 16);
  let a=1

  //Gerer l'alpha present dans la valeur hex (els 8 caractères)
  if(hex.length==8){
    const alphahex=hex.substring(6,8);
    a=parseInt(alphahex, 16)/255;
  }

  //Priorité aux paramètres ocpacity s'il est fonction_color_symboble_unique
  if(opacity!==undefined){
    a=Math.min(1, Math.max(0, opacity));//clamper entre 0 et 1
  }

  return `rgba(${r}, ${g}, ${b}, ${a})`;

}




//Fonction de combinaison des couleurs rgba

/*
* convertiy une chaine rgba en objet normalisé
* @param {string} rgbaStr --- Chaine RGBA
*/


function parseRgba(rgbaStr){
  const match=rgba.match(/rgba?\((\d+), \s*(\d+), \s*(\d+)(?:, \s*([\d.]+))?\)/i);

  if(!match) throw Error('Format RGBA invalide');

  return{
    r:parseInt(match[1]/255),
    g:parseInt(match[2]/255),
    b:parseInt(match[3]/255),
    a:match[4] !== undefined ? parseFloat(match[4]):1
  };
}




//Fonction d'application des modes fusion selon les couches de couleurs
function applyBlend(){

}


//Fonction qui permet d'afficher les options de paramétrage d'une couhe de sumbologie à partir de son ID
function afficher_options_couche_symbologie(couche){

  //alert(couche);

    var resultat_name_symbologie={
          "name": couche
       }

      //Envoi des valeurs à shiny
      Shiny.setInputValue(gestion_couchesNS+"choix_couche_symbologie", JSON.stringify(resultat_name_symbologie), {priority:'event'});
}





//Fonction qui afffiche la liste des options des effets choisis par l'utiilsateur
function afficher_options_effet(valeur){
  let decoupages = valeur.split("_");

  console.log(decoupages[1]);

   var resultat_name_effet={
          "name_effect": decoupages[1]
       }

      //Envoi des valeurs à shiny
      Shiny.setInputValue(gestion_couchesNS+"select_effet_click", JSON.stringify(resultat_name_effet), {priority:'event'});


}



function fonction_color_symboble_unique(valeur){
      //alert(valeur)
      //e.stopImmediatePropagation();

      $("#select_couleur_symbole").val(valeur);

       var infos_color_symboble_unique={
          "color_symboble_unique_js":this.val()
      }
          //Envoi des valeurs à shiny
          Shiny.setInputValue("infos_color_symboble_unique", JSON.stringify(infos_color_symboble_unique), {priority:'event'});
}


function fonction_color_trait_unique(valeur){
  //alert(valeur)
      //e.stopImmediatePropagation();
      $("#select_couleur_trait").val(valeur)
}







//Fonction pour la gestion de l'affichage des détails des objets ajoutés à une carte
function details_objets_carte(id){
    name_objet= id.substring(5, id.length );
    console.log(name_objet);

     var resultat_name_select={
      "name":name_objet
    }

    Shiny.setInputValue(gestion_objets_carteNS+"name_objet_carte_select",  JSON.stringify(resultat_name_select), {priority:'event'}     );
}




//Gestion de la visibilité des couches vecteurs
function gestion_visibilite_couche_vecteur(id, activation){

      var resultat_activation={
          "name": id,
          "activation":activation
      };

    //Envoi des valeurs à shiny
     Shiny.setInputValue(gestion_couchesNS+"select_activation_couche", JSON.stringify(resultat_activation), {priority:'event'});

}




//Gerer les paramètres de la symbologie pour les catégories de symbologie###################
function gestionnaire_parametres_symbologies_categorie(id){

     var resultat_activation={
            "name": id,
            "level":"categories"
          };

    //Envoi des valeurs à shiny
    Shiny.setInputValue(gestion_couchesNS+"parametres_symbologie_categorie", JSON.stringify(resultat_activation), {priority:'event'});

}



//Gerer la visibilité des couches de catégories des pour la symbologie de txpe catégorisée
function gestion_visibilite_categories(id, activation){

     var resultat_activation={
            "name": id,
            "activation":activation
          };
    //Envoi des valeurs à shiny
    Shiny.setInputValue(gestion_couchesNS+"select_activation_couche_categorie", JSON.stringify(resultat_activation), {priority:'event'});

}



//Gestion du paramétrage de la categorie des symboles pour le type de symbologie categorisee
function parametrer_symbole_categorise(){

  var level_parametrage = {
      "level":"symbole"
  }

   //Envoi des valeurs à shiny
  Shiny.setInputValue(gestion_couchesNS+"gestion_parametrage_symbole_categorie", JSON.stringify(level_parametrage), {priority:'event'});

}






//Visibilité des couches de catégories depuis la liste des couches
function gestion_visibilite_categories_dListeCouche(couche, categorie, activation){
    var resultat_activation={
          "couche": couche,
          "categorie":categorie,
          "activation":activation
      };

      //Envoi des valeurs à shiny
     Shiny.setInputValue(gestion_couchesNS+"select_activation_Categorie_dListeCouche", JSON.stringify(resultat_activation), {priority:'event'});

}




//gestion de symbologie d'une couche vecteur
function symbologie_couche_vecteur(id){

      var resultat_symbologie={
          "name": id
      }

      //Envoi des valeurs à shiny
      Shiny.setInputValue(gestion_couchesNS+"select_option_symbologie_couche", JSON.stringify(resultat_symbologie), {priority:'event'});
}






//Lancement de la jointure sur les couches vecteurs
function jointures_couche_vecteur(id){
    var resultat={
          "name": id
      }

      //Envoi des valeurs à shiny
      Shiny.setInputValue(gestion_couchesNS+"lancement_jointure_couche", JSON.stringify(resultat), {priority:'event'});
}


//Gestion des jointures_couche_vecteur sur lequel on a cliqué
function gestion_click_jointure(id){
   var resultat={
          "name": id
      }

      //Envoi des valeurs à shiny
      Shiny.setInputValue(gestion_couchesNS+"select_jointure_couche", JSON.stringify(resultat), {priority:'event'});

}




//La egstion du clic sur les intervalles de grades de symbologie
function gestionnaire_intervalles_symbologie_graduee(id){
  var resultat={
          "name": id,
          "level":"categories"
      }

      //Envoi des valeurs à shiny
      Shiny.setInputValue(gestion_couchesNS+"clic_intervalle_graduation_symbologie", JSON.stringify(resultat), {priority:'event'});
}

