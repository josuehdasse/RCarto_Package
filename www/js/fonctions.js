function ligne_tableau_couche(ns, id_body,symbologie, visible,  couleur_symbole, couleur_trait, epaisseur_trait,style_trait,   name, legende  ){
  console.log("#"+ns+id_body);
  var tab = $("#"+ns+id_body);

      //var ns=Shinyapp.$inputVAlues['map_ggplot'];



   console.log(tab)

      //Nouvelle ligne
     var  ligne = $("<tr>",{
                id: name,
                class:"ligne_tableau"
      })

        //Colonne de la case à cocher (activation ou désactivation de la couche)
       var  colonne_1 = $('<td>');

            //input checkbox
            if(visible){
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

            //ajout de l'action
            checkbox_ligne.change(function(){

             if(this.checked){
               var resultat_activation={
                      "name": name,
                      "activation":"TRUE"
                    };
             }else{
               var resultat_activation={
                      "name": name,
                      "activation":"FALSE"
                    };
             }

                //Envoi des valeurs à shiny
                Shiny.setInputValue(ns+"select_activation_couche", JSON.stringify(resultat_activation), {priority:'event'});

            });

            checkbox_ligne.appendTo(colonne_1);


        colonne_1.appendTo(ligne);


        //Colonne des symboles qui représentent la symbologie de la couhe
         var  colonne_2 = $('<td>');
              //valeur px= valeur/0.75comme on travaille en resolution 95
              var epaisseur_trait_calcul = epaisseur_trait/0.75 +1

              var style_symbole=  epaisseur_trait_calcul+ 'px '+style_trait+ ' '+couleur_trait+' ';

              console.log(style_symbole);

            //input des symboles
            if(symbologie=="unique"){
                  var symbole_ligne = $('<input>', {
                      type:"color",
                      id:"checkbox_"+name,
                      value:couleur_symbole,
                  }).css("border", style_symbole)


                  //on lui ajoute une fonction d'evenement (réservé)

                  symbole_ligne.change(function(){

                    var resultat_couleur={
                      "name": name,
                      "couleur":this.value
                    }

                    //Envoi des valeurs à shiny
                    Shiny.setInputValue(ns+"couleur_unique", JSON.stringify(resultat_couleur), {priority:'event'});

                  });

                  symbole_ligne.appendTo(colonne_2);

            }//Fin de la gestion des couches à symbologie unique





        colonne_2.appendTo(ligne);


        //Colonne du Name  (nom de la couche)
        var colonne_3 = $('<td>',{
          id:"name_couche"+name
          }).text(name);

          colonne_3.appendTo(ligne);

        //Colonne de la légende
        var colonne_4 = $('<td>',{
          id:"legende_couche"+name
          }).text(legende);

          colonne_4.appendTo(ligne);


        //Colonne des options
        var colonne_5 = $('<td>',{
          id:"options_couche"+name
          });

          var dropdown_opt=$("<div>",{class:"btn-group" });
              var btn_1 =$("<button>", {
                type:"button",
                class:"btn btn-default"
              }).text("Options").appendTo(dropdown_opt);

              var btn_2=$("<button>", {
                type:"button",
                class:"btn btn-default dropdown-toggle"
              }).attr("data-toggle", "dropdown");

                var span_1 =$("<span>",{
                  class:"caret"
                }).appendTo(btn_2)

                var span_2 =$("<span>",{
                  class:"sr-only"
                }).appendTo(btn_2);

              btn_2.appendTo(dropdown_opt);

          var ul=$("<ul>",{
              class:"dropdown-menu",
              role:"menu"
          });

              var li_1=$("<li>");
                  var li_a_1=$("<a>",{
                    href:"#",
                  }).text("symbologie");

                  li_a_1.click(function(){
                    //Gestion de la symbologie pour cette couche

                     var resultat_symbologie={
                      "name": name
                    }

                    //Envoi des valeurs à shiny
                    Shiny.setInputValue(ns+"select_option_symbologie_couche", JSON.stringify(resultat_symbologie), {priority:'event'});

                  });

                  li_a_1.appendTo(li_1);
                  li_1.appendTo(ul);

              var li_2=$("<li>");
                  var li_a_2=$("<a>",{
                    href:"#",
                  }).text("Etiquettes");

                  li_a_2.click(function(){
                    //Gestion des étiquettes pour cette couche

                  });

                  li_a_2.appendTo(li_1);
                  li_2.appendTo(ul);


               var li_3=$("<li>");
                  var li_a_3=$("<a>",{
                    href:"#",
                  }).text("Supprimer "+name);

                  li_a_3.click(function(){
                    //Gestion de la suppression de la couche

                  });

                  li_a_3.appendTo(li_1);
                  li_3.appendTo(ul);

              ul.appendTo(dropdown_opt);

          dropdown_opt.appendTo(colonne_5)

          colonne_5.appendTo(ligne);

      //Intégrer la lign au tableau
      ligne.appendTo(tab)


}


//Fonction pour actualiser les lignes du tableau de la liste des couches
function actualiser_liste_couches(ns, id_body, liste_couche){
      console.log("#"+ns+id_body);

      //on vide le conttenu de la table
      var tab = $("#"+ns+id_body);
      tab.empty();

  var names_couches = Object.keys(liste_couche);

  for (var i = 0; i < names_couches.length; i++) {


    var symbologie= "liste_couche."+   names_couches[i] +".type_symbologie";

        symbologie=eval(symbologie);

        console.log("Type de la couche : "+ symbologie);



    //On gère la visibilité de la couhe
    var visible= "liste_couche."+   names_couches[i] +".visible";
        visible=eval(visible);

      if(symbologie=="unique"){
            var legende= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique.legende";
                legende=eval(legende);

            var couleur_symbole= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique.couleur_symbole";
                couleur_symbole=eval(couleur_symbole);

            var couleur_trait= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique.couleur_trait";
                couleur_trait=eval(couleur_trait);

            var epaisseur_trait= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique.epaisseur_trait";
                epaisseur_trait=eval(epaisseur_trait);

            var style_trait= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique.style_trait";
                style_trait=eval(style_trait);


             //id_body, couleur_symbole, couleur_trait, epaisseur_trait,style_trait,   name, legende

            ligne_tableau_couche(ns, id_body,symbologie, visible, couleur_symbole,couleur_trait,epaisseur_trait, style_trait,   names_couches[i], legende  );

      }


  }

}




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





//Fonction qui afffiche la liste des options des effets choisis par l'utiilsateur
function afficher_options_effet(valeur){
  let decoupages = valeur.split("_");

  console.log( decoupages[1]);

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
