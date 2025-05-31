function ligne_tableau_couche(id_body,visible,  couleur_symbole, couleur_trait, epaisseur_trait,style_trait,   name, legende  ){
  var tab = $("#"+id_body);

      tab.empty();

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
                  height:20,
              }).appendTo(colonne_1);
            }else{
              var checkbox_ligne = $('<input>', {
                  type:"checkbox",
                  id:"checkbox_"+name,
                  width:20,
                  height:20,
              }).appendTo(colonne_1);
            }


        colonne_1.appendTo(ligne);


        //Colonne des symboles qui représentent la symbologie de la couhe
         var  colonne_2 = $('<td>');
              //valeur px= valeur/0.75comme on travaille en resolution 95
              var epaisseur_trait_calcul = epaisseur_trait/0.75

              var style_symbole=  epaisseur_trait_calcul+ 'px '+style_trait+ ' '+couleur_trait+' ';

              console.log(style_symbole);

            //input checkbox
            var symbole_ligne = $('<input>', {
                  type:"color",
                  id:"checkbox_"+name,
                  value:couleur_symbole,
            }).css("border", style_symbole)

            //on lui ajoute une fonction d'evenement
            symbole_ligne.change(function(){

              var resultat_couleur={
                "name": name,
                "couleur":this.value
              }

              //Envoi des valeurs à shiny
              Shiny.setInputValue("couleur_unique", JSON.stringify(resultat_couleur), {priority:'event'});

            });
            symbole_ligne.appendTo(colonne_2);



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
          }).text("options");

          colonne_5.appendTo(ligne);

      //Intégrer la lign au tableau
      ligne.appendTo(tab)


}



//Fonction pour actualiser les lignes du tableau de la liste des couches
function actualiser_liste_couches(id_body, liste_couche){

  var names_couches = Object.keys(liste_couche);

  for (var i = 0; i < names_couches.length; i++) {


    var symbologie= "liste_couche."+   names_couches[i] +".type_symbologie";
        symbologie=eval(symbologie);

             console.log(symbologie);

    //On gère la visibilité de la couhe
    var visible= "liste_couche."+   names_couches[i] +".visible";
        visible=eval(visible);



      if(symbologie=="Symbole unique"){
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

            ligne_tableau_couche(id_body, visible, couleur_symbole,couleur_trait,epaisseur_trait, style_trait,   names_couches[i], legende  );

      }


  }

}


