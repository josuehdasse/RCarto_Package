function ligne_tableau_couche(id_body, name, legende  ){
  var tab = $("#"+id_body);

   console.log(tab)

      //Nouvelle ligne
     var  ligne = $("<tr>",{
                id: name,
                class:"ligne_tableau"
      })

        //Colonne de la case à cocher (symbole)
       var  colonne_1 = $('<td>');

            //input checkbox
            var checkbox_ligne = $('<input>', {
                  type:"checkbox",
                  id:"checkbox_"+name,
                  checked:"checked"
            }).appendTo(colonne_1);

        colonne_1.appendTo(ligne);

        //Colonne du Name  (nom de la couche)
        var colonne_2 = $('<td>',{
          id:"name_couche"+name
          }).text(name);

          colonne_2.appendTo(ligne);

        //Colonne de la légende
        var colonne_3 = $('<td>',{
          id:"legende_couche"+name
          }).text(legende);

          colonne_3.appendTo(ligne);


        //Colonne des options
        var colonne_4 = $('<td>',{
          id:"options_couche"+name
          }).text("options");

          colonne_3.appendTo(ligne);

      //Intégrer la lign au tableau
      ligne.appendTo(tab)



}



//Fonction pour actualiser les lignes du tableau de la liste des couches
function actualiser_liste_couches(id_body, liste_couche){

  var names_couches = Object.keys(liste_couche);

  for (var i = 0; i < names_couches.length; i++) {


    var symbologie= "liste_couche."+   names_couches[i] +".type_symbologie";
        symbologie=eval(symbologie);

        console.log(symbologie)

      if(symbologie=="Symbole unique"){
        var legende= "liste_couche."+   names_couches[i] +".options_symbologie_couhe.options_symbologie_unique";
            legende=eval(legende);

            ligne_tableau_couche(id_body, names_couches[i], legende  );

      }


  }

}

