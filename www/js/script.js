$(document).ready(function(){
  //On récupère la liste des couches actualisée (écoute active)
  Shiny.addCustomMessageHandler("liste_couches", function(data){
    console.log(data);
    console.log(data.carte_admin_1)

    //liste_couche[0].carte_admin_1[0].type_symbologie

    actualiser_liste_couches("liste_couches_carte", data);

  });


   //Envoi des informations au clic de validation de sumbologie unique
    $("#select_couleur_symbole").on('change input', function(){
      alert("click");
      //e.stopImmediatePropagation();



      var liste_infos_couleurs_symbologie_unique={
          "couleur_fill_symbole_unique":this.val(),
          "couleur_trait_symbole_unique":$("#select_couleur_trait").val()
      }
          //Envoi des valeurs à shiny
          Shiny.setInputValue("infos_couleurs_symbologie_unique", JSON.stringify(liste_infos_couleurs_symbologie_unique), {priority:'event'});

    });


});



