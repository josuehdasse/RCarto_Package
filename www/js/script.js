$(document).ready(function(){
  //On contextualise les ID



  //On récupère la liste des couches actualisée (écoute active)
  Shiny.addCustomMessageHandler("liste_couches", function(data){
    console.log(data);
    console.log(data.carte_admin_1)

    //liste_couche[0].carte_admin_1[0].type_symbologie

    //actualiser_liste_couches(gestion_couchesNS, "liste_couches_carte", data);

  });


  //Gestion de la lsite des options d'effets de la symbologie des couches
  Shiny.addCustomMessageHandler("options_effets_symbologie", function(data){
    console.log("effets");
    console.log(data);

    actualiser_liste_effets(gestion_couchesNS, "liste_effets_associes_symbologie", data);

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



