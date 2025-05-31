$(document).ready(function(){
  //On récupère la liste des couches actualisée (écoute active)
  Shiny.addCustomMessageHandler("liste_couches", function(data){
    console.log(data);
    console.log(data.carte_admin_1)

    //liste_couche[0].carte_admin_1[0].type_symbologie

    actualiser_liste_couches("liste_couches_carte", data)
  });



});



