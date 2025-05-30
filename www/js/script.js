//Script de gestion

//gestion de la ligne du tableau
function ligne_tableau_couche(id_tab, symbole, name, legende  ){
  tab = $("#"+id_tab);

      //Nouvelle ligne
      ligne = $("<tr>",{
                id=name,
                class="ligne_tableau"
      })

        //Colonne de la case à cocher (symbole)
        colonne_1 <- $('<td>');

            //input checkbox
            checkbox_ligne <- $('<input>', {
                  type:"checkbox",
                  id:"checkbox_"+name
                  checked:TRUE
            }).appendTo(colonne_1);

        colonne_1.appendTo(ligne);

        //Colonne du Name  (nom de la couche)
        colonne_2 <- $('<td>',{
          id:"name_couche"+name
          }).text(name);

          colonne_2.appendTo(ligne);

        //Colonne de la légende
        colonne_3 <- $('<td>',{
          id:"legende_couche"+name
          }).text(legende);

          colonne_3.appendTo(ligne);


        //Colonne des options
        colonne_4 <- $('<td>',{
          id:"options_couche"+name
          }).text("options");

          colonne_3.appendTo(ligne);

      //Intégrer la lign au tableau
      ligne.appendTo(tab)

}


//Gestion de la réction des valeurs via web_socket
//const ws = new WebSocket("ws://localhost:8080");

//ws.onmessage = (event) => {
  //const data = JSON.parse(event.data)

  //construire la ligne du tabelau de la liste des couches


}
