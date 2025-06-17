function li_couche(ns, id_body,symbologie, visible,  couleur_symbole, couleur_trait, epaisseur_trait,style_trait, opacity_fill,opacity_border,  name){
  console.log("#"+ns+id_body);
  var ul_couches= $("#"+ns+id_body);


  //nouveu li pour la couche
  //Nouvelle ligne
  var  ligne = $("<li>",{
            id: name,
            class:"list_item"
      });

      //Container du checkbox
      var div_input = $("<div>", {
            class:"checkbox-container"
      });

        //Le contenu du checkbox-container
            //ajout de l'action
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

            checkbox_ligne.appendTo(div_input);
        div_input.appendTo(ligne);


      //On ajoute le container du symbologie
      var div_symbole = $("<div>", {
              class:"symbole-container"
        })


              //valeur px= valeur/0.75comme on travaille en resolution 95
              var epaisseur_trait_calcul = epaisseur_trait/0.75 +1

              //var style_border=  epaisseur_trait_calcul+ 'px '+style_trait+ ' '+couleur_trait+' ';

              var style_border=  epaisseur_trait_calcul+ 'px '+style_trait;

              //la couleur du trait
              var chaine_style_trait="";
              var chaine_style_background="";

              for (var k = 0; k < couleur_trait.length; k++) {
                chaine_style_trait=chaine_style_trait +"," + couleur_trait[k] + " " + parseFloat(opacity_border[k])*100 + "%"
                chaine_style_background=chaine_style_background +"," + couleur_symbole[k] + " " + parseFloat(opacity_fill[k])*100 + "%"
              }

              var border_color = "color-mix(in srgb" + chaine_style_trait + ")";
              var background = "color-mix(in srgb" + chaine_style_background + ")";


              var div_color_essai = document.createElement("div");
                  div_color_essai.style.display="none";
                  div_color_essai.style.color=chaine_style_trait;

                  var chaine_style_trait2=getComputedStyle(div_color_essai).color;

                  console.log(chaine_style_trait2);
              //var rire= reire.getComputedStyle();

              console.log(border_color);

            //input des symboles
            if(symbologie=="unique"){
                  var symbole_ligne = $('<input>', {
                      type:"color",
                      id:"checkbox_"+name,
                      value:background,
                  }).css("border", style_border +" #D6D6D6");

                  symbole_ligne.css("border-color",border_color);
                  symbole_ligne.css("background", background );


                  //on lui ajoute une fonction d'evenement (réservé)
                  symbole_ligne.change(function(){

                    var resultat_couleur={
                      "name": name,
                      "couleur":this.value
                    }

                    //Envoi des valeurs à shiny
                    Shiny.setInputValue(ns+"couleur_unique", JSON.stringify(resultat_couleur), {priority:'event'});
                  });

                  symbole_ligne.appendTo(div_symbole);
            }//Fin de la gestion des couches à symbologie unique


        div_symbole.appendTo(ligne);



        //Le texte
        var texte_name = $('<span>').text(name);
          texte_name.appendTo(ligne);


          //le bouton toogle de gestion
          var div_gestion = $('<div>',{
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

          dropdown_opt.appendTo(div_gestion)

          div_gestion.appendTo(ligne);


          //Gestion des sous listes ici pour les catégorisés et autres types de symbologie



      //on intègre au menu
      ligne.appendTo(ul_couches);


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
            //On récupère les symbologies uniques ici
           var  symbologies_unique_couche = "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique";
                symbologies_unique_couche=eval(symbologies_unique_couche)


            var couleur_symbole=[];//Array initié
            var couleur_trait=[];
            var epaisseur_trait=[];
            var style_trait=[];

            var opacity_fill=[];
            var opacity_border=[];

            //on partcour les symbologies pour recupérer les caractéristqiues essentielles
            var names_symbologies = Object.keys(symbologies_unique_couche);

            for (var j = 0; j < names_symbologies.length; j++) {
                 var couleur_symbole_i= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique."+names_symbologies[j]+".couleur_symbole";
                     couleur_symbole_i=eval(couleur_symbole_i);
                     //incrémentation dans le tableau
                     couleur_symbole.push(couleur_symbole_i);

                  var couleur_trait_i= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique."+names_symbologies[j]+".couleur_trait";
                      couleur_trait_i=eval(couleur_trait_i);
                      couleur_trait.push(couleur_trait_i);

                  var epaisseur_trait_i= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique."+names_symbologies[j]+".epaisseur_trait";
                       epaisseur_trait_i=eval(epaisseur_trait_i);
                       epaisseur_trait.push(epaisseur_trait_i);

                  var style_trait_i= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique."+names_symbologies[j]+".style_trait";
                      style_trait_i=eval(style_trait_i);
                      style_trait.push(style_trait_i);

                  var opacity_fill_i= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique."+names_symbologies[j]+".opacity_fill";
                      opacity_fill_i=eval(opacity_fill_i);
                      opacity_fill.push(opacity_fill_i);

                  var opacity_border_i= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique."+names_symbologies[j]+".opacity_border";
                      opacity_border_i=eval(opacity_border_i);
                      opacity_border.push(opacity_border_i);

            }



           // var legende= "liste_couche."+   names_couches[i] +".options_symbologie_couche.options_symbologie_unique.legende";
                //legende=eval(legende);


            console.log(couleur_symbole);

             //id_body, couleur_symbole, couleur_trait, epaisseur_trait,style_trait,   name, legende
            li_couche(ns, id_body,symbologie, visible, couleur_symbole,couleur_trait,epaisseur_trait, style_trait, opacity_fill,opacity_border,   names_couches[i] );

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
