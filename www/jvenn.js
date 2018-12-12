/***************************************************************
*  Copyright notice
*
*  (c) 2014 PF bioinformatique de Toulouse
*  All rights reserved
*
*
*  This script is an adaptation of the venny script developed by
*  Juan Carlos Oliveros, BioinfoGP, CNB-CSIC:
*  Oliveros, J.C. (2007) VENNY. An interactive tool for comparing
*  lists with Venn Diagrams.
*  http://bioinfogp.cnb.csic.es/tools/venny/index.html.
*  It is distributed under the terms of the GNU General Public
*  License as published by the Free Software Foundation; either
*  version 2 of the License, or (at your option) any later version.
*
*  The GNU General Public License can be found at
*  http://www.gnu.org/copyleft/gpl.html.
*
*  This script is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  This copyright notice MUST APPEAR in all copies of the script!
***************************************************************/

/***************************************************************
*
* Adapted to Shiny by Franck Soubès.
*
****************************************************************/

$(document).ready(function () {


      //$( "#jvenn-container-label1" ).draggable();
      //$('#jvenn-container-label1').addClass('draggable');
			var colorDefault = ["#FFA500", "#FFA500", "#FFA500", "#FFA500", "#FFA500", "#FFA500"],
				displayMode  = "classic",
				displayStat  = true,
				displaySwitch= true,
				shortNumber  = true,
				fontSize     = "12px",
				fontFamily   = "Arial",
        uploadSeries = new Array();



  function updateJvenn() {
    Shiny.addCustomMessageHandler("updatejvenn", function(final) {
	    let seriesTable = final;//jsonData;
      Shiny.addCustomMessageHandler("updatejcol", function(coljvenn) {

        let arraylen = (coljvenn.length/3)
        let R2jspal =new Array(arraylen);
        let mypalette = new Array(Math.ceil(coljvenn.length / 3)).fill("").map(function() { return this.splice(0, 3) }, coljvenn.slice());

        for (let i = 0 ; i< arraylen ; i++ ){
          R2jspal[i] = "rgb(".concat(mypalette[i]).concat(")");
        }

			  $("#jvenn-container").jvenn({
			  series: seriesTable,
			  colors : R2jspal,
			  fontSize:   fontSize,
			  fontFamily: fontFamily,
			  searchInput:  $("#search-field"),
			  searchStatus: $("#search-status"),
			  displayMode: displayMode,
			  displayStat: displayStat,
				displaySwitch:displaySwitch,

			  fnClickCallback: function() {
			    let value = "";
				  nameslis = [];
				  if (this.listnames.length == 1) {
				    value += "Elements only in ";
				  }
				  else {
					  value += "Common elements in ";
			    }
				  for (name in this.listnames) {
				    nameslis.push(this.listnames[name]);
				  }
				  value += ":\n";
				  jvennlist =[];
				  for (val in this.list) {
				    jvennlist.push( this.list[val]);
				  }
					console.log(jvennlist);
				  $("#names").val(value);
          Shiny.onInputChange("jvennlist",jvennlist);// renvoyer dans R
				  Shiny.onInputChange("together",nameslis.join(""));// renvoyer dans R
				  Shiny.onInputChange("selcontjv",nameslis);
				  return(jvennlist);
	        }
			  });
		  });

	  });
  }


      //$('.draggable').addClass('draggable');
      //$('.draggable').draggable();


      //$('#jvenn-container').addClass('parent');
      //$('.children').draggable({ containment: "parent" });

      //$('.children1').addClass('draggable');
      //$('.children1').draggable();

      //$("#jvenn-container" ).draggable();
      //$('#jvenn-container').addClass('draggable');

      //$("div#jvenn-container .test").addClass('draggable');
      //$("div#jvenn-container .test").draggable();

			$('.btn-group button').on('click', function(){
    	$(this).siblings().removeClass('active')
    	$(this).addClass('active');
			})



			$('[id^="clear"]').click(function() {
				let index = $(this).attr("id").split("_")[1];
				$("#area" + index).val("");
				$("#name" + index).val("List " + index);
				updateJvenn();
			});

			// update the view when any fields change
			$("[id^=name]").change(function() {
				updateJvenn();
			});
			$("[id^=area]").change(function() {
				updateJvenn();
			});

			$("#venn-type").change(function() {
				updateJvenn();
			});
				$("#ds_yes").click(function() {
				displayStat = true;
				Shiny.onInputChange("mystat",displayStat);
				updateJvenn();
			});
			$("#ds_no").click(function() {
				displayStat = false;
				Shiny.onInputChange("mystat",displayStat);
				updateJvenn();
			});

			$("#dsw_yes").click(function() {
				displaySwitch = true;
				Shiny.onInputChange("dispswitch",displaySwitch);
				updateJvenn();
			});
			$("#dsw_no").click(function() {
				displaySwitch = false;
				Shiny.onInputChange("dispswitch",displaySwitch);
				updateJvenn();
			});

			$("#dm_classic").click(function() {
				displayMode = "classic";
        Shiny.onInputChange("updamod",displayMode);
				updateJvenn();
			});


			$("#dm_edwards").click(function() {
				displayMode = "edwards";
				Shiny.onInputChange("updamod",displayMode);
				updateJvenn();
			});

			$('[id^="ff"]').click(function() {
				fontFamily = $(this).html();
				updateJvenn();
			});

			$('[id^="fs"]').click(function() {
				fontSize = $(this).html();
				Shiny.onInputChange("myfont",fontSize);
				updateJvenn();
			});


			updateJvenn();

});
