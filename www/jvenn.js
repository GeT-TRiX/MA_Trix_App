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


			var colorDefault = ["#006600", "#5a9bd4", "#f15a60", "#cfcf1b", "#ff7500", "#c09853"],
				displayMode  = "classic",
				displayStat  = true,
				displaySwitch= true,
				shortNumber  = true,
				fontSize     = "12px",
				fontFamily   = "Arial",
        uploadSeries = new Array();
    
    
    function updateJvenn() {
		

		Shiny.addCustomMessageHandler("updatejvenn", function(final) {	
    //Shiny.addCustomMessageHandler("updatetype", function(thisiswhy) {	


		  let seriesTable = final;//jsonData;
      
		      
			    $("#jvenn-container").jvenn({
					series: seriesTable,
					//colors: colorsTable,
					fontSize:   fontSize,
					fontFamily: fontFamily,
					searchInput:  $("#search-field"),
					searchStatus: $("#search-status"),
					displayMode: displayMode,
					//displayMode: thisiswhy,//"edwards",
					displayStat: displayStat,
					
					fnClickCallback: function() {
						let value = "";
						nameslis = [];
						if (this.listnames.length == 1) {
							value += "Elements only in ";
							
						} else {
							value += "Common elements in ";
						}
						
						for (name in this.listnames) {
							//value += this.listnames[name] + " ";
							nameslis.push(this.listnames[name]);
						}
						     value += ":\n";
						     mylist =[];
						for (val in this.list) {
						     //value += this.list[val] + "\n";
						     mylist.push( this.list[val]);
						  }
						     $("#names").val(value);
						     
						     
						     Shiny.onInputChange("testons",mylist);// renvoyer dans R
						     Shiny.onInputChange("together",nameslis.join(""));// renvoyer dans R
						     Shiny.onInputChange("selcontjv",nameslis);
						     return(mylist); 
					}

				});
		});
	}

			
		

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
				updateJvenn();				
			});
			$("#dsw_no").click(function() {
				displaySwitch = false;
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
				//Shiny.onInputChange("updamod",fontFamily);
				updateJvenn();				
			});
			
			$('[id^="fs"]').click(function() {
				fontSize = $(this).html();
				Shiny.onInputChange("myfont",fontSize);
				updateJvenn();				
			});
			
			updateJvenn();

});
