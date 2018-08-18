$(document).ready(function () {

    let jsonData =[{
            	name: 'list1',
            	data: ["Marilyn Monroe", "Arnold Schwarzenegger", "Jack Nicholson", "Barbra Streisand", "Robert de Niro", "Dean Martin", "Harrison Ford","Mom"]
            }, {
            	name: 'list2',
            	data: ["Freddy Mercury", "Barbra Streisand", "Dean Martin", "Ricky Martin", "Celine Dion", "Marilyn Monroe"]
    }];


    function updateJvenn() {
			
		Shiny.addCustomMessageHandler("updatejvenn", function(final) {	
	  
		  //letseriesTable = new Array();
		  let seriesTable = final;//jsonData;
		  //let seriesTable = jsonData;
		  //letseriesTable = final;
		  let obj = eval(jsonData);
		  let arr1 =new Array();
			for(let i=0;i<obj.length;i++){
			    arr1[i]=obj[i].data;
			 }
					console.log(jsonData);
					console.log(seriesTable);
		       
			    $("#jvenn-container").jvenn({
					series: seriesTable,
					searchInput:  $("#search-field"),
					searchStatus: $("#search-status"),
					displayMode:  $("#venn-type").val(),
					displayStat: true,
					
					fnClickCallback: function() {
						let value = "";
						nameslis = [];
						if (this.listnames.length == 1) {
							value += "Elements only in ";
							
						} else {
							value += "Common elements in ";
						}
						
						for (name in this.listnames) {
							value += this.listnames[name] + " ";
							nameslis.push(this.listnames[name]);
						}
						     value += ":\n";
						     mylist =[];
						for (val in this.list) {
						     value += this.list[val] + "\n";
						     mylist.push( this.list[val]);
						}
						     $("#names").val(value);
						     
						     
						     Shiny.onInputChange("testons",mylist);
						     Shiny.onInputChange("together",nameslis.join(""));
						     return(mylist); // renvoyer dans R
					}
				});
		});
	}
    //});
			
		

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
			// first init of the jvenn plugin
			
			updateJvenn();

});
