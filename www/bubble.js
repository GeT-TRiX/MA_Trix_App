/*
Author: Franck Soubès
*/

$(document).ready(function () {

var xTitle = "top genes enriched";

var defaultOptions = {
  
  
    series: null,
    chart: {
	  zoomType: "xy",
	  width: 1100,
	  height: 600,
	  renderTo: "highChart",
	  type: 'bubble'

  },

  dom: "highChart",  
  id: "highChart",  
                           
  exporting: {enabled: true},
  credits: {href: null, text: null},
  
                     
   legend: {
        enabled: true
    },

    title: {
        text: null
    },

    subtitle: {
        text: 'Source: <a href="http://www.euromonitor.com/">fuck</a> and <a href="https://data.oecd.org/">OECD</a>'
    },

    xAxis: {
        gridLineWidth: 1,
        title: {
            text: 'Fold enrichment'
        },
        labels: {
            format: '{value}'
        },
     
        plotLines: [{
            color: 'black',
            dashStyle: 'dot',
            width: 2,
            value: 80,
            label: {
                rotation: 0,
                y: 15,
                style: {
                    fontStyle: 'italic'
                },
                text: 'Mean of the fold enrichment'
            },
            zIndex: 3
        }]
    },

    yAxis: {
        startOnTick: false,
        endOnTick: false,
        reversed:true,
        tickInterval: 1,
        title: {
            text: 'Fold enrichment'
        },

        labels: {
            format: 'top {value}',
            step:1,
        },
        maxPadding: 0.2,
        
        plotLines: [{
            color: 'black',
            dashStyle: 'dot',
            width: 1,
            value: 0,
            label: {
                align: 'right',
                style: {
                    fontStyle: 'italic'
                },
                //text: 'Safe sugar intake 50g/day',
                x: -10
            },
            zIndex: 3
        }]
    },

    tooltip: {
        useHTML: true,
        headerFormat: '<table>',
        pointFormat: '<tr><th colspan="2"><h3>{point.term}</h3></th></tr>' +
                    '<tr><th> adj.pval:</th><td>{point.pvalue}</td></tr>' +
                    '<tr><th>Fold enrichment:</th><td>{point.x}</td></tr>' +
                    '<tr><th>Top:</th><td>{point.y}</td></tr>' +
                    '<tr><th>Fold enrichment:</th><td>{point.z}%</td></tr>',
        footerFormat: '</table>',
        followPointer: true
    },

    plotOptions: {
        series: {
            dataLabels: {
                enabled: true,
                format: '{point.GO}'
            }
        }
    },
    

  };

  
  /*
  var test = Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    console.log("tst");
    alert(JSON.stringify(message));
    }
  );
  */
  
  
  Shiny.addCustomMessageHandler("updateVariable", function(newData) {
  var newOptions = defaultOptions;
  newOptions.subtitle.text = newData.min;
  newOptions.subtitle.text = newData.max;
  newOptions.series = newData.series; //update the series data 
  newOptions.subtitle.text = newData.legend;
  newOptions.yAxis.title.text = newData.title;
  console.log(newOptions);
  var chartObj = new Highcharts.Chart(newOptions);
  //newOptions.title = "test";
  //chartObj.redraw();
 
  });
  
});
