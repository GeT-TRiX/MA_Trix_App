/*
Author: Franck Soubès
*/

$(document).ready(function () {

var xTitle = "top genes enriched";

var defaultOptions = {
  
  
    series: null,
    chart: {
	  zoomType: "xy",
	  //width: 1100,
	  //height: 600,
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
        text: 'Source: <a href="http://www.euromonitor.com/">ok</a> and <a href="https://data.oecd.org/">OECD</a>'
    },

    xAxis: {
        gridLineWidth: 1,
        title: {
            text: 'Fold enrichment'
        },
        labels: {
            format: '{value}'
        }
      /*
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
        */
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
                x: -10
            },
            zIndex: 3
        }]
    },

    tooltip: {
        //valueDecimals: 2,
        //yDecimals: 0,
        //xDecimals: 1,
        useHTML: true,
        headerFormat: '<table>',
        pointFormat: '<tr><th colspan="2"><h3>{point.term}</h3></th></tr>' +
                    '<tr><th> adj.pval:</th><td>{point.pvalue}</td></tr>' +
                    '<tr><th>Fold enrichment:</th><td>{point.x:,.1f}</td></tr>' +
                    '<tr><th>Top:</th><td>{point.y:,.0f}</td></tr>' +
                    '<tr><th>Percentage of hits:</th><td>{point.z:,.2f}%</td></tr>',
        footerFormat: '</table>',
        followPointer: true
    },

    plotOptions: {
        series: {
            stickyTracking: false,
            dataLabels: {
                enabled: true,
                format: '{point.GO}',
                color: 'black'
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
  
  function updatechart() {
  
  Shiny.addCustomMessageHandler("updateVariable", function(newData) {
  
  var newOptions = defaultOptions;
  newOptions.subtitle.text = newData.min;
  newOptions.subtitle.text = newData.max;
  newOptions.series = newData.series; //update the series data 
  newOptions.subtitle.text = newData.legend;
  newOptions.yAxis.title.text = newData.title;

  Shiny.addCustomMessageHandler("handler1", function (message){
  newOptions.plotOptions.series.dataLabels.enabled = message;
  var chartObj = new Highcharts.Chart(newOptions);
  
      });
  
    });
  }
    var printUpdate = function () {
        $('#container').highcharts().reflow();
    };

    if (window.matchMedia) {
        var mediaQueryList = window.matchMedia('print');
        mediaQueryList.addListener(function (mql) {
            printUpdate();
        });
      
    }

    
    
updatechart();

});
  
