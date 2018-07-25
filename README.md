# MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics

## Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Contact](#contact)
- [Acknowledgements](#acknowledgements)


## Introduction

This project initiate by Yannick Lippi aims to facilitate access to biologist in order to publish graphs such as heatmap, PCA or Venn diagram related to specifics data produced by TRiX's team.
MATRiX is an application dedicated to  DNA chip analysis, this application incorporates quality control with Principal components analysis to summarize microarray and differential analysis with various methods such as Venn diagram, Heatmap clustering and GO Enrichment analysis by querrying the DWS (DAVID WEB SERVICES).

MATRiX app is working with specific data produced by the limma package [name](citation), resulting p-values are adjusted according to the Benjamini and Hochberg procedure [Benjamini and Hochberg 1995].
PCA is computed with the FactoMineR package and the plot is produced with the factoextra package, for the Heatmap and Venn diagram the graphs are obtained respectively with the gplots and VennDiagram package, those packages are available on CRAN
This application works only with specific data produced by the plateau TRiX, you can check the example file (MA_Trix_App/downloadData.zip)
Here's the global workflow passing by the experiment to the visualization.
TODO

## Installation

MATRiX is available for R>3.5.0. The installation, download and execution can all be performed with a small R script :
```
## Download RDAVIDWebService
source("https://bioconductor.org/biocLite.R")
biocLite("RDAVIDWebService")

## Load RDAVIDWebService 
library(RDAVIDWebService)

## Load shiny packages
if(!require('shiny')){
 install.packages('shiny')
 library(shiny)
}

# Install dependencies, download last version of MATRiX from github and run matrix in one command :
runGitHub('fsoubes/MA_Trix_App')
```
You can also run the app as following : 
```
git clone https://github.com/fsoubes/MA_Trix_App
chmod +x ./cmd.sh
./cmd.sh
```

## Contact

Here are our mail [Franck Soubès] (franck.soubes@inra.fr) or [Yannick Lippi] (yannick.lippi@inra.fr) for any questions, suggestions or if you need help to use MATRiX, dont hesitate to contact us.

## Acknowledgements

The main contributors to MATRiX:

Yannick Lippi, (Initiator, beta-testing, feature suggestions),

Franck Soubès, (Coding, Unit testing, documentation, packaging, feature suggestions),

TOXALIM Team: BioToMyc & TIM, (beta-testing, feature suggestions)  

Claire Neyles, (suggestions)
