# MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
[Franck Soubès] (franck.soubes@inra.fr) [Yannick Lippi] (yannick.lippi@inra.fr)

## Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Contact](#contact)
- [Acknowledgements](#acknowledgements)


## Introduction

This project was initiate by Yannick Lippi aims to facilitate access to biologist in the aim of publishing graphs (heatmap, PCA, Venn diamgram) related to specifics data produced by microarray.
MATRiX is an application dedicated to  DNA chip analysis, this application incorporates quality control with Principal components analysis to summarize microarray and differential analysis with various methods such as Venn diagram, Heatmap clustering and GO Enrichment analysis by querrying the DWS (DAVID WEB SERVICES)
MATRiX app is working with specific data produced by the limma package [name](citation), resulting p-values are adjusted according to the Benjamini and Hochberg procedure [Benjamini and Hochberg 1995].
PCA is computed with the FactoMineR package and the plot is produced with the factoextra package, for the Heatmap and Venn diagram the graphs are obtained respectively with the gplots and VennDiagram package, those packages are available on CRAN
This application works only with specific data produced by the plateau TRiX, you can check the example file (MA_Trix_App/downloadData.zip)
Here's the global workflow passing by the experiment to the visualization.
TODO

## Installation

MATRiX is available for R>3.5.0. The installation, download and execution can all be performed with a small R script :
```
# Load shiny packages
if(!require('shiny')){
 install.packages('shiny')
 library(shiny)
}

# Install dependencies, download last version of SHAMAN from github and run shaman in one command :
runGitHub('fsoubes/MA_Trix_App')
```
This script can also be executed as following : 
```
git clone https://github.com/fsoubes/MA_Trix_App
chmod +x ./cmd.sh
./cmd.sh


## Contact

If you have any comments, questions or suggestions, or need help to use MATRiX, please contact me [here](franck.soubes@inra.fr).

## Acknowledgements

The main contributors to MATRiX:
Yannick Lippi, (Initiator, beta-testing, feature suggestions),
Franck Soubès, (Coding, Unit testing, documentation, packaging, feature suggestions),
TOXALIM Team: BioToMyc & TIM, (beta-testing, feature suggestions)  
Claire Neyles, (suggestions)


