# MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data.

## Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Contact](#contact)
- [Acknowledgements](#acknowledgements)


## Introduction

This project initiate by Yannick Lippi aims to facilitate access to biologists in order to publish graphs such as heatmap, PCA or Venn diagram related to transcriptomic data.
MATRiX is an application dedicated to  DNA chip, RNA-seq and  ChIP-Seq analysis, this application incorporates quality control with Principal components analysis helping to summarize the data  and differential analysis with various methods such as Venn diagram, Heatmap clustering and GO Enrichment analysis by querying the DWS (DAVID WEB SERVICES).

MATRiX app is working with specific data produced by the limma, DESeq2, edgeR packages, resulting p-values are adjusted according to the Benjamini and Hochberg procedure [Benjamini and Hochberg 1995].
PCA is computed with the FactoMineR package and the plot is produced with the factoextra package, for the Heatmap and Venn diagram the graphs are obtained respectively with the gplots and VennDiagram package, those packages are available on CRAN
This application works only with specific data, you can check the example file (MA_Trix_App/downloadData.zip)

Here's the global workflow passing by the experiment to the visualization:

![](./www/whatmaen.png)

and also a [video Presentation](https://www.youtube.com/watch?v=lfI0zRYzeJs)

## Installation

MATRiX is available for R>3.5.0. The installation, download and execution can all be performed with a small R script :
First you'll need to install RJava in the aim to using RDAVIDWebService.
You can install it using the following commands:
```
sudo apt-get install default-jdk
sudo R CMD javareconf #to associate it with R
sudo apt-get install r-cran-rjava
sudo apt-get install libgdal1-dev libproj-dev
sudo apt-get install libv8-3.14-dev
```
```
## Install RDAVIDWebService into R
source("https://bioconductor.org/biocLite.R")
biocLite("RDAVIDWebService")
install.packages("rJava")

## Load RDAVIDWebService
library(RDAVIDWebService)

## Load shiny packages
if(!require('shiny')){
 install.packages('shiny')
 library(shiny)
}

# Install dependencies, download last version of MATRiX from github and run matrix in one command :
runGitHub('GeT-TRiX/MA_Trix_App')
```
If RDAVIDWebService and Shiny are installed on your machine can also run the app as following :
```
git clone https://github.com/GeT-TRiX/MA_Trix_App
chmod +x ./cmd.sh
./cmd.sh
```

## Contact

Here are our mail [Franck Soubès] (franck.soubes@inra.fr) or [Yannick Lippi] (yannick.lippi@inra.fr) for any questions, suggestions or if you need help to use MATRiX, dont hesitate to contact us.

## Acknowledgements

The main contributors to MATRiX:

Yannick Lippi, (Initiator, beta-testing, feature suggestions),

Franck Soubès, (Coding, Unit testing, documentation, packaging, feature suggestions),

Didier Laborie, (Virtual machine and DNS),

TOXALIM Team: BioToMyc & TIM, (beta-testing, feature suggestions)  

Claire Naylies, (suggestions)
