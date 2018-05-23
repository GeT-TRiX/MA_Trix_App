import sys
import csv
import requests
import os.path
import webbrowser

'''
For a listing genes, display a web page with the enrichment of theses genes. It is possible in the web page to select the right organism.
'''

outputList=["D130043K22Rik","Fads2","Mif,NAP062858-1","Tead2","Fam82a1","Cyp4a13","Mgst3","Slc50a1","Nipal1","Cyp2b10","Mal","Dhrs7","Timp3","Esyt2","Aldob","Ppia","Spc25","Ak2","Rpl10","Gm5623","Fabp2","A_55_P2128224","Lrrn3","Ugp2","Htra2","Atp5g2","Fam13a"]


def david_query(outputlist):
    url = "http://david.abcc.ncifcrf.gov/api.jsp?type=OFFICIAL_GENE_SYMBOL&ids="
    for gene in outputList:
        url += gene + ","
        url += "&tool=summary&annot=GOTERM_BP_ALL,GOTERM_CC_ALL,GOTERM_MF_ALL,"
    webbrowser.open(url)
