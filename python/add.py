import sys
import csv
import requests
import os.path
import webbrowser


def add(x,y):
    return x+y

'''
For a listing genes, display a web page with the enrichment of theses genes. It is possible in the web page to select the right organism.
'''

def enrichmentdav(outputlist):
    
    url = "http://david.abcc.ncifcrf.gov/api.jsp?type=OFFICIAL_GENE_SYMBOL&ids="
    for gene in outputlist:
        url += gene + ","
    url += "&tool=summary&annot=GOTERM_BP_ALL,GOTERM_CC_ALL,GOTERM_MF_ALL,"
    webbrowser.open(url)

