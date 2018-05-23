library(reticulate)
os <- import("os")
os$listdir(".")
use_python("/usr/local/bin/python")
use_virtualenv("myenv")
source_python('/python/add.py')
test = list("D130043K22Rik","Fads2","Mif,NAP062858-1","Tead2","Fam82a1","Cyp4a13","Mgst3","Slc50a1")
typeof(test)
class(test)


enrichmentdav(test)

