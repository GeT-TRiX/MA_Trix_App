library(reticulate)
os <- import("os")
os$listdir(".")
use_python("/usr/local/bin/python")
use_virtualenv("myenv")
source_python('./python/add.py')
test = list("D130043K22Rik","Fads2","Mif,NAP062858-1","Tead2","Fam82a1","Cyp4a13","Mgst3","Slc50a1","Nipal1","Cyp2b10","Mal","Dhrs7","Timp3","Esyt2","Aldob","Ppia","Spc25","Ak2","Rpl10","Gm5623","Fabp2","A_55_P2128224","Lrrn3","Ugp2","Htra2","Atp5g2","Fam13a")
enrichmentdav(test)

#outputList=["D130043K22Rik","Fads2","Mif,NAP062858-1","Tead2","Fam82a1","Cyp4a13","Mgst3","Slc50a1","Nipal1","Cyp2b10","Mal","Dhrs7","Timp3","Esyt2","Aldob","Ppia","Spc25","Ak2","Rpl10","Gm5623","Fabp2","A_55_P2128224","Lrrn3","Ugp2","Htra2","Atp5g2","Fam13a"]




