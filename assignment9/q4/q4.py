#code from power point slide 29

import clusters

blognames,words,data=clusters.readfile('../PCI_Code_Folder/chapter3/blogdata.txt') 
#clust=clusters.hcluster(data) 

#clusters.printclust(clust,labels=blognames) 

coords=clusters.scaledown(data)

clusters.draw2d(coords,blognames,jpeg='blogs2d.jpg') 


