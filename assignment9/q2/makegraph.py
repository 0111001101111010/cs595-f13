#code from power point


import clusters
blognames,words,data=clusters.readfile('../PCI_Code_Folder/chapter3/blogdata.txt') 
clust=clusters.hcluster(data) 

clusters.printclust(clust,labels=blognames) 
clusters.drawdendrogram(clust,blognames,jpeg='blogclust.jpg') 

