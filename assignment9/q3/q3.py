#code from power point

import clusters

blognames,words,data=clusters.readfile('../PCI_Code_Folder/chapter3/blogdata.txt') 
#clust=clusters.hcluster(data) 

#clusters.printclust(clust,labels=blognames) 

kclust=clusters.kcluster(data,k=5)  
print "break for 5\n"
kclust=clusters.kcluster(data,k=10)  
print "break for 10\n"

kclust=clusters.kcluster(data,k=20) 
print "break for 20\n"


#clusters.drawdendrogram(kclust,blognames,jpeg='blogclust.jpg') 

