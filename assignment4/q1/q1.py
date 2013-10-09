#!/bin/bash/python


from bs4 import BeautifulSoup
import urllib2
 
redditFile = urllib2.urlopen("http://stanzh.com")
redditHtml = redditFile.read()
redditFile.close()
 #downloads all the reference link
soup = BeautifulSoup(redditHtml)
redditAll = soup.find_all("a")
for links in soup.find_all('a'):
    print (links.get('href'))


fn = open("../100url.list","r")
for i, line in enumerate(fn):
	print i
	f = open("../100urls/%i.data" %i,'w')
	
	#content =  bleach.clean(o.read())

	f.write(content.encode('utf-8'))
	f.close()


