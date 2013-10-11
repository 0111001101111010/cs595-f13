#!/bin/bash/python


from bs4 import BeautifulSoup
import urllib2

fn = open("100url.list","r")
for i, line in enumerate(fn):
	print i
	f = open("100urls/%i.data" %i,'w')
	hdr = { 'User-Agent' : 'Firefox 25.0' }
	req = urllib2.Request(line, headers=hdr)
	redditFile = urllib2.urlopen(req)
	redditHtml = redditFile.read()
	redditFile.close()
	 #downloads all the reference link
	soup = BeautifulSoup(redditHtml)
	redditAll = soup.find_all("a")
	for links in soup.find_all('a'):
		content = links.get('href')
		print(content)
		f.write(content+"\n")
	    	#f.write("\n")
			#f.close()	
	#content =  bleach.clean(o.read())

	#f.write(content)



