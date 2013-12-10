
#convert into clean urls
import bs4
import pycurl
import StringIO
import bleach
#import encode
import html2text
#from HTMLParser import HTMLParser



fn = open("sources.txt","r")
for i, line in enumerate(fn):
	print i
	o = open("../urldump/rawHtml%i.data" %i,'r')
	f = open("bodyHtml%i.data" %i,'w')
	#u = unicode(b.getvalue())
	
	content =  bleach.clean(o.read())

	f.write(content.encode('utf-8'))
	f.close()


testing = ""


