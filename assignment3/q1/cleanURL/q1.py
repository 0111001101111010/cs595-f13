import bs4
import pycurl
import StringIO
import html2text
from HTMLParser import HTMLParser
fn = open("sample.txt","r")
for i, line in enumerate(fn):
	try:
		print i
		f = open("../urldump/rawHtml%i.data" %i,'r')
		f = open("bodyHtml%i.data" %i,'w')
		content = b.getvalue()
		#u = unicode(b.getvalue())
		content =  bs4.BeautifulSoup(content)
		f.write(content.text)
		f.close()
	except:
		pass