

import pycurl
import StringIO
import html2text
from HTMLParser import HTMLParser

class MLStripper(HTMLParser):
    def __init__(self):
        self.reset()
        self.fed = []
    def handle_data(self, d):
        self.fed.append(d)
    def get_data(self):
        return ''.join(self.fed)

def strip_tags(html):
    s = MLStripper()
    s.feed(html)
    return s.get_data()


fn = open("sample.txt","r")
for i, line in enumerate(fn):
	f = open("sample%i.data" %i,'w')
	c = pycurl.Curl()
	c.setopt(pycurl.URL, line)
	c.setopt(pycurl.HTTPHEADER, ["Accept:"])
	b = StringIO.StringIO()
	c.setopt(pycurl.WRITEFUNCTION, b.write)
	c.setopt(pycurl.FOLLOWLOCATION, 1)
	c.setopt(pycurl.MAXREDIRS, 5)
	c.perform()
	content = b.getvalue()
	#u = unicode(b.getvalue())
	content =  strip_tags(content)
	f.write(content)
	f.close()
