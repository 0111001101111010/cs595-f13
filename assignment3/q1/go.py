import pycurl
import StringIO

fn = open("sample.txt","r")
for i, line in enumerate(fn):
	f = open("output%i.data" %i,'w')
	c = pycurl.Curl()
	c.setopt(pycurl.URL, line)
	c.setopt(pycurl.HTTPHEADER, ["Accept:"])
	b = StringIO.StringIO()
	c.setopt(pycurl.WRITEFUNCTION, b.write)
	c.setopt(pycurl.FOLLOWLOCATION, 1)
	c.setopt(pycurl.MAXREDIRS, 5)
	c.perform()
	f.write(b.getvalue())
	f.close()
