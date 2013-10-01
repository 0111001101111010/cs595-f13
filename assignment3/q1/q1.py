#!/usr/bin/python 
# -*- coding: utf-8 -*-
import subprocess
import requests
import pycurl
from StringIO import StringIO 

'''
storage = StringIO()
c = pycurl.Curl()
c.setopt(c.URL, url)
c.setopt(c.WRITEFUNCTION, storage.write)
c.perform()
c.close()
content = storage.getvalue()
print content
'''

f = open('sample.txt', 'r')
#t = open('curled', 'w')
l = f.read().splitlines()
#print l

storage = StringIO()
c = pycurl.Curl()
c.setopt(c.URL, l[0])
c.setopt(c.WRITEFUNCTION, storage.write)
c.perform()
c.close()
content = storage.getvalue()
print content
c.setopt(content, storage.write)
'''
for each in l:
  	try:
		r= requests.get(each)
		if (r.status_code==200):
			print r.url 
			t.write(r.url+"\n")
	except:
		pass

t.close()

	#shell_command = 'curl" -I -L %s"'%(each)
	#subp = subprocess.Popen(['curl',  '-L', '-I', each],stdout=subprocess.PIPE, stderr=subprocess.PIPE)
	#curlstdout, curlstderr = subp.communicate()

	#op = str(curlstdout)
	#print op

	#event = Popen(shell_command, shell=True, stdin=PIPE, stdout=PIPE, 
    #stderr=STDOUT)
	#output = event.communicate()
	#print output + "\n"
	#print "I can program in " + each
'''