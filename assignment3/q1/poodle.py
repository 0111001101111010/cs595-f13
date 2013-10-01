import subprocess



f = open('sample.txt', 'r')
#t = open('curled', 'w')
l = f.read().splitlines()


for each in l:
	try:
		proc = subprocess.check_output(["curl ","--output", "dump-header", "http://www.cnn.com/", ">", "www.cnn.com"], stdout=subprocess.PIPE)
		
	except: 
		pass