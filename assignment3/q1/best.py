import subprocess



f = open('sample.txt', 'r')
#t = open('curled', 'w')
l = f.read().splitlines()


for each in l:
	try:
		proc = subprocess.Popen(["curl", each,">", each[0:6]], stdout=subprocess.PIPE)
		(out, err) = proc.communicate()
		print out
	except: 
		pass