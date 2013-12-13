#!/usr/bin/python
# -*- coding: utf-8 -*-

import unittest
import feedparser
import docclass
import codecs

cl=docclass.classifier(docclass.getwords)

cl.setdb('testdb.db')

f = open('title.txt')
lines = f.readlines()
f.close()

#for entry in lines:
#    print entry


#for post in d.entries:
    #title.write((post.title+ "\n").encode(encoding='UTF-8',errors='strict'))
    #links.write((post.link + "\n").encode(encoding='UTF-8',errors='strict'))
    #print (post.title + ", " + post.link + "\n").encode(encoding='UTF-8',errors='strict')
    # Do something with content
'''
Meme - any sort of captioned text with an iconic picture. 
Gif - animated content, can be original or edited content
Animal - anything related to animals - can span media categories
original picture - undoctored pictures taken of content excluding animals 
edited picture - doctored content by photo editing software excluding animals
media picture - content from tv, magazine, media, movies 
'''



#cl.train(lines[0], 'original')

cl.train(lines[0], 'animal')
cl.train(lines[1], 'media')
cl.train(lines[2], 'gif')
cl.train(lines[3], 'original')
cl.train(lines[4], 'media')
cl.train(lines[5], 'original')
cl.train(lines[6], 'meme')
cl.train(lines[7], 'original')
cl.train(lines[8], 'original')
cl.train(lines[9], 'animal')
cl.train(lines[10], 'media')
cl.train(lines[11], 'animal')
cl.train(lines[12], 'animal')
cl.train(lines[13], 'edited')
cl.train(lines[14], 'original')
cl.train(lines[15], 'animal')
cl.train(lines[16], 'meme')
cl.train(lines[17], 'meme')
cl.train(lines[18], 'original')
cl.train(lines[19], 'original')
cl.train(lines[20], 'edited')
cl.train(lines[21], 'original')
cl.train(lines[22], 'original')
cl.train(lines[23], 'animal')
cl.train(lines[24], 'original')
cl.train(lines[25], 'original')
cl.train(lines[26], 'edited')
cl.train(lines[27], 'original')
cl.train(lines[28], 'original')
cl.train(lines[29], 'meme')
cl.train(lines[30], 'gif')
cl.train(lines[31], 'animal')
cl.train(lines[32], 'edited')
cl.train(lines[33], 'meme')
cl.train(lines[34], 'gif')
cl.train(lines[35], 'meme')
cl.train(lines[36], 'original')
cl.train(lines[37], 'original')
cl.train(lines[38], 'original')
cl.train(lines[39], 'meme')
cl.train(lines[40], 'meme')
cl.train(lines[41], 'original')
cl.train(lines[42], 'meme')
cl.train(lines[43], 'gif')
cl.train(lines[44], 'media')
cl.train(lines[45], 'gif')
cl.train(lines[46], 'comic')
cl.train(lines[47], 'original')
cl.train(lines[48], 'gif')
cl.train(lines[49], 'gif')


cl = docclass.fisherclassifier(docclass.getwords)

for i in range(50, 100):
   category = cl.classify(lines[i])
   title = (lines[i][:30] + '..') if len(lines[i]) > 75 else lines[i]
   #title = (lines[i]).encode('utf-8')
   print (category).encode('utf-8') #lines[i] + ' : ' +  
   #print cl.cprob(title, category)   
    



