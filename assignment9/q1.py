#!/usr/bin/env python
# -*- coding: utf-8 -*- 
#http://www.pythonforbeginners.com/python-on-the-web/using-feedparser-in-python/
import feedparser 
f = open('feeds.txt', 'w')
e = feedparser.parse('http://www.rssreader.com/englishfeeds.xml')
d = feedparser.parse('http://www.rssreader.com/dutchfeeds.xml')


#the two feeds we need
required = "http://f-measure.blogspot.com/"+"\n"
required2 = "http://ws-dl.blogspot.com/"+"\n"
f.write(required)
f.write(required2)



#english links
for post in e.entries:
     f.write((post.link + "\n").encode('utf-8'))


#deutch links
for post in d.entries:
    f.write((post.link + "\n").encode('utf-8'))
