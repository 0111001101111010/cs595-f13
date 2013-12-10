#!/usr/bin/env python
# -*- coding: utf-8 -*- 
#http://www.pythonforbeginners.com/python-on-the-web/using-feedparser-in-python/
import feedparser
import bleach 

f = open('feeds.txt', 'w')
e = feedparser.parse('http://www.rssreader.com/englishfeeds.xml')
d = feedparser.parse('http://www.rssreader.com/dutchfeeds.xml')

feedList = []

#the two feeds we need
required = "http://f-measure.blogspot.com/feeds/posts/default"+"\n"
required2 = "http://ws-dl.blogspot.com/feeds/posts/default"+"\n"
f.write(required)
f.write(required2)
feedList.append(required)
feedList.append(required2)




#english links
for post in e.entries:
     f.write((post.link + "\n").encode('utf-8'))
     feedList.append(post.link)


#deutch links
for post in d.entries:
    f.write((post.link + "\n").encode('utf-8'))
    feedList.append(post.link)


#openRSSfeeds and parse them
for item in feedList:
        print item