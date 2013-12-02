#!/usr/bin/env python
# -*- coding: utf-8 -*- 
#http://www.pythonforbeginners.com/python-on-the-web/using-feedparser-in-python/
import feedparser 

d = feedparser.parse('http://www.reddit.com/r/python/.rss')

for post in d.entries:
    print (post.title + ": " + post.link + "\n").encode('utf-8')