#!/usr/bin/env python
# -*- coding: utf-8 -*-
import feedparser
import codecs
import unittest



d = feedparser.parse('http://www.reddit.com/r/funny/.rss?limit=100')
title = open('title.txt', 'w')
links = open('links.txt', 'w')



for post in d.entries:
    title.write((post.title+ "\n").encode(encoding='UTF-8',errors='strict'))
    links.write((post.link + "\n").encode(encoding='UTF-8',errors='strict'))
    #print (post.title + ", " + post.link + "\n").encode(encoding='UTF-8',errors='strict')
    # Do something with content