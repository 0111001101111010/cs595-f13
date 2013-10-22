import sys
import xml.dom.minidom 
from bs4 import BeautifulSoup
# -*- coding: utf-8 -*-

#f = open("../Stanley Zheng_1382295671.graphml").read()
#pass in ^ file 
g=open("friends.csv",'w')


def parseLog(file):
    file = sys.argv[1]
    handler = open(file).read()
    soup = BeautifulSoup(handler)
    f.write(str("name") + ',' + str("friends") + '\n')
    #as of 10/21/13
    f.write(str("Stanley Zheng") + ',' + str("342") + '\n')
    for message in soup.findAll('node'):
        msg_attrs = dict(message.attrs)
        f_user = message.find('Label').data
        f_user_dict = dict(f_user.attrs)
        print "%s, %s \n" % (f_user_dict[u'Label'], message.find('friend_count')())

if __name__ == "__main__":
    parseLog(sys.argv[1])
