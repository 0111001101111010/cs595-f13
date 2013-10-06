#!/bin/bash 
grep -o -i learning ../cleanURL/* | uniq -c | sort -r
