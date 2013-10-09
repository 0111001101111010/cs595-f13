#!/bin/bash

directory="urldump"
directory=`ls $directory`
for page in $mypages
do
	newfilename="cleanURL/clean.${page}"
	lynx -dump -core -force_html -verbose $directory/$page > $newfilename
done
