from bs4 import BeautifulSoup, NavigableString


html = "<p>Good, <b>bad</b>, and <i>ug<b>l</b><u>y</u></i></p>"
invalid_tags = ['b', 'i', 'u','p']
soup = BeautifulSoup(html)
for tag in invalid_tags: 
    for match in soup.findAll(tag):
        match.replaceWithChildren()
print soup
