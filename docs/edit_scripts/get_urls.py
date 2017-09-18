
# use on a website

import urllib2
from bs4 import BeautifulSoup
url = 'http://www.google.co.in/'

conn = urllib2.urlopen(url)
html = conn.read()

soup = BeautifulSoup(html)
links = soup.find_all('a')

for tag in links:
    link = tag.get('href',None)
    if link is not None:
        print link