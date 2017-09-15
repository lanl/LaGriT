import re, urllib

textfile = file('/Users/nknapp/Desktop/website_map.txt','wt')
print 'Usage  - "http://phocks.org/stumble/creepy/" <-- With the double quotes'
myurl = "https://lanl.github.io/LaGriT" 
for i in re.findall('''href=["'](.[^"']+)["']''', urllib.urlopen(myurl).read(), re.I):
        print i  
        for ee in re.findall('''href=["'](.[^"']+)["']''', urllib.urlopen(i).read(), re.I):
                print ee
                textfile.write(ee+'\n')
textfile.close()
