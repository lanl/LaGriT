import os

dr = '/home/nknapp/deknapp.github.io/not_on_website/'

for fle in os.listdir(dr):
    if 'another_' in fle:
        os.system('mv ' + dr + fle + ' ' + dr + fle[9:])
