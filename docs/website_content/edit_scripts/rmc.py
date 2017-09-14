import os

rm_dr = '/home/nknapp/deknapp.github.io/pages/docs/'
good_dr = '/home/nknapp/deknapp.github.io/pages/docs/commands/'

listA = os.listdir(rm_dr)
listB = os.listdir(good_dr)

for fle in listA:
    if fle in listB:
        os.system('rm ' + rm_dr + fle)
        print 'removed ', fle
