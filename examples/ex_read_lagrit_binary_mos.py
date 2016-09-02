################################################################################
# This example shows how to merge multiple mesh objects together.              #
################################################################################

#To use pylagrit, import the module.
import pylagrit
import numpy

#Instantiate the lagrit object.
lg = pylagrit.PyLaGriT()

# Create list with mesh object as first element
dxyz = numpy.array([0.25]*3)
mins = numpy.array([0.]*3)
maxs = numpy.array([1.]*3)
ms = [lg.createpts_dxyz(dxyz,mins,maxs,'tet',connect=True,name='testmo')]

# Create three new mesh objects, each one directly above the other
for i in range(3):
    ms.append(ms[-1].copy())
    ms[-1].trans(ms[-1].mins,ms[-1].mins+numpy.array([0.,0.,1.]))

lg.dump('lagrit_binary.lg')
lg.close()

lg = pylagrit.PyLaGriT()
ms_read = lg.read('lagrit_binary.lg')

print 'Name of mesh object read in should be testmo, is: ', ms_read.name



